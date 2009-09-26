var p6builtin = {}; (function(){

var bigInt = libBigInt;

// immutable
p6builtin.Int = function(integer,radix) {
    if (typeof(integer)=='string') {
        this.v = bigInt.nbi();
        this.v.fromString(integer,+(radix || 10));
    } else {
        this.v = integer instanceof bigInt
            ? integer
            : bigInt.nbv(integer);
    }
};
p6builtin.Int.prototype = {
toString: function(){
    return this.v.toString();
},
toBool: function(){
    return this.v.signum() != 0;
},
succ: function(){
    return new p6builtin.Int(this.v.add(bigInt.ONE));
},
pred: function(){
    return new p6builtin.Int(this.v.subtract(bigInt.ONE));
},
do_Additive:function(right, subtract){
    var left = this;
    if (left instanceof p6builtin.p6var) left = left.value; // deref
    if (right instanceof p6builtin.p6var) right = right.value; // deref
    left = left instanceof p6builtin.Int
        ? left // TODO: use the proper coercion
        : new p6builtin.Int(Number(this.value.toString()));
    right = right instanceof p6builtin.Int
        ? right // TODO: use the proper coercion
        : new p6builtin.Int(Number(right));
    return new p6builtin.Int(subtract ? left.v.subtract(right.v) : left.v.add(right.v));
},
do_Multiplicative:function(right, divide){
    var left = this;
    if (left instanceof p6builtin.p6var) left = left.value; // deref
    if (right instanceof p6builtin.p6var) right = right.value; // deref
    left = left instanceof p6builtin.Int
        ? left // TODO: use the proper coercion
        : new p6builtin.Int(Number(this.value.toString()));
    right = right instanceof p6builtin.Int
        ? right // TODO: use the proper coercion
        : new p6builtin.Int(Number(right));
    switch(divide || 0) {
    case 3:
        return new p6builtin.Int(left.v.shiftLeft(right.v));
    case 4:
        return new p6builtin.Int(left.v.shiftRight(right.v));
    case 1:
    case 2:
        var q = bigInt.nbi(), r = bigInt.nbi();
        left.v.divRemTo(right.v,q,r);
        return new p6builtin.Int(divide == 1 ? q : r);
    default:
        return new p6builtin.Int(left.v.multiply(right.v));
    }
},
do_infix__S_Lt:function(right){
    return new p6builtin.Bool(this.v.compareTo(right.v) < 0);
},
do_infix__S_LtEqual:function(right){
    return new p6builtin.Bool(this.v.compareTo(right.v) <= 0);
},
do_infix__S_Gt:function(right){
    return new p6builtin.Bool(this.v.compareTo(right.v) > 0);
},
do_infix__S_GtEqual:function(right){
    return new p6builtin.Bool(this.v.compareTo(right.v) >= 0);
},
do_infix__S_EqualEqual:function(right){
    return new p6builtin.Bool(this.v.compareTo(right.v) == 0);
},
do_infix__S_BangEqual:function(right){
    return new p6builtin.Bool(this.v.compareTo(right.v) != 0);
}
};
  
p6builtin.Bool = function(bool) {
    this.v = typeof(bool)=='boolean' ? bool : !!bool;
};
p6builtin.Bool.prototype = {
toString: function(){
    return this.v ? '1' : '0';
},
toBool:function(){
    return this.v;
}
};

p6builtin.Str = function(str) {
    this.v = typeof(str)=='string' ? str : str.toString();
};
p6builtin.Str.prototype = {
toString: function(){
    return this.v;
},
toBool:function(){
    return this.v.length != 0 && this.v != '0'
},
do_infix__S_lt:function(right){
    return new p6builtin.Bool(this.v < right.v);
},
do_infix__S_le:function(right){
    return new p6builtin.Bool(this.v <= right.v);
},
do_infix__S_gt:function(right){
    return new p6builtin.Bool(this.v > right.v);
},
do_infix__S_ge:function(right){
    return new p6builtin.Bool(this.v >= right.v);
},
do_infix__S_eq:function(right){
    return new p6builtin.Bool(this.v == right.v);
},
do_infix__S_ne:function(right){
    return new p6builtin.Bool(this.v != right.v);
}
};

p6builtin.Undef = function(){},
p6builtin.Undef.prototype = {
toString: function(){
    return 'Undef';
},
toBool:function(){
    return false;
}
};

p6builtin.Nil = function(){},
p6builtin.Nil.prototype = {
toString: function(){
    return 'Nil';
},
toBool:function(){
    return false;
}
};

p6builtin.jssub = function(func,name,source){
    this.func = func;
    this.name = name;
    this.source = func.toString();
};
p6builtin.jssub.prototype = {
toString:function(){
    return this.source.toString();
},
toBool:function(){
    return true;
}
};

p6builtin.p6sub = function(sub_body, declaration_context, arg_slots){
    this.sub_body = sub_body;
    this.arg_slots = arg_slots;
    this.declaration_context = declaration_context; // parent for closure
    this.T = 'p6sub_invocation';
};
p6builtin.p6sub.prototype = {
toString:function(){
    return this.sub_body.BEG;
},
toBool:function(){
    return true;
}
};

p6builtin.p6var = function(sigil,name,context,forceDeclare){
// essentially an autovivifying "slot" (STD has prevented undeclared uses!)
    this.sigil = sigil;
    this.name = name;
    this.context = context;
    var a;
    // either create or lookup. :)  Inefficient, I know.
    if (forceDeclare
            || typeof(a = this.context[this.sigil+this.name])=='undefined') {
        this.context[this.sigil+this.name] = this;
        this.value = null;
    } else {
        return a;
    }
};
p6builtin.p6var.prototype = {
set:function(value){
    this.value = value;
    return this;
},
toString:function(){
    return this.value.toString();
},
increment:function(){
    this.value = this.value.succ();
    return this;
},
decrement:function(){
    this.value = this.value.pred();
    return this;
},
do_Additive:function(right, subtract){
    return p6builtin.Int.prototype.do_Additive.call(
        this.value, right, subtract);
},
do_Multiplicative:function(right, divide){
    return p6builtin.Int.prototype.do_Multiplicative.call(
        this.value, right, divide);
},
toBool:function(){
    return this.v.toBool();
},
do_infix__S_Lt:function(right){
    return this.value.do_infix__S_Lt(right.value ? right.value : right);
},
do_infix__S_LtEqual:function(right){
    return this.value.do_infix__S_LtEqual(right.value ? right.value : right);
},
do_infix__S_Gt:function(right){
    return this.value.do_infix__S_Gt(right.value ? right.value : right);
},
do_infix__S_GtEqual:function(right){
    return this.value.do_infix__S_GtEqual(right.value ? right.value : right);
},
do_infix__S_EqualEqual:function(right){
    return this.value.do_infix__S_EqualEqual(right.value ? right.value : right);
},
do_infix__S_BangEqual:function(right){
    return this.value.do_infix__S_BangEqual(right.value ? right.value : right);
},
do_infix__S_lt:function(right){
    return this.value.do_infix__S_lt(right.value ? right.value : right);
},
do_infix__S_le:function(right){
    return this.value.do_infix__S_le(right.value ? right.value : right);
},
do_infix__S_gt:function(right){
    return this.value.do_infix__S_gt(right.value ? right.value : right);
},
do_infix__S_ge:function(right){
    return this.value.do_infix__S_ge(right.value ? right.value : right);
},
do_infix__S_eq:function(right){
    return this.value.do_infix__S_eq(right.value ? right.value : right);
},
do_infix__S_ne:function(right){
    return this.value.do_infix__S_ne(right.value ? right.value : right);
}
};

p6builtin.p6array = function(items){
    this.items = items;
};
p6builtin.p6array.prototype = {
toString:function(){
    return this.items.join('');
},
toBool:function(){
    return true;
}
};

})();


function say() { // javascript say
    if (typeof(arguments)!='undefined') {
        for (var s_args=[], i=-1, j=-1, a, l=arguments.length; i<l;)
            if (typeof(a=arguments[++i])!='undefined') {
                s_args[++j] = typeof(a)==='string' ? a : a.toString();
            }
        say_them.apply(this.context, s_args);
    } else {
        say_them('');
    }
    this.result = new p6builtin.Int(1);
}

var Scope = (function(){
    function Deriver(){}
    var contextId = 0;
    return function(parentScope){
        if (!parentScope) {
            this.constructor = Scope;
            this.contextId = contextId++;
            return this;
        }
        Deriver.prototype = parentScope;
        var newScope = new Deriver();
        newScope.constructor = Scope;
        newScope.contextId = contextId++;
        return newScope;
    };
})();

var p6toplevel = new Scope();
p6toplevel.say = new p6builtin.jssub(say,'say');
p6toplevel.True = new p6builtin.Bool(true);
p6toplevel.False = new p6builtin.Bool(false);

1;


