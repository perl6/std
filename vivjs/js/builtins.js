var p6builtin = {}; (function(){

var bigInt = libBigInt;

// immutable
p6builtin.Int = function(integer) {
    if (typeof(integer)=='string') {
        this.v = bigInt.nbi();
        this.v.fromString(integer,10);
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
    
}
};

p6builtin.Str = function(str) {
    this.v = typeof(str)=='string' ? str : str.toString();
};

p6builtin.Str.prototype = {
toString: function(){
    return this.v;
}
};

p6builtin.Undef = {
toString: function(){
    return 'Undef';
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
}
};

p6builtin.p6sub = function(sub_body, declaration_context){
    this.sub_body = sub_body;
    this.context = declaration_context; // parent for closure
    this.T = 'p6sub_invocation';
};
p6builtin.p6sub.prototype = {
toString:function(){
    return this.sub_body.BEG;
}
};

p6builtin.p6var = function(sigil,name,context){
// essentially an autovivifying "slot" (STD has prevented undeclared uses!)
    this.sigil = sigil;
    this.name = name;
    this.context = context;
    var a;
    // either create or lookup. :)  Inefficient, I know.
    if (typeof(a = this.context[this.sigil+this.name])=='undefined') {
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
}
};

p6builtin.p6array = function(items){
    this.items = items;
};
p6builtin.p6array.prototype = {
toString:function(){
    return this.items.join('');
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

1;


