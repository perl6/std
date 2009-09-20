var p6builtin = {}; (function(){

// immutable
p6builtin.Int = function(integer) {
    this.v = integer;
};
p6builtin.Int.prototype = {
toString: function(){
    return this.v.toString();
},
succ: function(){
    return new p6builtin.Int(this.v + 1);
},
pred: function(){
    return new p6builtin.Int(this.v - 1);
},
do_Additive:function(right, subtract){
    var left = this;
    if (left instanceof p6builtin.p6var) left = left.value; // deref
    if (right instanceof p6builtin.p6var) right = right.value; // deref
    left = left instanceof p6builtin.Int
        ? left // TODO: use the proper coercion
        : new p6builtin.Int(Number(this.value.toString()))
    right = right instanceof p6builtin.Int
        ? right // TODO: use the proper coercion
        : new p6builtin.Int(Number(right.toString()))
    return new p6builtin.Int(subtract ? left.v - right.v : left.v + right.v);
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

p6builtin.p6sub = function(func,name,source){
    this.func = func;
    this.name = name;
    this.source = func.toString();
};
p6builtin.p6sub.prototype = {
toString:function(){
    return this.source.toString();
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


function say() {
    if (typeof(arguments)!='undefined') {
        for (var s_args=[], i=-1, j=-1, a, l=arguments.length; i<l;)
            if (typeof(a=arguments[++i])!='undefined') {
                s_args[++j] = typeof(a)==='string' ? a : a.toString();
            }
        say_them.apply(this, s_args);
    } else {
        say_them('');
    }
    return new p6builtin.Int(1);
}
var p6toplevel={
    'say': new p6builtin.p6sub(say,'say')
};








