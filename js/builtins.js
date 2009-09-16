var p6builtin = {}; (function(){

p6builtin.Integer = function(integer) {
    this.v = integer;
};
p6builtin.Integer.prototype = {
toString: function(){
    return this.v.toString();
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
    return new p6builtin.Integer(1);
}
var p6toplevel={
    'say': new p6builtin.p6sub(say,'say')
};








