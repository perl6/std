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
