
class Int {
    # install constructor
    jseval '
this.invoker.invoker.invoker.invoker.invoker.class_obj.ctor = function(integer, radix) {
    var o = {};
    if (typeof(integer)=="string") {
        o.v = libBigInt.nbi();
        o.v.fromString(filt__(integer),+(radix || 10));
    } else {
        o.v = integer instanceof libBigInt ? integer : libBigInt.nbv(integer);
    }
    o.toString = function(){ return this.v.toString() };
    return o;
}';
}

multi sub infix:<+> { jseval '
    this.result = new ctx.classes.Int.ctor(args[0].v.add(args[1].v));
';
}

multi sub infix:<-> { jseval '
    this.result = new ctx.classes.Int.ctor(args[0].v.subtract(args[1].v));
';
}
