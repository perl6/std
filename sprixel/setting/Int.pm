
sub say {
    jseval '
say("PK FIRE");
';
}

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
    return o;
}';
}

say 4,5

#multi sub infix:<+>(Int $a, Int $b) { jseval '
#    this.result = new c.Int(c["$a"].v.add(c["$b"]))
#';
#}
#1;