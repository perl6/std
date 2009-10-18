sub jseval is export {}

sub say is export { jseval 'say(this.invoker.invoker.arg_array.join(""))' }

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

#multi sub infix:<+>(Int $a, Int $b) { jseval '
#    this.result = new c.Int(c["$a"].v.add(c["$b"]))
#';
#}
#1;