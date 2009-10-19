
class Str {
    # install constructor
    jseval '
this.invoker.invoker.invoker.invoker.invoker.class_obj.ctor = function(str) {
    return {
        v : (typeof str == "string" ? str : str.toString()),
        toString : function(){ return this.v }
    };
}';
}

#multi sub infix:<+>(Int $a, Int $b) { jseval '
#    this.result = new c.Int(c["$a"].v.add(c["$b"]))
#';
#}
#1;