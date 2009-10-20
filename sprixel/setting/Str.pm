
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

multi sub infix:<~>(Str $a, Str $b) { jseval '
    this.result = new ctx.classes.Str.ctor(args[0].v + args[1].v)
';
}

multi sub infix:<x>(Str $a, Int $b) { jseval '
    var a = args[0].v; var b = args[1].v; var c = "";
    for ( var i=0; i<b; i++ ) { c += a; }
    this.result = new ctx.classes.Str.ctor(c)
';
}

