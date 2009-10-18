var trace_on, trace_off;

var Act = (function(){

var global_trace = false;

trace_on = function(){ global_trace = true };
trace_off = function(){ global_trace = false };

var node_types = {};

// an activation is an object that carries state about a Node, Context pair.
var act_ctor = function(node, invoker){
    this.node = node;
    this.context = (this.invoker = invoker).context;
};

act_ctor.prototype.phase = 0;

act_ctor.prototype.exec = function(){
    var T;
    if (typeof(T = node_types[this.node.T]) != 'undefined') {
        return T.call(this);
    }
    throw 'NYI: '+this.node.T;
};

act_ctor.interpret = function(ast) {
    var act = new act_ctor(ast, { context: new Context({ symbols: {} }) });
    try{
        for (;;(global_trace && say(act.node.T)),(act = act.exec()));
    } catch(e) {
        if (/^NYI:/.test(e)) {
            //delete ast.stabs;
            //say(ToJS(act));
            say(e);
        } else if ('program_done'!=e) {
            say('Sprixel Error: '+e);
        }
    }
    return '';
};

act_ctor.types = node_types;

return act_ctor;
})();
1;