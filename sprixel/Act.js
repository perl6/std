var trace_on, trace_off;

var Act = (function(){

var global_trace = 1;

trace_on = function(){ global_trace = 1 };
trace_off = function(){ global_trace = 0 };

var node_types = {};

// an activation is an object that carries state about a Node, Context pair.
var act_ctor = function(node, invoker){
    this.node = node;
    this.context = (this.invoker = invoker).context;
};

act_ctor.prototype.phase = 0;

act_ctor.prototype.exec = function(T){
    if (T = node_types[this.node.T]) {
        return T.call(this);
    }
    throw 'NYI: '+this.node.T;
};

act_ctor.interpret = function(ast) {
    var act = new act_ctor(ast, { context: { /* TODO: new context goes here */ } });
    try{
        for (;(global_trace&&say(act.node.T))||true;act = act.exec());
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