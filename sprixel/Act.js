// Act.js - define a constructor for Activation objects, which keep
//          state information for an interpreter executing the
//          instructions in an Abstract Syntax Tree (AST).

var trace_on, trace_off;
    // will be assigned functions that set and return a boolean storing the
    // state of the global_trace flag inside the following lexical block.

var Act = (function(){
    var global_trace = false;

    trace_on  = function(){ global_trace = true  }; // as promised
    trace_off = function(){ global_trace = false }; // above

    var node_types = {};

    // an activation is an object that carries state about a
    // (Node, Context) pair.
    var act_ctor = function(node, invoker){
        this.node = node;
        this.context = (this.invoker = invoker).context;
    };

    act_ctor.prototype.phase = 0; // used to implement Continuation
    // Passing Style, keeping intermediate results in an Activation
    // object instead of on the stack, letting the runloop evaluate
    // subnodes (eg expressions) and then Continuing with more phases
    // until the node result is complete.

    act_ctor.prototype.exec = function() {
        var T;
        if (typeof(T = node_types[this.node.T]) != 'undefined') {
            return T.call(this);
        }
        throw 'NYI: '+this.node.T;
    };

    // entry point to interpret an entire Abstract Syntax Tree
    act_ctor.interpret = function(ast, setting) {
        // start with one Activation object for the initial node
        // of the Abstact Syntax Tree.
        var context = new Context({ symbols: {} });
        if (setting) {
            var act = new act_ctor(setting, { context: context });
            try { // load the setting into the context
                for (;;(global_trace && say(act.node.T)),(act = act.exec()));
            } catch(e) {}
        }
        var act = new act_ctor(ast, { context: context });
        try {
            // Runloop for the interpreter. Optionally tracing the
            // nodes it executes, each exec() call returns the next
            // node that must be executed (this implements the
            // Continuation Passing Style (CPS) which is alternative
            // to stack-based preservation of execution state).
            for (;;(global_trace && say(act.node.T)),(act = act.exec()));
            // when it throws an exception, program execution has ended.
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
// the Act variable has been assigned the result of a function called
// without any parameters, which returns an Activation Constructor object
// (act_ctor).

// Objects constructed with the Act object (as in new Act(node, this)) inherit
// from Act's prototype object (Act.prototype), which is equipped with a
// property called phase (default value 0) and a method called exec(), which
// (by default unless virtually overridden on the Activation object itself)
// dispatches to the built-in "opcodes" stored in the Act.types hash.

// The Activation Constructor object also has an interpret() method
// which is called as the entry point from sprixel.pl to execute an
// Abstract Syntax Tree of code.

1;
