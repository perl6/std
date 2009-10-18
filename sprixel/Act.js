// Act.js - define a constructor for Activation objects, which keep
//          state information for an interpreter executing the
//          instructions in an Abstract Syntax Tree (AST).

var trace_on, trace_off; // will be assigned functions that return a
               // boolean indicating the state of the global_trace flag.

var Act = (
    function(){
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

        act_ctor.prototype.phase = 0; // used 

        act_ctor.prototype.exec = function() {
            var T;
            if (typeof(T = node_types[this.node.T]) != 'undefined') {
                return T.call(this);
            }
            throw 'NYI: '+this.node.T;
        };

        // entry point to interpret an entire Abstract Syntax Tree
        act_ctor.interpret = function(ast) {
            // start with one Activation object for the initial node
            // of the Abstact Syntax Tree.
            var act = new act_ctor(ast, { context: { symbols: {} } });
            try {
                // Runloop for the interpreter. Optionally tracing the
                // nodes it executes, each exec() call returns the next
                // node that must be executed (this implements the
                // Continuation Passing Style (CPS) which is alternative
                // to stack-based preservation of execution state).
                for (;;(global_trace && say(act.node.T)),(act = act.exec()));
                // when the loop exits, program execution has ended.
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
    }
)();
// the Act variable has been assigned a function, which will be called
// without any parameters. The function returns an Activation
// Constructor object (act_ctor).

// The Activation Constructor object (returned by Act) is equipped with
// a prototype property called phase and a prototype method called
// exec(), which will be inherited by any Activation objects constructed
// by the Activation Constructor.

// The Activation Constructor object also has an interpret() method
// which is called as the entry point from sprixel.pl to execute an
// Abstract Syntax Tree of code.

1;
