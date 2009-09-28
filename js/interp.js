var global_trace = 0
// + 1;

var disp = { // "bytecode" dispatch - by name of grammar node.
statementlist:function(){
    switch(this.phase) {
    case 0:
        this.idx = -1;
        if (this.M) {
            this.MM = Array(this.len = this.M.length);
            this.phase = 1;
        } else {
            this.phase = 2;
            return [this.do_next = dupe(this.statement), this];
        }
    case 1:
        while (++this.idx < this.len) {
            var next;
            if ((next = this.MM[this.idx]
                    = dupe(this.M[this.idx])).T != 'eat_terminator') {
                return [next,this];
            }
        }
        while (this.len > 0) {
            if (this.MM[--this.len].T != 'eat_terminator') {
                this.result = this.MM[this.len].result;
                break;
            }
        }
        if (typeof(this.result)=='undefined') {
            this.result = this.MM[this.len - 1]
                ? this.MM[this.len - 1].result
                : new p6builtin.Undef();
        }
        return [this.invoker];
    case 2:
        this.result = this.do_next.result;
        if (typeof(this.result)=='undefined') {
            this.result = new p6builtin.Undef();
        }
        return [this.invoker];
    }
},
statement:function(){
    switch(this.phase) {
    case 0:
        if (this.statement_mod_cond
                && typeof(this.statement_mod_cond.length)!='undefined'
                && this.statement_mod_cond.length) {
            this.phase = 1;
            return [this.do_next =
                dupe(this.statement_mod_cond[0]), this];
        } else {
            this.do_next = 0;
        }
    case 1:
        this.phase = 2;
        if ((!this.do_next || this.do_next.result.toBool())
                && (this.statement_control || this.EXPR))
            return [this.do_next =
                dupe(this.statement_control || this.EXPR),this];
        return [this.invoker];
    case 2:
        this.result = this.do_next.result;
        return [this.invoker];
    }
},
statement_mod_cond__S_if:function(){
    switch(this.phase) {
    case 0:
        ++this.phase;
        return [this.do_next = dupe(this.modifier_expr),this];
    case 1:
        this.result = this.do_next.result;
        return [this.invoker];
    }
},
statement_control__S_unless:function(){
    switch(this.phase) {
    case 0:
        this.elsif_index = -1;
        this.phase = 1;
        this.do_after = this.xblock.pblock;
        return [this.do_next = dupe(this.xblock.EXPR),this];
    case 1:
        if (!this.do_next.result.toBool()) {
            this.phase = 2;
            return [this.do_next = dupe(this.do_after), this];
        }
        if (this.elsif && this.elsif.length != 0
                && ++this.elsif_index < this.elsif.length) {
            this.do_after = 
                this.elsif[this.elsif_index].pblock;
            return [this.do_next =
                dupe(this.elsif[this.elsif_index].EXPR), this];
        } else if (this['else'] && this['else'].length != 0) {
            this.phase = 2;
            return [this.do_next =
                dupe(this['else'][0]), this];
        }
        this.result = new p6builtin.Nil();
        return [this.invoker];
    case 2:
        this.result = this.do_next.result;
        return [this.invoker];
    }
},
statement_control__S_if:function(){
    switch(this.phase) {
    case 0:
        this.elsif_index = -1;
        this.phase = 1;
        this.do_after = this.xblock.pblock;
        return [this.do_next = dupe(this.xblock.EXPR),this];
    case 1:
        if (this.do_next.result.toBool()) {
            this.phase = 2;
            return [this.do_next = dupe(this.do_after), this];
        }
        if (this.elsif && this.elsif.length != 0
                && ++this.elsif_index < this.elsif.length) {
            this.do_after = 
                this.elsif[this.elsif_index].pblock;
            return [this.do_next =
                dupe(this.elsif[this.elsif_index].EXPR), this];
        } else if (this['else'] && this['else'].length != 0) {
            this.phase = 2;
            return [this.do_next =
                dupe(this['else'][0]), this];
        }
        this.result = new p6builtin.Nil();
        return [this.invoker];
    case 2:
        this.result = this.do_next.result;
        return [this.invoker];
    }
},
modifier_expr:function(){
    switch(this.phase) {
    case 0:
        ++this.phase;
        return [this.do_next = dupe(this.EXPR),this];
    case 1:
        this.result = this.do_next.result;
        return [this.invoker];
    }
},
SYMBOL__:function(){
    switch(this.phase) {
    case 0:
        ++this.phase;
        return [this.do_next = dupe(this[this.SYM]),this];
    case 1:
        this.result = this.do_next.result;
        return [this.invoker];
    }
},
numish:function(){
    switch(this.phase) {
    case 0:
        ++this.phase;
        return [this.do_next = dupe(this.M),this];
    case 1:
        this.result = this.do_next.result;
        return [this.invoker];
    }
},
integer:function(){
    // cache integer generation since it's immutable
    if (typeof(this.result)=='undefined') {
        this.result = new p6builtin.Int(this.TEXT);
    }
    return [this.invoker];
},
dec_number:function(){
    if (typeof(this.result)=='undefined') {
        this.result = new p6builtin.Num(this.coeff.TEXT + 
            (typeof(this.escale[0])=='undefined' ? ''
                : this.escale[0].TEXT));
    }
    return [this.invoker];
},
eat_terminator:function(){
    return [this.invoker];
},
identifier:function(){
    this.result = symbol_lookup(this.context, this.TEXT);
    return [this.invoker];
},
NIBBLER__:function(){
    switch(this.phase) {
    case 0:
        this.phase = 1;
        return [this.do_next = this.nibble.M.length < 2
            ? dupe(this.nibble.M[0])
            : { T: 'Concatenation', args: this.nibble.M },this];
    case 1:
        this.result = this.do_next.result;
        return [this.invoker];
    }
},
Concatenation:function(){
    // Str concatenation
    var strings = Array(this.eval_args.length);
    // TODO: make iterative (stackless) and don't assume toString() won't
    // overflow the JS stack.
    // TODO: use the Perl 6 stringify instead of just JS toString()
    for (var i=0,a; i < strings.length; ++i) {
        var a = this.eval_args[i];
        strings[i] = a instanceof p6builtin.p6var
            ? a.value.toString()
            : typeof(a)=='string'
                ? a
                : a.toString();
    }
    this.result = strings.join('');
    return [this.invoker];
},
Str:function(){
    this.result = new p6builtin.Str(this.TEXT);
    return [this.invoker];
},
eval_args:function eval_args(){
    switch(this.phase) {
    case 0:
        this.idx=-1;
        if (!this.M) {
            this.invoker.eval_args = this.invoker.eval_args || [];
            this.phase = 4;
            return [this.invoker];
        }
        if (!this.M.length) {
            this.M = [this.M];
        }
        this.len=this.M.length;
        this.invoker.eval_args = [];
        ++this.phase;
    case 1:
        if (this.idx > -1) {
            this.invoker.eval_args.push(this.do_next.result);
        }
        if (this.idx < this.len - 1) {
            return [this.do_next = dupe(this.M[++this.idx]),this];
        } else {
            this.result = this.invoker.eval_args;
            return [this.invoker];
        }
    case 2:
        if (disp[this.invoker.T]===eval_args
                || this.invoker.T=='List_assignment') {
            for (var i=0;i<this.eval_args.length;++i) {
                this.invoker.eval_args.push(this.eval_args[i]);
            }
            this.invoker.phase = 2;
        } else {
            this.invoker.eval_args = this.result = this.eval_args;
        }
        return [this.invoker];
    case 3:
        this.result = this.do_next.result;
        return [this.invoker];
    }
},
Comma:function(){
    if (this.invoker.eval_args) {
        for (var i=0;i<this.eval_args.length;++i) {
            this.invoker.eval_args.push(this.eval_args[i]);
        }
    } else {
        this.invoker.eval_args = this.eval_args;
        this.invoker.phase = 2;
    }
    return [this.invoker];
},
new_variable:function(){
    this.result = new p6builtin.p6var(this.sigil.TEXT,
        this.desigilname.longname.name.identifier.TEXT, this.context, true);
    return [this.invoker];
},
scope_declarator__S_my:function(){
    switch(this.phase) {
    case 0:
        ++this.phase;
        this.do_next = dupe(this.M.M.M.M);
        this.do_next.T = 'new_variable';
        return [this.do_next,this];
    case 1:
        var a = this.do_next.result;
        this.result = this.context[a.sigil+a.name] = this.do_next.result;
        return [this.invoker];
    }
},
Autoincrement:function(){
    //throw keys(this.eval_args[0])
    if (!(this.eval_args[0] instanceof p6builtin.p6var)) { // check num role
        throw "Can't modify read-only value at "+this.BEG;
    }
    if(this.M[1].T == 'POST') { // post-increment/decrement
        this.result = this.eval_args[0].value;
        this.M[1].M.T=='postfix__S_MinusMinus'
            ? this.eval_args[0].decrement().value
            : this.eval_args[0].increment().value;
    } else { // pre-increment/decrement
        this.result = this.M[0].M.T=='prefix__S_MinusMinus'
            ? this.eval_args[0].decrement().value
            : this.eval_args[0].increment().value;
    }
    return [this.invoker];
},
variable:function(){
    //if (this.desigilname.longname.name.identifier.TEXT=='True') throw keys(this.desigilname.longname);
    this.result = new p6builtin.p6var(this.sigil.TEXT,
        this.desigilname.longname.name.identifier.TEXT, this.context);
    return [this.invoker];
    //say(keys(this.context));
},
Item_assignment:function(){
    this.eval_args[0].set(this.eval_args[1]);
    this.result = this.eval_args[1];
    return [this.invoker];
},
noun__S_variable:function(){
    switch(this.phase) {
    case 0:
        ++this.phase;
        return [this.do_next = dupe(this.variable),this];
    case 1:
        this.result = this.do_next.result;
        return [this.invoker];
    }
},
statement_control__S_use:function(){
    // only require/BEGIN the fake Test.pm
    //var ctx = this.context;
    //ctx.is;
    return [this.invoker];
},
circumfix__S_Paren_Thesis:function(){
    switch(this.phase) {
    case 0:
        ++this.phase;
        return [this.do_next = { T: 'statementlist', M: this.semilist.M },this];
    case 1:
        this.result = this.do_next.result;
        return [this.invoker];
    }
},
Additive:function(){
    this.result = this.eval_args[0].do_Additive(this.eval_args[1],
        this.M[1].M.T == 'infix__S_Minus');
    return [this.invoker];
},
Multiplicative:function(){
    this.result = this.eval_args[0].do_Multiplicative(this.eval_args[1],
        this.M[1].M.T == 'infix__S_Slash' ? 1
            : this.M[1].M.T == 'infix__S_Percent' ? 2
            : this.M[1].M.T == 'infix__S_PlusLt' ? 3
            : this.M[1].M.T == 'infix__S_PlusGt' ? 4
            : 0);
    return [this.invoker];
},
List_assignment:function(){
    this.result = this.eval_args[0];
    this.result.value = new p6builtin.p6array(this.eval_args.slice(1));
    return [this.invoker];
},
routine_declarator__S_sub:function(){
    switch(this.phase) {
    case 0:
        ++this.phase;
        return [this.do_next = dupe(this.routine_def),this];
    case 1:
        this.result = this.do_next.result;
        return [this.invoker];
    }
},
routine_def:function(){
    //throw keys(this); 
    switch(this.phase) {
    case 0:
        ++this.phase;
        this.do_next = dupe(this.blockoid);
        var arg_slots = [], signature;
        if (this.multisig && this.multisig.length) {
            for (var i in (signature = this.multisig[0].signature)) {
                arg_slots[i] = [signature[i].parameter[0].param_var.sigil.TEXT,
                    signature[i].parameter[0].param_var.name[0].TEXT];
            }
        }
        this.do_next.arg_slots = arg_slots;
        return [this.do_next,this];
    case 1:
        this.result = this.do_next.result;
        return [this.invoker];
    }
},
blockoid:function(){
    switch(this.phase) {
    case 0:
        this.result = new p6builtin.p6sub(this.statementlist, this.context,
            this.arg_slots);
        if (this.invoker.T == 'pblock') {
            this.phase = 1;
            this.do_next = dupe(this.result);
            return [this.do_next, this];
        }
        return [this.invoker];
    case 1:
        this.result = this.do_next.result;
        return [this.invoker];
    }
},
p6sub_invocation:function(){
    switch(this.phase) {
    case 0:
        ++this.phase;
        this.do_next = dupe(this.sub_body);
        this.do_next.invoker = this;
        // derive new Scope from the declaration context (new "lexpad"/frame)
        var ctx = this.do_next.context = new Scope(this.declaration_context);
        for (var i in this.arg_slots) {
            var arg_var = new p6builtin.p6var(this.arg_slots[i][0],
                this.arg_slots[i][1], ctx, true);
            arg_var.value = typeof(this.arg_array[i])!='undefined'
                ? this.arg_array[i][0]
                : new p6builtin.Undef();
        }
        this.do_next.phase = 0;
        return [this.do_next];
    case 1:
        this.result = this.do_next.result;
        return [this.invoker];
    }
},
Methodcall:function(){
    //throw keys(this.M[1].M.semiarglist);
    switch(this.phase) {
    case 0:
        this.subr = this.eval_args[0];
        if (this.M && this.M[1] && this.M[1].M.semiarglist) {
            this.phase = 1;
            // replaces eval_args with the sub's args
            return [this.do_next = dupe(this.M[1].M.semiarglist), this];
        } else {
            this.eval_args = []; // empty the sub's arg list to-be
            this.do_next = null;
        }
        // fall through
    case 1:
        this.phase = 2;
        //throw S(this.eval_args);
        if (this.subr instanceof p6builtin.p6sub) {
            this.do_next = dupe(this.subr);
            this.do_next.arg_array = this.eval_args;
            return [this.do_next, this];
        } else if (this.subr instanceof p6builtin.p6var &&
                this.subr.value instanceof p6builtin.p6sub) {
            this.do_next = dupe(this.subr.value);
            this.do_next.arg_array = this.eval_args;
            return [this.do_next, this];
        }
        throw keys(this.subr) + ' cannot be invoked';
    case 2:
        this.result = this.do_next.result;
        return [this.invoker];
    }
},
Tight_and:function(){
    switch(this.phase) {
    case 0:
        this.last_index = 0;
        this.phase = 1;
        this.do_next = null;
    case 1:
        if (!this.do_next || (this.do_next.result.toBool()
                && ++this.last_index < this.args.length)) {
            return [this.do_next =
                dupe(this.args[this.last_index]), this];
        }
        this.result = this.do_next.result;
        return [this.invoker];
    }
},
Tight_or:function(){
    switch(this.phase) {
    case 0:
        this.last_index = 0;
        this.phase = 1;
        this.do_next = null;
    case 1:
        if (!this.do_next || (!this.do_next.result.toBool()
                && ++this.last_index < this.args.length)) {
            return [this.do_next =
                dupe(this.args[this.last_index]), this];
        }
        this.result = this.do_next.result;
        return [this.invoker];
    }
},
rad_number:function(){
    var radix = +this.radix.TEXT;
    if (radix > 36 || radix < 2)
        throw 'radix out of range (2..36)';
    this.result = new p6builtin.Int(this.intpart.TEXT, radix);
    return [this.invoker];
},
Symbolic_unary:function(){
    // yes, yes, it currently treats all Symbolic_unary as the negation sign.
    var sym;
    switch(sym = this.M[0].prefix.T) {
    case 'prefix__S_Minus':
        this.result = new p6builtin.Int(this.eval_args[0].negate());
        break;
    case 'prefix__S_Tilde':
        this.result = new p6builtin.Str(this.eval_args[0].toString());
        break;
    default: throw 'Symbolic_unary '+sym+' not yet implemented; srsly!!?!??';
    }
    return [this.invoker];
},
comparison_op:function(){
    var op = 'do_'+this.comp_node, op_method;
    if (typeof(op_method = this.left[op])=='undefined') {
        throw 'comparison_op '+this.comp_node+' not yet implemented; srsly!!?!??';
    } else {
        this.result = op_method.call(this.left, this.right);
    }
    return [this.invoker];
},
termish:function(){
    switch(this.phase) {
    case 0:
        if (this.M.T) {
            this.phase = 2;
            return [this.do_next = dupe(this.M),this];
        }
        // handle chained operators
        this.last_index = -1;
        //throw keys(this);
        this.MM = Array(this.chain.length);
        this.do_next = null;
        this.phase = 1;
        // fall through
    case 1:
        // evaluate first 2 args
        if (this.last_index == this.MM.length) {
            this.result = this.do_next.result;
            return [this.invoker];
        }
        if (this.last_index > -1) { // gather result of arg or comparison
            this.MM[this.last_index] = this.do_next.result;
        }
        if ((++this.last_index % 2) == 0) { // queue up next arg
            if (this.last_index > 2 && !this.do_next.result.toBool()) {
                this.result = this.do_next.result;
                return [this.invoker];
            }
            return [this.do_next = dupe(this.chain[this.last_index]), this];
        } else if (this.last_index == 1) { // skip "first" comparison
            return [this.do_next = dupe(this.chain[++this.last_index]), this];
        } else { // ready for a comparison
            //throw keys(this.chain[this.last_index - 2].infix.T);
            return [this.do_next = {
                T: 'comparison_op',
                comp_node: this.chain[this.last_index - 2].infix.T,
                left: this.MM[this.last_index - 3],
                right: this.MM[this.last_index - 1]
            }, this];
        }
    case 2:
        this.result = this.do_next.result;
        return [this.invoker];
    }
},
term__S_name:function(){
    this.result = new p6builtin.p6var('',
        this.longname.name.identifier.TEXT, this.context);
    //throw keys(this);
    return [this.invoker];
},
circumfix__S_Cur_Ly:function(){
    switch(this.phase) {
    case 0:
        ++this.phase;
        return [this.do_next = dupe(this.pblock),this];
    case 1:
        this.result = this.do_next.result;
        return [this.invoker];
    }
},
pblock:function(){
    switch(this.phase) {
    case 0:
        ++this.phase;
        return [this.do_next = dupe(this.blockoid),this];
    case 1:
        this.result = this.do_next.result;
        return [this.invoker];
    }
}
};
// aliases (nodes with identical semantics)
disp.term__S_identifier = disp.noun__S_term = disp.number__S_numish =
    disp.value__S_number = disp.noun__S_value = disp.value__S_quote =
    disp.noun__S_circumfix = //disp.circumfix__S_Paren_Thesis = 
    disp.noun__S_scope_declarator = disp.noun__S_routine_declarator =
    disp.SYMBOL__;
disp.quote__S_Double_Double = disp.quote__S_Single_Single = disp.NIBBLER__;
disp.args = disp.arglist = disp.semiarglist = disp.eval_args;
disp.xblock = disp.escape__S_At = disp.escape__S_Dollar = disp.modifier_expr;
disp.term__S_DotDotDot = disp.comment__S_Sharp =
    disp.terminator__S_Semi = disp.nibbler = disp.eat_terminator;
disp.Loose_and = disp.Tight_and;
disp.Loose_or = disp.Tight_or;

function keys(o) {
    var res = [], j=-1;
    for (var i in o) res[++j] = i;
    return res;
}

function top_interp(obj,context) {
    interp(obj.M,context);
    return '';
}

function S(s){
    say(JSON.stringify(s,null,' '));
};

function addPostDo(act,func){
    if (!act.doPostDo) {
        act.doPostDo = [];
    }
    act.doPostDo.push(func);
}

function doPostDo(act){
    if (act.doPostDo) {
        for (var i = 0; i < act.doPostDo.length; ++i) {
            act.doPostDo[i].call(act);
        }
    }
}

var __lazyarg_Types = {
    Tight_and : 1,
    Tight_or : 1,
    Loose_and: 1,
    Loose_or: 1
};
/*
var __lazyarg_Identifiers = {
    'ok' : 1,
    'is' : 1,
    'isnt' : 1,
    'is_deeply' : 1,
    'isa_ok' : 1
};
*/
function interp(obj,context) {
    var act = obj, result = Array(1), empty = [0], last = act;
    act.phase = 0; act.context = context;
    for(;typeof(act) != 'undefined';) {
        if (result.length > 1) {
            act.invoker = result[1];
            act.phase = 0;
            act.eval_args = null;
            act.context = result[1].context;
            if (global_trace) say('trying '+act.T);
            //say(keys(act), act.SYM);
        } else {
            if (global_trace) say('returning to '+act.T);
            //if (global_trace) say('\tresult type was '+Type(last.result)+' .toString() is '+last.result+' and the members are: '+keys(last.result));
            if (last.invoker && last.invoker===act) {
                doPostDo(last);
            }
        }
        if (disp[act.T]) {
            if (act.args && !act.eval_args && !__lazyarg_Types[act.T]) {
                act = {
                    T : "eval_args",
                    M : typeof(act.args.length)!='undefined'
                        ? dupe_array(act.args)
                        : [dupe(act.args)],
                    phase : 0,
                    invoker : act,
                    context : act.context
                };
                result = empty;
                continue;
            } else if (act.arg && !act.eval_args) {
                act = {
                    T : "eval_args",
                    M : [dupe(act.arg)],
                    phase : 0,
                    invoker : act,
                    context : act.context
                };
                result = empty;
                continue;
            } else {
                act = (result = disp[act.T].call(last = act))[0];
                if (last.result instanceof p6builtin.jssub && last.eval_args) {
                    //S(last.eval_args);
                    last.result.func.apply(last, last.eval_args);
                }
            }
        } else {
            throw act.T+' not yet implemented; srsly!!?!?\nlast: '+last.T+'\n'
                + keys(act).join(',');
            last = act;
            act = last.invoker;
            result = [null, null];
        }
    }
    return obj.result;
}

function dupe(act){ // shallow clone
    var newact = {};
    for (var i in act) {
        newact[i] = act[i];
    }
    newact.phase = 0;
    newact.postDo = undefined;
    newact.eval_args = null;
    return newact;
}

function dupe_array(act_array){
    var newacts = Array(act_array.length);
    for (var i in act_array) {
        newacts[i] = dupe(act_array[i]);
    }
    return newacts;
}

function symbol_lookup(ctx,sym){
    var result;
    if (typeof(result = ctx[sym]) != 'undefined') {
        return result;
    } else {
        return new p6builtin.Undef();
    }
}

function Type(obj){
    var type;
    switch(type = typeof obj) {
    case 'object':
        if (typeof(obj.T)!='undefined') {
            return obj.T;
        }
        return obj.WHAT ? obj.WHAT() : obj.constructor.name;
    default:
        return type;
    }
}

1;
