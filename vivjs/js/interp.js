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
statement_control__S_if:function(){
    switch(this.phase) {
    case 0:
        this.elsif_index = -1;
        this.phase = 1;
        this.do_after = this.xblock.pblock.blockoid.statementlist;
        return [this.do_next = dupe(this.xblock.EXPR),this];
    case 1:
        if (this.do_next.result.toBool()) {
            this.phase = 2;
            return [this.do_next = dupe(this.do_after), this];
        }
        if (this.elsif && this.elsif.length != 0
                && ++this.elsif_index < this.elsif.length) {
            this.do_after = 
                this.elsif[this.elsif_index].pblock.blockoid.statementlist;
            return [this.do_next =
                dupe(this.elsif[this.elsif_index].EXPR), this];
        } else if (this['else'] && this['else'].length != 0) {
            this.phase = 2;
            return [this.do_next =
                dupe(this['else'][0].blockoid.statementlist), this];
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
scope_declarator__S_my:function(){
    switch(this.phase) {
    case 0:
        ++this.phase;
        return [this.do_next = dupe(this.M.M.M.M),this];
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
            ? this.eval_args[1].decrement().value
            : this.eval_args[1].increment().value;
    }
    return [this.invoker];
},
variable:function(){
    this.result = new p6builtin.p6var(this.sigil.TEXT,
        this.desigilname.longname.name.identifier.TEXT, this.context);
    return [this.invoker];
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
},/*
statement_control__S_use:function(){
    // only require/BEGIN the fake Test.pm
    var ctx = this.context;
    ctx.is;
    return [this.invoker];
},*/
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
    switch(this.phase) {
    case 0:
        ++this.phase;
        return [this.do_next = dupe(this.blockoid),this];
    case 1:
        this.result = this.do_next.result;
        return [this.invoker];
    }
},
blockoid:function(){
    this.result = new p6builtin.p6sub(this.statementlist, this.context);
    return [this.invoker];
},
p6sub_invocation:function(){
    switch(this.phase) {
    case 0:
        ++this.phase;
        this.do_next = dupe(this.sub_body);
        // derive new Scope from the declaration context (new "lexpad"/frame)
        this.do_next.context = new Scope(this.declaration_context);
        this.do_next.invoker = this;
        this.do_next.phase = 0;
        return [this.do_next];
    case 1:
        this.result = this.do_next.result;
        return [this.invoker];
    }
},
Methodcall:function(){
    switch(this.phase) {
    case 0:
        ++this.phase;
        if (this.eval_args[0] instanceof p6builtin.p6sub) {
            return [this.do_next = dupe(this.eval_args[0]),this];
        } else if (this.eval_args[0] instanceof p6builtin.p6var &&
                this.eval_args[0].value instanceof p6builtin.p6sub) {
            return [this.do_next = dupe(this.eval_args[0].value), this];
        }
        throw keys(this.eval_args[0]) + ' cannot be invoked';
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
disp.termish = disp.numish;
disp.nibbler = disp.eat_terminator;

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
            if (last.invoker && last.invoker===act) {
                doPostDo(last);
            }
        }
        if (disp[act.T]) {
            if (act.args && !act.eval_args) {
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
                    last.result.func.apply(last, last.eval_args);
                }
            }
        } else {
            throw act.T+' not yet implemented; srsly!!?!?\nlast: '+last.T+'\n'
                + keys(act).join(',');
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
        return p6builtin.Undef;
    }
}
1;
