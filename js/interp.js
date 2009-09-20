var disp = { // "bytecode" dispatch - by name of grammar node.
statementlist:function(){
    switch(this.phase) {
    case 0:
        this.idx=-1;
        this.len=this.M.length;
        ++this.phase;
    case 1:
        if (this.idx < this.len - 1) {
            return [this.M[++this.idx],this];
        } else {
            while (this.len > 0) {
                if (this.M[--this.len].T != 'eat_terminator') {
                    this.result = this.M[this.len].result;
                    break;
                }
            }
            if (typeof(this.result)=='undefined') {
                this.result = this.M[0].result;
            }
            return [this.invoker];
        }
    }
},
statement:function(){
    switch(this.phase) {
    case 0:
        ++this.phase;
        this.do_next = this.statement_control
            || this.EXPR;
        return [this.do_next,this];
    case 1:
        this.result = this.do_next.result;
        return [this.invoker];
    }
},
SYMBOL__:function(){
    switch(this.phase) {
    case 0:
        ++this.phase;
        return [this[this.SYM],this];
    case 1:
        this.result = this[this.SYM].result;
        return [this.invoker];
    }
},
numish:function(){
    switch(this.phase) {
    case 0:
        ++this.phase;
        return [this.M,this];
    case 1:
        this.result = this.M.result;
        return [this.invoker];
    }
},
integer:function(){
    this.result = new p6builtin.Int(+this.TEXT);
    return [this.invoker];
},
eat_terminator:function(){
    switch(this.phase) {
    case 0:
        return [this.invoker];
    }
},
identifier:function(){
    this.result = symbol_lookup(this.context, this.TEXT);
    return [this.invoker];
},
termish:function(){
    switch(this.phase) {
    case 0:
        ++this.phase;
        return [this.M,this];
    case 1:
        this.result = this.M.result;
        return [this.invoker];
    }
},
NIBBLER__:function(){
    switch(this.phase) {
    case 0:
        if (this.nibble.M.length < 2) {
            this.phase = 1;
            return [this.nibble.M[0],this];
        }
        this.phase = 2;
        this.concat = {
            T: 'Concatenation',
            args: this.nibble.M
        };
        return [this.concat,this];
    case 1:
        this.result = this.nibble.M[0].result;
        return [this.invoker];
    case 2:
        this.result = this.concat.result;
        return [this.invoker];
    }
},
nibbler:function(){
    return [this.invoker];
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
        if (!this.M.length) {
            this.M = [this.M];
        }
        this.len=this.M.length;
        this.invoker.eval_args = Array(this.len);
        ++this.phase;
    case 1:
        if (this.idx > -1) {
            this.invoker.eval_args[this.idx] = this.M[this.idx].result;
        }
        if (this.idx < this.len - 1) {
            return [this.M[++this.idx],this];
        } else {
            this.result = this.invoker.eval_args;
            return [this.invoker];
        }
    case 2:
        if (disp[this.invoker.T]===eval_args) {
            this.invoker.eval_args = this.eval_args;
            this.invoker.phase = 2;
        } else {
            this.invoker.eval_args = this.result = this.eval_args;
        }
        return [this.invoker];
    }
},
Comma:function(){
    if (this.invoker.eval_args) {
        this.invoker.eval_args = this.invoker.eval_args.concat(this.eval_args);
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
        return [this.M.M.M.M,this];
    case 1:
        var a = this.M.M.M.M.result;
        this.result = this.context[a.sigil+a.name] =
            this.M.M.M.M.result;
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
        return [this.variable,this];
    case 1:
        this.result = this.variable.result;
        return [this.invoker];
    }
},
escape__S_Dollar:function(){
    switch(this.phase) {
    case 0:
        ++this.phase;
        return [this.EXPR,this];
    case 1:
        this.result = this.EXPR.result;
        return [this.invoker];
    }
},
statement_control__S_use:function(){
    // only require/BEGIN the fake Test.pm
    var ctx = this.context;
    ctx.is;
    return [this.invoker];
},
circumfix__S_Paren_Thesis:function(){
    switch(this.phase) {
    case 0:
        ++this.phase;
        this.do_next = {
            T: 'statementlist',
            M: this.semilist.M
        };
        return [this.do_next,this];
    case 1:
        this.result = this.do_next.result;
        return [this.invoker];
    }
}
};
disp.term__S_identifier = disp.noun__S_term = disp.number__S_numish =
    disp.value__S_number = disp.noun__S_value = disp.value__S_quote =
    disp.noun__S_circumfix = //disp.circumfix__S_Paren_Thesis = 
    disp.noun__S_scope_declarator = disp.SYMBOL__;
disp.quote__S_Double_Double = disp.quote__S_Single_Single = disp.NIBBLER__;
disp.args = disp.arglist = disp.semiarglist = disp.eval_args;

function keys(o) {
    var res = [], j=-1;
    for (var i in o) res[++j] = i;
    return res;
}

function top_interp(obj,context) {
    interp(obj.M,context);
    return '';
}

var S=function(s){
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
            act.context = result[1].context;
            //say('trying '+act.T);
            //say(keys(act), act.SYM);
        } else {
            //say('returning to '+act.T);
            if (last.invoker && last.invoker===act) {
                doPostDo(last);
            }
        }
        if (disp[act.T]) {
            if (act.args && !act.eval_args) {
                act = {
                    T : "eval_args",
                    M : typeof(act.args.length)!='undefined'
                        ? act.args
                        : [act.args],
                    phase : 0,
                    invoker : act,
                    context : act.context
                };
                result = empty;
                continue;
            } else if (act.arg && !act.eval_args) {
                act = {
                    T : "eval_args",
                    M : [act.arg],
                    phase : 0,
                    invoker : act,
                    context : act.context
                };
                result = empty;
                continue;
            } else {
                act = (result = disp[act.T].call(last = act))[0];
                if (last.result instanceof p6builtin.p6sub && last.eval_args) {
                    last.result = (last.referent = last.result).
                        func.apply(last.context, last.eval_args);
                }
            }
        } else {
            throw act.T+' not yet implemented; srsly!!?!?';
        }
    }
    return obj.result;
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
