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
            return [this.invoker];
        }
    }
},
statement:function(){
    switch(this.phase) {
    case 0:
        ++this.phase;
        return [this.EXPR,this];
    case 1:
        this.result = this.EXPR.result;
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
    switch(this.phase) {
    case 0:
        ++this.phase;
        this.result = new p6builtin.Integer(+this.TEXT);
        return [this.invoker];
    }
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
}

};
disp.term__S_identifier = disp.noun__S_term = disp.number__S_numish =
    disp.value__S_number = disp.noun__S_value = disp.SYMBOL__;

function keys(o) {
    var res = [], j=-1;
    for (var i in o) res[++j] = i;
    return res;
}

function top_interp(obj,context) {
    interp(obj.M,context);
    return '';
    //var result;
    //return typeof(result = obj.M.result) != 'undefined'
    //    ? result
    //    : 'Error; undefined result';
}

var S=function(s){
    say(JSON.stringify(s));
};

function interp(obj,context) {
    var act = obj, result = Array(1);
    act.phase = 0; act.context = context;
    for(;typeof(act) != 'undefined';) {
        if (result.length > 1) {
            act.invoker = result[1];
            act.phase = 0;
            act.context = result[1].context;
            //say('trying '+act.T);
        } else {
            //say('returning to '+act.T);
        }
        if (disp[act.T]) {
            if (act.args) {
                var ArgList = act.args.M.M, ArgArray = [];
                if (typeof(ArgList.M)!='undefined') {
                    if (typeof(ArgList.M.length)!='undefined') {
                        throw S(ArgList.M);
                    } else if (ArgList.M.T == 'Comma') {
                        ArgList = ArgList.M.M;
                    } else {
                        ArgList = [ArgList.M.M];
                    }
                    for (var i=0;i<ArgList.length;i+=2) {
                        ArgArray[i/2] = interp(ArgList[i],act.context);
                    }
                }
                interp(act[act.SYM],act.context);
                if (act[act.SYM].result instanceof p6builtin.p6sub) {
                    act.result = act[act.SYM].result.
                        func.apply(act.context, ArgArray);
                    act = act.invoker;
                    result = [];
                } else {
                    throw act+" is not a subroutine!";
                }
            } else {
                act = (result = disp[act.T].call(act))[0];
            }
        } else {
            throw 'Type '+act.T+' not found!\n'+S(act,undefined,' ');
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
