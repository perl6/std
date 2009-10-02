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
            this.len = 1;
            this.MM = Array(1);
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
        //say(keys(this.statement_mod_loop[0].modifier_expr));
        if (this.statement_mod_loop
                && typeof(this.statement_mod_loop.length)!='undefined'
                && this.statement_mod_loop.length) {
            this.phase = 2;
            var stripped = dupe(this);
            stripped.statement_mod_loop = null;
            return [this.do_next = {
                T: 'statement_control__S_while',
                phase: 0,
                block_override: stripped,
                M: { M: [this.statement_mod_loop[0].modifier_expr] }
            }, this];
        }
        if (this.statement_mod_cond
                && typeof(this.statement_mod_cond.length)!='undefined'
                && this.statement_mod_cond.length) {
            this.phase = 1;
            return [this.do_next =
                dupe(this.statement_mod_cond[0]), this];
        } else {
            this.do_next = { result: new p6builtin.Bool(true) };
        }
    case 1:
        this.phase = 2;
        if (this.do_next.result.toBool()
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
statement_control__S_loop:function(){
    switch(this.phase) {
    case 0:
        this.phase = 1;
        this.catch_next = this.catch_last = true;
        //this.context = new Scope(this.context); // derive scope for ctrl exprs
        this.loop_block = null;
        this.result = new p6builtin.List();
        if (this.eee && this.eee.e1) {
            return [this.do_next = dupe(this.eee.e1), this];
        }
    case 1:
        this.phase = 2;
        if (this.eee && this.eee.e2) {
            return [this.do_next = dupe(this.eee.e2), this];
        } else {
            this.do_next = { result : new p6builtin.Bool(true) };
        }
    case 2:
        if (this.do_next.result.toBool()) {
            this.phase = 3;
            if (!this.loop_block) {
                if (this.block && this.block.blockoid && this.block.blockoid
                    && this.block.blockoid.statementlist) {
                    this.loop_block = new p6builtin.Sub(
                        this.block.blockoid.statementlist, this.context, []);
                } else {
                    return [this.invoker];
                }
            }
            return [this.do_next = this.loop_block, this];
        }
        return [this.invoker];
    case 3:
        this.phase = 1;
        if (this.eee && this.eee.e3) {
            return [this.do_next = dupe(this.eee.e3), this];
        }
        return [this]; // special, trampoline to myself!
    case 13: // interrupt "last"
        return [this.invoker];
    case 15: // interrupt "next"
        this.phase = 3;
        return [this];
    }
},
statement_control__S_while:function(){
    switch(this.phase) {
    case 0:
        this.phase = 1;
        this.catch_next = this.catch_last = true;
        this.loop_block = null;
        if (this.block_override) {
            this.block_override.context = this.context;
        }
    case 1:
        this.phase = 2;
        return [this.do_next = dupe(this.M.M[0]), this];
    case 2:
        if (this.do_next.result.toBool()) {
            this.phase = 1;
            if (this.block_override) {
                this.loop_block = dupe(this.block_override);
            } else if (!this.loop_block) {
                if (this.xblock && this.xblock.pblock.blockoid && this.xblock.pblock.blockoid
                    && this.xblock.pblock.blockoid.statementlist) {
                    this.loop_block = new p6builtin.Sub(
                        this.xblock.pblock.blockoid.statementlist, this.context, []);
                } else {
                    return [this.invoker];
                }
            }
            return [this.do_next = this.loop_block, this];
        }
        return [this.invoker];
    case 13: // interrupt "last"
        return [this.invoker];
    case 15: // interrupt "next"
        this.do_next = { result : new p6builtin.Bool(true) };
        this.phase = 1;
        return [this];
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
        this.result = new p6builtin.Str(this.TEXT).toInt();
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
Replication:function(){
    // this is partly working  - "A" x 3 gives "AAA", but so does
    // "A" xx 3 - should give "A","A","A"
    this.result = '';
    for ( var i=0;  i<this.eval_args[1]; i++ ) {
        this.result = this.result + this.eval_args[0];
    }
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
    if (!this.result.value) {
        throw this.sigil.TEXT+this.desigilname.longname.name.identifier.TEXT+
            ' is not defined';
    }
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
    var sym = this.M[1].M.T, done=false;
    try {
        var leftText, rightText;
        // "constant folding" for division of two ints (coercion to Rat)
        if (sym == 'infix__S_Slash' && 
                ((this.args[0].M.M && this.args[0].M.M.M.M.T == 'integer' &&
                    (leftText = this.args[0].M.M.M.M.TEXT)) ||
                (this.args[0].M[0].prefix.TEXT == '-'
                    && this.args[0].M[1].M.M.M.M.T == 'integer' &&
                    (leftText = '-'+this.args[0].M[1].M.M.M.M.TEXT))) &&
                ((this.args[1].M.M && this.args[1].M.M.M.M.T == 'integer' &&
                    (rightText = this.args[1].M.M.M.M.TEXT)) ||
                (this.args[1].M[0].prefix.TEXT == '-'
                    && this.args[1].M[1].M.M.M.M.T == 'integer' &&
                    (rightText = '-'+this.args[1].M[1].M.M.M.M.TEXT)))) {
            this.result = new p6builtin.Rat(leftText, rightText);
            done = true;
        }
    } catch(e) {};
    if (!done) {
        this.result = this.eval_args[0].do_Multiplicative(this.eval_args[1],
            sym == 'infix__S_Slash' ? 1
                : sym == 'infix__S_Percent' ? 2
                : sym == 'infix__S_PlusLt' ? 3
                : sym == 'infix__S_PlusGt' ? 4
                : 0);
    }
    return [this.invoker];
},
List_assignment:function(){
    this.result = this.eval_args[0];
    this.result.value = new p6builtin.List(this.eval_args.slice(1));
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
        this.result = new p6builtin.Sub(this.statementlist, this.context,
            this.arg_slots);
        if (this.invoker.T == 'pblock'
                && this.invoker.invoker.invoker.invoker.T!='eval_args') {
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
Sub_invocation:function(){
    switch(this.phase) {
    case 0:
        ++this.phase;
        this.do_next = dupe(this.sub_body);
        this.do_next.invoker = this;
        // derive new Scope from the declaration context (new "lexpad"/frame)
        var ctx = this.do_next.context = new Scope(this.declaration_context);
        if (this.topic) { ctx['$_'] = this.topic };
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
        //throw keys(this.M[1].dotty.dottyop.methodop.longname.name.identifier);
        if (this.M && this.M[1] && this.M[1].M.semiarglist) {
            this.phase = 1;
            // replaces eval_args with the sub's args
            return [this.do_next = dupe(this.M[1].M.semiarglist), this];
        } if (this.M && this.M[1] && this.M[1].dotty
                && this.M[1].dotty.dottyop.methodop.arglist.length) {
            //throw keys(this.M[1].dotty.dottyop.methodop.arglist);
            return [this.do_next = dupe(
                this.M[1].dotty.dottyop.methodop.arglist), this];
        } else {
            this.eval_args = []; // empty the sub's arg list to-be
            this.do_next = null;
        }
        // fall through
    case 1:
        this.phase = 2;
        //throw S(this.eval_args);
        if (this.M && this.M[1] && this.M[1].dotty
                && this.M[1].dotty.dottyop) {
            // it's a full blown method call.  this.subr becomes the responding
            //   object; now we need to resolve the method member itself.
            var sym;
            this.subr = symbol_lookup(this.invocant = this.subr, 
                sym = this.M[1].dotty.dottyop.
                    methodop.longname.name.identifier.TEXT);
            if (Type(this.subr)=='Undef()') {
                this.subr = symbol_lookup(this.context, sym);
            }
            if (this.subr && this.subr.isUndefined) { // it's a Type object
                this.result = this.invocant['to'+sym]();
                return [this.invoker];
            }
            this.subr.invocant = this.invocant;
            throw 'real method calls NYI';
        } else if (this.subr instanceof p6builtin.Sub) {
            this.do_next = dupe(this.subr);
            this.do_next.arg_array = this.eval_args;
            return [this.do_next, this];
        } else if (this.subr instanceof p6builtin.p6var &&
                this.subr.value instanceof p6builtin.Sub) {
            this.do_next = dupe(this.subr.value);
            this.do_next.arg_array = this.eval_args;
            return [this.do_next, this];
        }
        //throw Type(this.subr);
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
    switch(this.phase) {
    case 0:
        //throw keys(this.circumfix);
        this.int_radix = +this.radix.TEXT;
        if (this.int_radix > 36 || this.int_radix < 2)
            throw 'radix out of range (2..36)';
        if (!this.intpart) {
            this.phase = 1;
            return [this.do_next = dupe(this.circumfix.semilist), this];
        }
        this.result = new p6builtin.Int(this.intpart.TEXT, this.int_radix);
        return [this.invoker];
    case 1:
        var int_str = this.do_next.result.toString(), res;
        //throw [int_str, /^0([dbox])/i.exec(int_str)];
        if ((/^0[dbox]/i.test(int_str) && this.int_radix==10) || (
            (res = /^0([dbox])/i.exec(int_str))
                && DBOX[res[1]] == this.int_radix
        )) {
            this.result = new p6builtin.Int(
                new p6builtin.Str(this.do_next.result).toInt().toString(),
                    this.int_radix);
        } else {
            this.result = new p6builtin.Int(int_str, this.int_radix);
        }
        return [this.invoker];
    }
},
Symbolic_unary:function(){
    // yes, yes, it currently treats all Symbolic_unary as the negation sign.
    var sym;
    switch(sym = this.M[0].prefix.T) {
    case 'prefix__S_Minus':
        var type = Type(this.eval_args[0]);
        this.result = {'Int()':1,'Num()':1,'Rat()':1}[type]
            ? this.eval_args[0].negate()
            : new p6builtin.Num(p6builtin.Int.bigInt.ZERO.negate());
        break;
    case 'prefix__S_Tilde':
        this.result = new p6builtin.Str(this.eval_args[0].toString());
        break;
    case 'prefix__S_Plus':
        this.result = new p6builtin.Num(this.eval_args[0].toString());
        break;
    case 'prefix__S_PlusCaret':
        this.result = this.eval_args[0].do_NumericComplement();
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
},
number__S_rational:function(){
    this.result = new p6builtin.Rat(this.nu.TEXT, this.de.TEXT);
    return [this.invoker];
},
Exponentiation:function(){
    this.result = this.eval_args[0].do_Exponentiation(this.eval_args[1]);
    return [this.invoker];
},
quote__S_Slash_Slash:function(){
    throw keys(this.M.M[0]);
},
do_iterate_map:function(){
    switch(this.phase) {
    case 0:
        if (!this.list || !(this.count = this.list.do_count())) {
            this.result = new p6builtin.Nil();
            return [this.invoker];
        }
        this.idx = -1;
        this.result = new p6builtin.List();
        this.phase = 1;
    case 1:
        if (this.idx > -1) {
            this.result.push(this.do_next.result);
        }
        if (this.idx < this.count - 1) {
            this.do_next = dupe(this.block);
            this.do_next.topic = this.list.items[++this.idx];
            return [this.do_next, this];
        } else {
            this.last_op.result = this.result;
            return [this.invoker];
        }
    }
}
};
// aliases (nodes with identical semantics)
disp.term__S_identifier = disp.noun__S_term = disp.number__S_numish =
    disp.value__S_number = disp.noun__S_value = disp.value__S_quote =
    disp.noun__S_circumfix =
    disp.noun__S_scope_declarator = disp.noun__S_routine_declarator =
    disp.SYMBOL__;
disp.quote__S_Double_Double = disp.quote__S_Single_Single = disp.NIBBLER__;
disp.args = disp.arglist = disp.semiarglist = disp.semilist = disp.eval_args;
disp.xblock = disp.escape__S_At = disp.escape__S_Dollar = disp.modifier_expr;
disp.term__S_DotDotDot = disp.comment__S_Sharp = disp.terminator__S_Ly =
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

var JSI=0;
function JSIND(n) {
    return Array((JSI += n || 0) + 1).join(' ');
}

function interp(obj,context) {
    var act = obj, result = Array(1), empty = [0], last = act, continuation;
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
            //if (last.invoker && last.invoker===act) {
            //    doPostDo(last);
            //}
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
            } else {
                act = (result = disp[act.T].call(last = act))[0];
                if (last.result instanceof p6builtin.jssub && last.eval_args) {
                    if (continuation // must be a built-in op that is flattened
                            = last.result.func.apply(last, last.eval_args)) {
                        if (!continuation.invoker) {
                            // sometimes the invoker is set by the subroutine
                            continuation.invoker = act;
                        }
                        continuation.context = act.context;
                        continuation.last_op = last;
                        act = continuation;
                        result = empty;
                    }
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
    //newact.postDo = undefined;
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

function filt__(str){
    return str.replace(/_/g,'');
}

1;
