
// operation not to occur concurrently with another statement, logically.
Act.types.statement = function(){
    do { switch(this.phase) {
    case 0:
        this.phase = 2;
        /*if (this.node.statement_mod_loop
                && this.node.statement_mod_loop.length) {
            this.phase = 2;
            var stripped = new Act(this, this);
            stripped.statement_mod_loop = null;
            return this.do_next = new Act({
                T: 'statement_control__S_while',
                phase: 0,
                block_override: stripped,
                M: { M: [this.statement_mod_loop[0].modifier_expr] }
            }, this);
        }
        if (this.statement_mod_cond
                && typeof(this.statement_mod_cond.length)!='undefined'
                && this.statement_mod_cond.length) {
            this.phase = 1;
            return this.do_next = new Act(this.statement_mod_cond[0], this);
        } else {
            this.do_next = { result: new p6builtin.Bool(true) };
        }
    case 1:
        this.phase = 2;
        var do_next;
        if (this.do_next.result.toBool()*/
        return this.next = new Act(this.node.statement_control || this.node.EXPR
            || { T: 'noop'}, this);
    case 2:
        this.result = this.next.result;
        return this.invoker;
    } } while (false);
    this.next = null;
    return this.invoker;
};
1;
