
// trailing "if" conditional
Act.types.statement_mod_cond__S_if = function(){
    do { switch(this.phase) {
    case 0:
        this.phase = 1;
        return this.next = new Act(this.node.modifier_expr, this);
    default: break;
    } } while (false);
    this.result = this.next.result;
    this.next = null;
    return this.invoker;
};

// leading "if" conditional
Act.types.statement_control__S_if = function(){
    do { switch(this.phase) {
    case 0:
        this.elsif_index = -1;
        this.phase = 1;
        this.after = this.node.xblock.pblock;
        this.elsif = this.node.elsif;
        this._else = this.node['else'];
        return this.next = new Act({
            T: 'MethodCall',
            invocant: this.node.xblock.EXPR,
            method_name: 'Bool',
            args: []
        }, this);
    case 1:
        if (to_js_bool(this.next.result)) {
            this.phase = 2;
            return this.next = new Act(this.after, this);
        }
        if (this.elsif && this.elsif.length != 0
                && ++this.elsif_index < this.elsif.length) {
            this.after = this.elsif[this.elsif_index].pblock;
            return this.next = new Act(this.elsif[this.elsif_index].EXPR, this);
        } else if (this.node._else && this._else.length != 0) {
            this.phase = 2;
            return this.next = new Act(this._else[0], this);
        }
        this.result = new p6builtin.Nil(); // TODO: yes.
        break;
    case 2:
        this.result = this.next.result;
        break;
    } } while (false);
    this.after = this.elsif = this._else = this.next = null;
    return this.invoker;
};
1;