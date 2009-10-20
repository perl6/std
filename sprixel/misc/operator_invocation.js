
// 
Act.types.operator_invocation = function(){
    //throw ;
    do { switch(this.phase) {
    case 0:
        if (this.node.args && this.node.args.length > 0) {
            this.phase = 1;
            return this.next = new Act({
                T: 'args_strict_eval',
                M: this.node.args
            }, this);
        } else {
            this.next = { result: [] };
        }
    case 1:
        var arg_array = this.next.result;
        this.phase = 2;
        var T;
        if (typeof (T = this.context.symbols['multi__'+this.node.op])
                != 'undefined') {
            this.next = new Act(T, this);
            this.next.arg_array = arg_array;
            return this.next;
        }
        throw 'NYI: '+this.node.op;
    case 2:
        this.result = this.next.result;
    default: break;
    } } while (false);
    this.next = this.args = null;
    return this.invoker;
};
1;