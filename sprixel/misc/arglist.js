
// 
Act.types.arglist = function(){
    //throw ;
    //throw keys(this.node.EXPR);
    do { switch(this.phase) {
    case 0:
        this.phase = 1;
        return this.next = new Act({
            T: 'args_strict_eval',
            M: [this.node.EXPR]
        }, this);
    case 1:
        this.result = this.next.result;
    default: break;
    } } while (false);
    this.next = null;
    return this.invoker;
};
1;