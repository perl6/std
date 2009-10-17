
// 
Act.types.infix__S_Comma = function(){
    //throw ;
    //throw keys(this.node.args);
    do { switch(this.phase) {
    case 0:
        this.phase = 1;
        return this.next = new Act({
            T: 'args_strict_eval',
            M: this.node.args
        }, this);
    case 1:
        this.result = this.next.result;
    default: break;
    } } while (false);
    this.next = null;
    return this.invoker;
};
1;