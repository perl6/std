
// 
Act.types.value__S_quote = function(){
    //throw ;
    //throw keys(this.node);
    do { switch(this.phase) {
    case 0:
        this.phase = 1;
        return this.next = new Act(this.node.quote, this);
    case 1:
        this.result = this.next.result;
    default: break;
    } } while (false);
    this.next = null;
    return this.invoker;
};
1;