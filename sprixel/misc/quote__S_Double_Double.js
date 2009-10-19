
// 
Act.types.quote__S_Double_Double = function(){
    //throw ;
    //throw keys(this.node);
    do { switch(this.phase) {
    case 0:
        this.phase = 1;
        return this.next = new Act(this.node.nibble, this);
    case 1:
        this.result = this.next.result;
    default: break;
    } } while (false);
    this.next = null;
    return this.invoker;
};
1;