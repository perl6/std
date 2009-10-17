
// 
Act.types.termish = function(){
    do { switch(this.phase) {
    case 0:
        this.phase = 1;
        return this.next = new Act(this.node.noun, this);
    case 1:
        this.result = this.next.result;
    default: break;
    } } while (false);
    this.next = null;
    return this.invoker;
};
1;