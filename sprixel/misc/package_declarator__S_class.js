
// 
Act.types.package_declarator__S_class = function(){
    //throw keys(this.node);
    do { switch(this.phase) {
    case 0:
        this.phase = 1;
        return this.next = new Act(this.node.package_def, this);
    default: break;
    } } while (false);
    this.result = this.next.result;
    this.next = null;
    return this.invoker;
};
1;