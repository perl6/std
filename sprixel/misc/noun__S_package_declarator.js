
// 
Act.types.noun__S_package_declarator = function(){
    //throw keys(this.node.package_declarator);
    do { switch(this.phase) {
    case 0:
        this.phase = 1;
        return this.next = new Act(this.node.package_declarator, this);
    default: break;
    } } while (false);
    this.result = this.next.result;
    this.next = null;
    return this.invoker;
};
1;