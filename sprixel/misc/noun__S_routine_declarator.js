
// 
Act.types.noun__S_routine_declarator = function(){
    //throw keys(this.node.def_module_name[0]);
    //throw keys(this.node.M);
    do { switch(this.phase) {
    case 0:
        this.phase = 1;
        return this.next = new Act(this.node.M, this);
    default: break;
    } } while (false);
    this.result = this.next.result;
    this.next = null;
    return this.invoker;
};
1;