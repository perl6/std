
// 
Act.types.routine_declarator__S_sub = function(){
    //throw keys(this.node.def_module_name[0]);
    //throw keys(this.node.routine_def.deflongname[0].name.identifier.TEXT);
    do { switch(this.phase) {
    case 0:
        this.phase = 1;
        return this.next = new Act(this.node.routine_def, this);
    default: break;
    } } while (false);
    this.result = this.context.symbols[
        this.node.routine_def.deflongname[0].name.identifier.TEXT
    ] = this.next.result;
    this.next = null;
    return this.invoker;
};
1;