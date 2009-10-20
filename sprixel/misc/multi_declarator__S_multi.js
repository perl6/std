
// 
Act.types.multi_declarator__S_multi = function(){
    //throw keys(this.node.def_module_name[0]);
    //throw keys(this.node);
    do { switch(this.phase) {
    case 0:
        this.phase = 1;
        return this.next = new Act(this.node.declarator, this);
    default: break;
    } } while (false);
    var multiname = this.node.declarator.routine_declarator.routine_def.
        deflongname[0].name.identifier.TEXT+'__S_'+mangle(
            this.node.declarator.routine_declarator.routine_def.
                deflongname[0].colonpair[0].circumfix.nibble.M[0].TEXT);
    this.context.symbols['multi__'+multiname] = this.result = this.next.result;
    this.next = null;
    return this.invoker;
};
1;