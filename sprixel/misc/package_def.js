
// 
Act.types.package_def = function(){
    //throw keys(this.node.def_module_name[0]);
    //throw keys(this.node);
    do { switch(this.phase) {
    case 0:
        this.phase = 1;
        if (typeof this.context.classes == 'undefined') {
            this.context.classes = {};
        }
        this.class_obj = this.context.classes[
            this.class_name = this.node.decl.name] = {};
        return this.next = new Act(this.node.block, this);
    default: break;
    } } while (false);
    this.result = this.next.result;
    this.next = null;
    return this.invoker;
};
1;