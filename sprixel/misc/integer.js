
// 
Act.types.integer = function(){
    this.result = typeof this.node.result != 'undefined'
        ? this.node.result
        : (this.node.result = this.context.classes.Int.ctor(this.node.TEXT));
    return this.invoker;
};

1;
