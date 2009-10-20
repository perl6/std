
// 
Act.types.nibbler = function(){
    this.result = typeof this.node.result != 'undefined'
        ? this.node.result
        : (this.node.result =
            this.context.classes.Str.ctor(this.node.M[0].TEXT));
    return this.invoker;
};
1;