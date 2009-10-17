
// 
Act.types.routine_def = function(){
    this.result = {
        T: 'routine_invocation',
        statementlist: this.node.blockoid.statementlist
    };
    return this.invoker;
};
1;