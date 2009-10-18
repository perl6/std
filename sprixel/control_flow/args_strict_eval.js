
// strictly evaluate an array of argument expressions; collate their results
Act.types.args_strict_eval = function(){
    do { switch(this.phase) {
    case 0:
        //throw keys(this.node.M[0].EXPR);
        this.idx = -1;
        if (this.node.M) {
            this.MM = Array(this.len = this.node.M.length);
            this.phase = 1;
        } else {
            this.result = [];
            break;
        }
    case 1:
        while (++this.idx < this.len) {
            return this.MM[this.idx] = new Act(this.node.M[this.idx], this);
        }
        this.result = [];
        for (var i=0,l=this.MM.length;i<l;++i){
            this.result[i] = this.MM[i].result;
        }
        this.result = Array.flatten.call(this.result);
        break;
    } } while (false);
    return this.invoker;
};
1;