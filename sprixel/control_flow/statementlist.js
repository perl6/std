
// zero or more statements
Act.types.statementlist = function(){
    do { switch(this.phase) {
    case 0:
        this.idx = -1;
        if (this.node.M) {
            this.MM = Array(this.len = this.node.M.length);
            this.phase = 1;
        } else if (this.node.statement && Type(this.node.statement)!='Array') {
            this.phase = 2;
            this.len = 1;
            this.MM = Array(1);
            return this.next = new Act(this.node.statement, this);
        } else {
            this.result = 1;//new p6builtin.Undef();
            break;
        }
    case 1:
        while (++this.idx < this.len) {
            var next;
            if ((next = this.MM[this.idx] = new Act(this.node.M[this.idx], this))
                    .node.T != 'eat_terminator') {
                return next;
            }
        }
        while (this.len > 0) {
            if (this.MM[--this.len].node.T != 'eat_terminator') {
                this.result = this.MM[this.len].result;
                break;
            }
        }
        if (typeof(this.result)=='undefined') {
            this.result = this.MM[this.len - 1]
                ? this.MM[this.len - 1].result
                : 0;//new p6builtin.Undef();
        }
        break;
    case 2:
        this.result = this.next.result;
        if (typeof(this.result)=='undefined') {
            this.result = 0;//new p6builtin.Undef();
        }
        break;
    case 17:
        if (Type(this.result)=='Array') {
            this.result = Array.flatten.call(this.result)[0];
        }
        break;
    } } while (false);
    this.MM = this.next = null;
    return this.invoker;
};
1;