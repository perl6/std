
// 
Act.types.noun__S_term = function(){
    //throw ;
    //throw keys(this.node.term);
    var id;
    do { switch(this.phase) {
    case 0:
        try {
            if ((id = this.node.term.identifier.TEXT) == 'jseval') {
                if (typeof this.node.js_func == 'undefined') {
                    this.node.js_func = new Function('ctx',
                        this.node.term.args.arglist[0].
                            EXPR.value.quote.nibble.M[0].TEXT);
                }
                this.node.js_func.call(this, this.context);
                return this.invoker;
            } else { // non jseval sub call
            }
        } catch(e) {
            //say('eee: '+e);
        }
        if (this.node.term.args && this.node.term.args.arglist &&
                this.node.term.args.arglist.length > 0) {
            this.phase = 3;
            return this.next = new Act({
                T: 'args_strict_eval',
                M: this.node.term.args.arglist
            }, this);
        } else {
            this.next = { result: [] };
        }
    case 3:
        var sub;
        if (typeof (sub = this.context.symbols[
                this.node.term.identifier.TEXT]) != 'undefined') {
            this.phase = 4;
            var args = this.next.result || [];
            this.next = new Act(sub, this);
            this.next.arg_array = args;
            return this.next;
        } else {
            throw 'no sub named '+id;
        }
    case 4:
        this.result = this.next.result;
    default: break;
    } } while (false);
    this.next = null;
    return this.invoker;
};
1;