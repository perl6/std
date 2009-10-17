
// 
Act.types.noun__S_term = function(){
    //throw ;
    //throw keys(this.node.term);
    do { switch(this.phase) {
    case 0:
        try {
            var id;
            if ((id = this.node.term.identifier.TEXT) == 'jseval') {
                if (typeof this.node.js_func == 'undefined') {
                    this.node.js_func = new Function('ctx',
                        this.node.term.args.arglist[0].
                            EXPR.value.quote.nibble.M[0].TEXT);
                }
                this.node.js_func.call(this, this.context);
                return this.invoker;
            } else { // non jseval sub call
                var sub;
                if (typeof (sub = this.context.symbols[id]) != 'undefined') {
                    this.next = 1;
                } else {
                    throw 'no sub named '+id;
                }
            }
        } catch(e) {
            say('eee: '+e);
        }
        if (this.node.term.args && this.node.term.args.arglist &&
            this.node.term.args.arglist.length > 0) {
            this.phase = 1;
            return this.next = new Act({
                T: 'args_strict_eval',
                M: this.node.term.args.arglist
            }, this);
        } else {
            this.next = { result: [] };
        }
    case 1:
        this.phase = 1;
        throw 'NYI: sub calls';
    default: break;
    } } while (false);
    this.next = null;
    return this.invoker;
};
1;