var Context = (function(){

function context_to_string(){
    var res = [], symbols = this.symbols;
    for (var i in symbols) {
        res.push(i+' ('+Type(symbols[i])+')');
    }
    return 'Context with keys: [ '+res.join(' , ')+' ]';
}

function Deriver(){}

var contextId = 0;

var context_constructor = function(parentContext){
    Deriver.prototype = parentContext.symbols;
    this.symbols = new Deriver();
    this.contextId = ++contextId;
};

return context_constructor;
})();
1;