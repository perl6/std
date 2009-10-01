var p6builtin = {}; (function(){

var bigInt = libBigInt;

// immutable, emulated arbitrary-precision BigInteger
p6builtin.Int = function(integer,radix) {
    if (typeof(integer)=='string') {
        this.v = bigInt.nbi();
        this.v.fromString(filt__(integer),+(radix || 10));
    } else {
        this.v = integer instanceof bigInt
            ? integer
            : bigInt.nbv(integer);
    }
};
p6builtin.Int.prototype = {
WHAT: function(){
    return 'Int()';
},
toString: function(){
    return this.v.toString();
},
toBool: function(){
    return this.v.signum() != 0;
},
succ: function(){
    return new p6builtin.Int(this.v.add(bigInt.ONE));
},
pred: function(){
    return new p6builtin.Int(this.v.subtract(bigInt.ONE));
},
do_Additive:function(right, subtract){
    var left = this;
    right = right.value || right;
    right = right instanceof p6builtin.Int ? right
        : new p6builtin.Int(Number(right));
    return new p6builtin.Int(subtract ? left.v.subtract(right.v) : left.v.add(right.v));
},
do_Multiplicative:function(right, divide){
    var left = this;
    right = right.value || right;
    right = right instanceof p6builtin.Int ? right
        : new p6builtin.Int(Number(right));
    switch(divide || 0) {
    case 3:
        return new p6builtin.Int(left.v.shiftLeft(right.v));
    case 4:
        return new p6builtin.Int(left.v.shiftRight(right.v));
    case 1:
    case 2:
        var q = bigInt.nbi(), r = bigInt.nbi();
        left.v.divRemTo(right.v,q,r);
        return new p6builtin.Int(divide == 1 ? q : r);
    default:
        return new p6builtin.Int(left.v.multiply(right.v));
    }
},
do_infix__S_Lt:function(right){
    right = right.value || right;
    right = right instanceof p6builtin.Int ? right
        : new p6builtin.Int(Number(right));
    return new p6builtin.Bool(this.v.compareTo(right.v) < 0);
},
do_infix__S_LtEqual:function(right){
    right = right.value || right;
    right = right instanceof p6builtin.Int ? right
        : new p6builtin.Int(Number(right));
    return new p6builtin.Bool(this.v.compareTo(right.v) <= 0);
},
do_infix__S_Gt:function(right){
    right = right.value || right;
    right = right instanceof p6builtin.Int ? right
        : new p6builtin.Int(Number(right));
    return new p6builtin.Bool(this.v.compareTo(right.v) > 0);
},
do_infix__S_GtEqual:function(right){
    right = right.value || right;
    right = right instanceof p6builtin.Int ? right
        : new p6builtin.Int(Number(right));
    return new p6builtin.Bool(this.v.compareTo(right.v) >= 0);
},
do_infix__S_EqualEqual:function(right){
    right = right.value || right;
    right = right instanceof p6builtin.Int ? right
        : new p6builtin.Int(Number(right));
    return new p6builtin.Bool(this.v.compareTo(right.v) == 0);
},
do_infix__S_BangEqual:function(right){
    right = right.value || right;
    right = right instanceof p6builtin.Int ? right
        : new p6builtin.Int(Number(right));
    return new p6builtin.Bool(this.v.compareTo(right.v) != 0);
},
negate:function(){
    return new p6builtin.Int(this.v.negate());
},
do_infix__S_TildeTilde:function(right,swapped){
    if (!swapped) {
        return (right.value || right).do_infix__S_TildeTilde(this,true);
    }
    return new p6builtin.Bool(this.isUndefined ? right instanceof p6builtin.Int
        : this.v.compareTo(new p6builtin.Int(right.toString())) == 0);
},
do_Exponentiation:function(right){
    right = right.value || right;
    if (Type(right)!='Int()') {
        throw 'Exponentiation of Int to '+Type(right)+' NYI';
    }
    return new p6builtin.Int(this.v.pow(right.v));
},
do_NumericComplement:function(){
    return new p6builtin.Int(this.v.negate().subtract(bigInt.ONE));
}
};
p6builtin.Int.bigInt = bigInt;

// immutable, boxed JS double
p6builtin.Num = function(num) {
    var sym;
    switch(sym = Type(num)) {
    case 'string':
        this.v = Number(new p6builtin.Str(num).toNum().toString());
        break;
    case 'number':
        this.v = num;
        break;
    case 'Num()':
        this.v = num.v;
        break;
    case 'BigInteger':
        this.v = Number(num.toString());
        break;
    default: throw 'unknown Num initializer type: '+sym;
    }
};
p6builtin.Num.prototype = {
WHAT: function(){
    return 'Num()';
},
toString: function(){
    return this.v.toString();
},
toBool: function(){
    return this.v != 0;
},
succ: function(){
    return new p6builtin.Num(this.v + 1);
},
pred: function(){
    return new p6builtin.Num(this.v - 1);
},
do_Additive:function(right, subtract){
    var left = this;
    right = right.value || right;
    right = right instanceof p6builtin.Num ? right
        : new p6builtin.Num(Number(right.toString()));
    return new p6builtin.Num(subtract ? left.v - right.v : left.v + right.v);
},
do_Multiplicative:function(right, divide){
    var left = this;
    right = right.value || right;
    right = right instanceof p6builtin.Num ? right
        : new p6builtin.Num(Number(right.toString()));
    switch(divide || 0) {
    case 3:
        return new p6builtin.Num(left.v << right.v);
    case 4:
        return new p6builtin.Num(left.v >> right.v);
    case 1:
        return new p6builtin.Num(left.v / right.v);
    case 2:
        return new p6builtin.Num(left.v % right.v);
    default:
        return new p6builtin.Num(left.v * right.v);
    }
},
do_infix__S_Lt:function(right){
    right = right.value || right;
    right = right instanceof p6builtin.Num ? right
        : new p6builtin.Num(Number(right.toString()));
    return new p6builtin.Bool(this.v < right.v);
},
do_infix__S_LtEqual:function(right){
    right = right.value || right;
    right = right instanceof p6builtin.Num ? right
        : new p6builtin.Num(Number(right.toString()));
    return new p6builtin.Bool(this.v <= right.v);
},
do_infix__S_Gt:function(right){
    right = right.value || right;
    right = right instanceof p6builtin.Num ? right
        : new p6builtin.Num(Number(right.toString()));
    return new p6builtin.Bool(this.v > right.v);
},
do_infix__S_GtEqual:function(right){
    right = right.value || right;
    right = right instanceof p6builtin.Num ? right
        : new p6builtin.Num(Number(right.toString()));
    return new p6builtin.Bool(this.v >= right.v);
},
do_infix__S_EqualEqual:function(right){
    right = right.value || right;
    right = right instanceof p6builtin.Num ? right
        : new p6builtin.Num(Number(right.toString()));
    return new p6builtin.Bool(this.v == right.v);
},
do_infix__S_BangEqual:function(right){
    right = right.value || right;
    right = right instanceof p6builtin.Num ? right
        : new p6builtin.Num(Number(right.toString()));
    return new p6builtin.Bool(this.v != right.v);
},
negate:function(){
    return new p6builtin.Num(0 - this.v);
},
do_infix__S_TildeTilde:function(right,swapped){
    if (!swapped) {
        return (right.value || right).do_infix__S_TildeTilde(this,true);
    }
    return new p6builtin.Bool(this.isUndefined ? right instanceof p6builtin.Num
        : this.v == right.v); // they're reduced
}
};

// immutable pair of Ints, representing numerator & denominator of a ratio
p6builtin.Rat = function(nu,de) {
    var sym;
    switch(sym = Type(nu)) {
    case 'string':
    case 'number':
        this.nu = new p6builtin.Int(nu).v;
        break;
    case 'Int()':
        this.nu = nu.v;
        break;
    case 'BigInteger':
        this.nu = nu;
        break;
    case 'object':
    default: throw 'unknown Rat initializer type: '+sym;
    }
    switch(sym = Type(de)) {
    case 'string':
    case 'number':
        this.de = new p6builtin.Int(de).v;
        break;
    case 'Int()':
        this.de = de.v;
        break;
    case 'BigInteger':
        this.de = de;
        break;
    case 'object':
    default: throw 'unknown Rat initializer type: '+sym;
    }
    // reduce it...
    var gcd;
    while ((gcd = this.nu.gcd(this.de)).compareTo(bigInt.ONE) > 0) {
        this.nu = this.nu.divide(gcd);
        this.de = this.de.divide(gcd);
    }
};
p6builtin.Rat.prototype = {
WHAT: function(){
    return 'Rat()';
},
toString: function(){
    return this.toNum().toString();
},
toBool: function(){
    return this.nu != 0;
},
succ: function(){
    return new p6builtin.Rat(this.nu.v.add(this.de.v), this.de.v);
},
pred: function(){
    return new p6builtin.Rat(this.nu.v.subtract(this.de.v), this.de.v);
},
do_Additive:function(right, subtract){
    var left = this;
    right = right.value || right;
    right = right instanceof p6builtin.Rat ? right
        : new p6builtin.Rat(Number(right.toString()));
    var lcm = left.de.lcm(right.de);
    var leftNu = left.nu.multiply(lcm.divide(left.de));
    var rightNu = right.nu.multiply(lcm.divide(right.de));
    return new p6builtin.Rat(subtract ? leftNu.subtract(rightNu)
        : leftNu.add(rightNu), lcm);
},
do_Multiplicative:function(right, divide){
    throw 'Rat Multiplicative not yet implemented; srsly!??!?!';
},
do_infix__S_Lt:function(right){
    throw 'Rat Multiplicative not yet implemented; srsly!??!?!';
},
do_infix__S_LtEqual:function(right){
    throw 'Rat comparisons not yet implemented; srsly!??!?!';
},
do_infix__S_Gt:function(right){
    throw 'Rat comparisons not yet implemented; srsly!??!?!';
},
do_infix__S_GtEqual:function(right){
    throw 'Rat comparisons not yet implemented; srsly!??!?!';
},
do_infix__S_EqualEqual:function(right){
    throw 'Rat comparisons not yet implemented; srsly!??!?!';
},
do_infix__S_BangEqual:function(right){
    throw 'Rat comparisons not yet implemented; srsly!??!?!';
},
negate:function(){
    return new p6builtin.Rat(this.nu.negate(), this.de);
},
do_infix__S_TildeTilde:function(right,swapped){
    if (!swapped) {
        return (right.value || right).do_infix__S_TildeTilde(this,true);
    }
    return new p6builtin.Bool(this.isUndefined ? right instanceof p6builtin.Rat
        : (this.de.compareTo(right.de)==0 &&
        this.nu.compareTo(right.nu)==0)); // they're reduced
},
toNum:function(){
    var q = bigInt.nbi(), r = bigInt.nbi();
    this.nu.divRemTo(this.de, q, r);
    if (r.signum() == 0) {
        return new p6builtin.Int(q);
    }
    return new p6builtin.Num(((r.signum() < 0 || q.signum() < 0) ? '-' : '') +
        (Number(+q.abs().toString()) + Number(r.abs().toString())
            / Number(this.de.toString())).toString());
}
};

p6builtin.Bool = function(bool) {
    this.v = typeof(bool)=='boolean' ? bool : !!bool;
};
p6builtin.Bool.prototype = {
WHAT: function(){
    return 'Bool()';
},
toString: function(){
    return this.v ? '1' : '0';
},
toBool:function(){
    return this.v;
},
negate:function(){
    return new p6builtin.Int(this.toString()).negate();
},
do_infix__S_TildeTilde:function(right,swapped){
    if (!swapped) {
        return (right.value || right).do_infix__S_TildeTilde(this,true);
    }
    return new p6builtin.Bool(this.isUndefined ? right instanceof p6builtin.Bool
        : this.v == right.toBool());
}
};

var DBOX={b:2,B:2,x:16,X:16,o:8,O:8,d:10,D:10};

p6builtin.Str = function(str) {
    this.v = typeof(str)=='string' ? str : str.toString();
};
p6builtin.Str.prototype = {
WHAT: function(){
    return 'Str()';
},
toString: function(){
    return this.v;
},
toBool:function(){
    return this.v.length != 0 && this.v != '0'
},
do_infix__S_lt:function(right){
    return new p6builtin.Bool(this.v < right.v);
},
do_infix__S_le:function(right){
    return new p6builtin.Bool(this.v <= right.v);
},
do_infix__S_gt:function(right){
    return new p6builtin.Bool(this.v > right.v);
},
do_infix__S_ge:function(right){
    return new p6builtin.Bool(this.v >= right.v);
},
do_infix__S_eq:function(right){
    return new p6builtin.Bool(this.v == right.v);
},
do_infix__S_ne:function(right){
    return new p6builtin.Bool(this.v != right.v);
},
negate:function(){
    return new p6builtin.Num(0).negate();
},
do_infix__S_TildeTilde:function(right,swapped){
    if (!swapped) {
        return (right.value || right).do_infix__S_TildeTilde(this, true);
    }
    return new p6builtin.Bool(this.isUndefined ? right instanceof p6builtin.Str
        : right.toString() == this.v);
},
toNum:function(){
    var res;
    var str = filt__(this.v);
    if (isNaN(res = Number(str))) {
        return (res = /^(?:0([dbox]))([^\.]+)/i.exec(str))
            ? new p6builtin.Int(res[2], DBOX[res[1] || 'd'])
            : new p6builtin.Int(str);
    }
    return Math.floor(res)==res ? new p6builtin.Int(res)
        : new p6builtin.Num(res);
},
toInt:function(){
    var res;
    var str = filt__(this.v);
    if (isNaN(res = Number(str)) || res.toString()!=str) {
        return (res = /^(?:0([dbox]))?([^\.]+)/i.exec(str))
            ? new p6builtin.Int(res[2], DBOX[res[1] || 'd'])
            : new p6builtin.Int(str);
    }
    return new p6builtin.Int(str.replace(/(^[^\.]+).*/, "$1"), 10);
}
};

p6builtin.Undef = function(){},
p6builtin.Undef.prototype = {
WHAT: function(){
    return 'Undef()';
},
toString: function(){
    return 'Undef';
},
toBool:function(){
    return false;
},
do_infix__S_TildeTilde:function(right,swapped){
    if (!swapped) {
        return (right.value || right).do_infix__S_TildeTilde(this, true);
    }
    return new p6builtin.Bool(right instanceof p6builtin.Undef);
}
};

p6builtin.Nil = function(){},
p6builtin.Nil.prototype = {
WHAT: function(){
    return 'Nil()';
},
toString: function(){
    return 'Nil';
},
toBool:function(){
    return false;
},
do_infix__S_TildeTilde:function(right,swapped){
    if (!swapped) {
        return (right.value || right).do_infix__S_TildeTilde(this, true);
    }
    return new p6builtin.Bool(right instanceof p6builtin.Nil);
}
};

p6builtin.jssub = function(func,name,source){
    this.func = func;
    this.name = name;
    this.source = func.toString();
};
p6builtin.jssub.prototype = {
WHAT: function(){
    return 'JSSUB';
},
toString:function(){
    return this.source.toString();
},
toBool:function(){
    return true;
}
};

p6builtin.Sub = function(sub_body, declaration_context, arg_slots){
    this.sub_body = sub_body;
    this.arg_slots = arg_slots;
    this.declaration_context = declaration_context; // parent for closure
    this.T = 'Sub_invocation';
};
p6builtin.Sub.prototype = {
WHAT: function(){
    return 'Sub()';
},
toString:function(){
    return this.sub_body.BEG;
},
toBool:function(){
    return true;
},
do_infix__S_TildeTilde:function(right,swapped){
    if (!swapped) {
        return (right.value || right).do_infix__S_TildeTilde(this, true);
    }
    throw 'smartmatch not yet implemented for Sub';
}
};

p6builtin.p6var = function(sigil,name,context,forceDeclare){
// essentially an autovivifying "slot" (STD has prevented undeclared uses!)
    this.sigil = sigil;
    this.name = name;
    this.context = context;
    var a;
    // either create or lookup. :)  Inefficient, I know.
    if (forceDeclare
            || typeof(a = this.context[this.sigil+this.name])=='undefined') {
        this.context[this.sigil+this.name] = this;
        this.value = null;
    } else {
        return a;
    }
};
p6builtin.p6var.prototype = {
WHAT: function(){
    return this.value.WHAT();
},
set:function(value){
    this.value = value;
    return this;
},
toString:function(){
    return this.value.toString();
},
increment:function(){
    this.value = this.value.succ();
    return this;
},
decrement:function(){
    this.value = this.value.pred();
    return this;
},
do_Additive:function(right, subtract){
    return p6builtin.Int.prototype.do_Additive.call(
        this.value, right, subtract);
},
do_Multiplicative:function(right, divide){
    return p6builtin.Int.prototype.do_Multiplicative.call(
        this.value, right, divide);
},
toBool:function(){
    return this.v.toBool();
},
do_infix__S_Lt:function(right){
    return this.value.do_infix__S_Lt(right.value || right);
},
do_infix__S_LtEqual:function(right){
    return this.value.do_infix__S_LtEqual(right.value || right);
},
do_infix__S_Gt:function(right){
    return this.value.do_infix__S_Gt(right.value || right);
},
do_infix__S_GtEqual:function(right){
    return this.value.do_infix__S_GtEqual(right.value || right);
},
do_infix__S_EqualEqual:function(right){
    return this.value.do_infix__S_EqualEqual(right.value || right);
},
do_infix__S_BangEqual:function(right){
    return this.value.do_infix__S_BangEqual(right.value || right);
},
do_infix__S_lt:function(right){
    return this.value.do_infix__S_lt(right.value || right);
},
do_infix__S_le:function(right){
    return this.value.do_infix__S_le(right.value || right);
},
do_infix__S_gt:function(right){
    return this.value.do_infix__S_gt(right.value || right);
},
do_infix__S_ge:function(right){
    return this.value.do_infix__S_ge(right.value || right);
},
do_infix__S_eq:function(right){
    return this.value.do_infix__S_eq(right.value || right);
},
do_infix__S_ne:function(right){
    return this.value.do_infix__S_ne(right.value || right);
},
negate:function(){
    return this.value.negate();
},
do_infix__S_TildeTilde:function(right){
    return (right.value || right).do_infix__S_TildeTilde(this.value || this,
        true);
},
toInt:function(){
    return this.value.toInt();
},
toNum:function(){
    return this.value.toNum();
},
do_Exponentiation:function(){
    return this.do_Exponentiation(right.value || right);
}
};

p6builtin.p6array = function(items){
    this.items = items || [];
};
p6builtin.p6array.prototype = {
toString:function(){
    return this.items.join('');
},
'push':function(item){
    this.items.push(item);
},
do_count:function(){
    return this.items.length;
},
toBool:function(){
    return true;
},
WHAT:function(){
    return 'Array()';
}
};

})();


var Scope = (function(){
    function Deriver(){}
    var contextId = 0;
    return function(parentScope){
        if (!parentScope) {
            this.constructor = Scope;
            this.contextId = contextId++;
            return this;
        }
        Deriver.prototype = parentScope;
        var newScope = new Deriver();
        newScope.constructor = Scope;
        newScope.contextId = contextId++;
        return newScope;
    };
})();

function do_die(msg){
    if (typeof(msg)!='undefined') {
        throw msg.toString();
    }
    throw 'ENOERRORMESSAGE';
}

function do_map(block,list){
    return {
        T: 'do_iterate_map',
        block: block.value || block,
        list: list.value || list,
        phase: 0
    };
}

var p6toplevel = new Scope();
p6toplevel.say = new p6builtin.jssub(say,'say');
p6toplevel.map = new p6builtin.jssub(do_map,'map');
p6toplevel.die = new p6builtin.jssub(do_die,'die');
p6toplevel["Bool::True"] = p6toplevel.True = new p6builtin.Bool(true);
p6toplevel["Bool::False"] = p6toplevel.False = new p6builtin.Bool(false);
var tmp1;
(tmp1 = p6toplevel["Int"] = Derive(p6builtin.Int.prototype)).constructor =
p6builtin.Int; tmp1.isUndefined = true;
(tmp1 = p6toplevel["Num"] = Derive(p6builtin.Num.prototype)).constructor =
p6builtin.Num; tmp1.isUndefined = true;
(tmp1 = p6toplevel["Str"] = Derive(p6builtin.Str.prototype)).constructor =
p6builtin.Str; tmp1.isUndefined = true;
(tmp1 = p6toplevel["Bool"] = Derive(p6builtin.Bool.prototype)).constructor =
p6builtin.Bool; tmp1.isUndefined = true;
(tmp1 = p6toplevel["Sub"] = Derive(p6builtin.Sub.prototype)).constructor =
p6builtin.Sub; tmp1.isUndefined = true;
(tmp1 = p6toplevel["Rat"] = Derive(p6builtin.Rat.prototype)).constructor =
p6builtin.Rat; tmp1.isUndefined = true;


1;


