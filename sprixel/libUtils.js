
/*
  This software is copyright 2009 Matthew Wilson matthew@wilson.org
  and available under the Artistic License 2 ("AL2")
*/

function filt__(str){
    return str.replace(/_/g,'');
}

function say() { // javascript say
    if (typeof(arguments)!='undefined') {
        for (var s_args=[], i=-1, j=-1, a, l=arguments.length; i<l;)
            if (typeof(a=arguments[++i])!='undefined') {
                s_args[++j] = typeof(a)==='string' ? a : a.toString();
            }
        say_them.apply(this.context, s_args);
    } else {
        say_them('');
    }
    this.result = 1;//new p6builtin.Int(1);
}

function do_print() {
    for (var s_args=[], i=-1, j=-1, a, l=arguments.length; i<l;)
        if (typeof(a=arguments[++i])!='undefined') {
            s_args[++j] = typeof(a)==='string' ? a : a.toString();
        }
    if (s_args.length) {
        print_them.apply(this.context, s_args);
    }
    this.result = new p6builtin.Int(1);
}

var Derive = (function(){
    function F(){};
    return function(obj){
        F.prototype = obj;
        return new F();
    };
})();

var DBOX={b:2,B:2,x:16,X:16,o:8,O:8,d:10,D:10};

function F__() { }

var p6toplevel = {};

function Type(obj){
    var type;
    switch(type = typeof obj) {
    case 'object':
        if (obj===null) {
            return 'null';
        }
        if (typeof(obj.length)!='undefined') {
            var res = [];
            for (var i in obj) {
                res.push(Type(obj[i]));
            }
            return '[ '+res.join(', ')+' ]';
        }
        if (typeof(obj.T)!='undefined' && !obj.WHAT) {
            return obj.T;
        }
        return typeof(obj.WHAT)=='function' ? obj.WHAT() : obj.constructor.name;
    default:
        return type;
    }
}

function keys(o) {
    var res = [], j=-1;
    for (var i in o) res[++j] = i;
    return res;
}

/*
function Beget(parent) {
  F__.prototype = parent;
  return new F__();
}

var ignoreDisqualifyingREs = false;

var inConsole;
if (inConsole = typeof(window)=='undefined') {
  window = this; // alias the top-level context
  callTraceNodeAdd = callTraceNodeStatus = function(){};
}
var __slice = Array.prototype.slice;

var printingResult = false;

var warnMode = false;
  //  warnMode = true;
var debugMode = false;
    debugMode = true;
var TestMode = false;
  //  TestMode = true;
var debugTrace = false;
  //  debugTrace = true;
var traceActivations = false;
  //  traceActivations = true;
var traceOn = false;
  //  traceOn = true;
var ConsoleTrace = false;
var stepIntoIE = false;
  //  stepIntoIE = true;
var tempDisableTrace = false;
var emitCoreDump = false;
if (debugMode) {
  warnMode = true;
}

if (debugMode && debugTrace && !stepIntoIE) {
   traceOn = true;
   if (inConsole)
     ConsoleTrace = true;
  //ignoreDisqualifyingREs = true;
    traceActivations = true;
}

var JSMetaPadInputString; // nice name!

var continueCurrentParse = true;
var currentParseCancelled = false;
var parseRunning = false;
var inNestedParse = false;

var Dom = inConsole ? function() {} : function(id) {
  return document.getElementById(id)
}

function F__() { }

function Beget(parent) {
  F__.prototype = parent;
  return new F__();
}

Function.prototype.derivesFrom = function(parent) {
  this.prototype = Beget(parent.prototype);
}

function BegetLink(parent, linkToPrototype) {
  F__.prototype = parent;
  var obj = new F__();
  obj.__prototype__ = parent;
  return obj;
}

var functionNameExtractor = /^[\s\(]*function\s*([^(]*)\([^)]*\)/;

// from https://developer.mozilla.org/En/Core_JavaScript_1.5_Reference/Global_Objects/String/CharCodeAt
var __origCharCodeAt = String.prototype.charCodeAt;

String.prototype.charCodeAt = function(idx) {
  var code = __origCharCodeAt.call(this, idx);
  return 0xD800 <= code && code <= 0xDBFF
    ? (code - 0xD800) * 0x400 + __origCharCodeAt.call(this, idx+1) + 9216
    : 0xDC00 <= code && code <= 0xDFFF
        ? (__origCharCodeAt.call(this, idx-1) - 0xD800) * 0x400 + code + 9216
        : code;
}

function popArgs() { arguments.length--; return arguments }

//Array.prototype.has = function Array_prototype_has(item) {
//  return Array_prototype_indexOf.call(this, item) != -1;
//}

Array.prototype.removeAt = function Array_prototype_removeAt(index) {
  var len;
  return index == 0
    ? (len = this.length) > 1
      ? this.slice(1)
      : []
    : index == (len = this.length) - 1
      ? this
      : index > len - 1
        ? this.slice(0, len - 1)
        : this.slice(0, index).concat(this.slice(index + 1));
}

Array.prototype.isArray = true;

// IE doesn't have Array.prototype.indexOf
// from https://developer.mozilla.org/En/Core_JavaScript_1.5_Reference:Objects:Array:indexOf
// available under MIT license, according to https://developer.mozilla.org/Project:Copyrights
var Array_prototype_indexOf;
if (!Array.prototype.indexOf) {
  Array_prototype_indexOf = Array.prototype.indexOf = function(elt) {
    var len = this.length >>> 0;
    var from = Number(arguments[1]) || 0;
    from = (from < 0)
      ? Math.ceil(from)
      : Math.floor(from);
    if (from < 0) {
      from += len;
    }
    for (; from < len; from++) {
      if (from in this && this[from] == elt) {
        return from;
      }
    }
    return -1;
  };
} else {
  Array_prototype_indexOf = Array.prototype.indexOf
}

var ObjectToString = Object.prototype.toString;
var ArrayToString = Array.prototype.toString;
var ArrayJoin = Array.prototype.join;

String.prototype.times = function String_prototype_times(number) {
  return new Array(number+1).join(this);
}

function rawString(obj) {
  var Class = Object.getClass(obj);
  return typeof(obj)=='undefined' ? 'undefined' : typeof(obj)=='function' ? 'function' : Class!='builtins' ? eval(Class).prototype.toString.call(obj) : Class;
}

Object.Length = function Object_Length() {
  var memberCount = 0;
  for (var i in this) if (this.hasOwnProperty(i)) memberCount++;
  return memberCount;
};
*/
var ToJS = (function(){

var seen, jsi, app_act, this_js_parent, this_js_member, o, r, objs;

function tps(obj) {
    return obj.toString().toProgramString();
}

function jsind(n) {
    return "\n" + Array((jsi += n || 0) + 1).join(' ');
}

function emit_js(self) {
    if (typeof(self)=='function') return 'js_function';
    if (typeof(self)!='object') return tps(self);
    var last_js_parent = this_js_parent, text = '', id;
    if (typeof(id = self.__INTERNAL__TOJS__SEEN)!='undefined') {
        if (seen[id]) { // inside it...
            r[id] = 1;
            app_act += '\no[' + this_js_parent + ']['
                + this_js_member + ']=o[' + id + '];';
            return 'null'; // the circular reference will be injected later
        }
        return 'o[' + id + ']'; // it's a reference to a closed object
    }
    seen[id = self.__INTERNAL__TOJS__SEEN = ++o] = 1;
    objs[id] = self;
    this_js_parent = id;
    if (typeof(self.push)!='undefined') {
        var idx = 0;
        var array_text = '[' + jsind(1);
        for (var i=0; i<self.length; ++i) {
            if (typeof(self[i])=='undefined' || self[i]===null) continue;
            idx++ && (array_text += ',' + jsind());
            this_js_member = idx;
            array_text += emit_js(self[i]);
        }
        var close_indent = jsind(-1);
        text += /^\[\s+$/m.test(array_text)
            ? '[]'
            : array_text + close_indent + ']';
    } else {
        text += "{" + jsind(1);
        var first = 1;
        for (var prop in self) {
            if (typeof(self[prop])=='undefined' || self[prop]===null
                || prop == '__INTERNAL__TOJS__SEEN') continue;
            text += (first ? (first = 0, '') : ',' + jsind())
                + (this_js_member = tps(prop)) + ': ' + emit_js(self[prop]);
        }
        text += jsind(-1) + '}';
    }
    seen[id] = 0; // unmark "inside"
    this_js_parent = last_js_parent; // reset the parent
    return /*r[id] ? */'(o[' + id + ']=' + text + ')'/* : text*/;
}

return function topEmit(obj) {
    seen = [];
    objs = [];
    jsi = 0;
    o = -1;
    r = {};
    app_act = '';
    this_js_parent = '';
    this_js_member = '';
    var out = '(function(){var o={};return '+emit_js(obj)+app_act + '})()';
    for (var i=0; i<objs.length; ++i){ // clean up the markers
        delete objs[i].__INTERNAL__TOJS__SEEN;
    }
    return out;
};

})();

Array.flatten = function() {
  var result = [];
  var len = this.length;
  for (var i = 0; i < len; i++) {
    var el = this[i];
    if (typeof(el)!='undefined' && el.constructor.name=='Array') {
      result = result.concat(Array.flatten.call(el));
    } else if (el !== undefined) {
      result.push(el);
    }
  }
  return result;
};
/*
Array.prototype.flattenOnce = function() {
  var result = [];
  var len = this.length;
  for (var i = 0; i < len; i++) {
    var el = this[i];
    if (el.isArray) {
      result.push.apply(result, el);
    } else {
      result.push(el);
    }
  }
  return result;
};

Array.prototype.max = function() {
  return Math.max.apply(null, this);
}

Array.prototype.min = function() {
  return Math.min.apply(null, this);
}

Number.prototype.abs = function() {
  return Math.abs(this);
}

Number.prototype.ceil = function() {
  return Math.ceil(this);
}

Number.prototype.floor = function() {
  return Math.floor(this);
}

Number.prototype.pow = function(power) {
  return Math.pow(this, power);
}

function randInt(max) { // random distribution along 0..max (default 1)
  return Math.floor(Math.random() * ((max || 1) + 1));
}

function rand(max) { // random distribution along 0..max (default 1)
  return Math.random() * (max || 1);
}

var toArgArray = Array.prototype.toArgArray = function toArgArray(obj) {
  obj = typeof(obj)!='undefined' ? obj : this;
  var res, i, len = obj.length, results = new Array(len);
  for (i = 0; i < len; i++) {
    results[i] = typeof(res = obj[i])!='undefined'
        ? res.toProgramString
            ? res.toProgramString() : res.toString() : null;
  }
  return len > 0
    ? '( ' + ArrayJoin.call(results, ', ') + ' )'
    : '()';
}

Array.prototype.sortNum = function() {
   return this.sort( function (a,b) { return a-b; } );
}

Array.prototype.shuffle = function(){
  var v = this.slice(0);
  for(var j, x, i = v.length; i;
    j = parseInt(Math.random() * i), x = v[--i], v[i] = v[j], v[j] = x);
  return v;
};

Array.prototype.sum = function(){
  for(var s = 0, i = this.length; i; s += this[--i]);
  return s;
};

Number.prototype.randInt = function() {
  return randInt(this);
}

var __HexChars =
    ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'];

var __HexChar = function __HexChar() {
  return __HexChars[randInt(15)];
}

Function.prototype.times = function Function_times(number) {
  var res = new Array(number);
  while (number--) {
    res[number] = this();
  }
  return ArrayJoin.call(res, '');
}

Number.prototype.rand = function() {
  return rand(this);
}

Object.getClass = function getClass(object) {
  return ObjectToString.call(object).slice(8, -1);
};

Array.prototype.valuesOfAtPositions = function valuesOfAtPositions() {
  var positions = __slice.call(arguments, 0);
  var result = this.slice(0); // copy the original
  var offset = 0;
  var item, head, index;
  for (var i = 0; i < positions.length; i++) {
    if (typeof((item = result[index = positions[i]+offset]).isArray)!='undefined') {
      offset += item.length - 1;
      result = result.slice(0,index).concat(item).concat(result.slice(index+1));
    }
  }
  return result;
}

// http://stevenlevithan.com/regex/xregexp/
// Returns an array with the elements of an existng array for which the provided filtering function returns true
if (!Array.prototype.filter) Array.prototype.filter = function (fn, context) {
	var results = [], k, j = 0;
	for (var i = 0, len = this.length; i < len; i++) {
		if (fn.call(context, (k = this[i]), i, this))
			results[j++] = k;
	}
	return results;
};

// http://stevenlevithan.com/regex/xregexp/
// Returns true if every element in the array satisfies the provided testing function
if (!Array.prototype.every) Array.prototype.every = function (fn, context) {
  var k;
	for (var i = 0, len = this.length; i < len; i++) {
		if (!fn.apply(context, [this[i], i, this]))
			return false;
	}
	return true;
};

//+ Jonas Raoni Soares Silva
//@ http://jsfromhell.com/classes/overloader [rev. #2]

Overloader = function(){
    var f = function(args){
        var i, l, h = "", empty = {};
        for(i = -1, l = (args = [].slice.call(arguments)).length; ++i < l;)
            args[i] !== undefined && args[i] !== null ? h += args[i].constructor : empty[i] = 1;
        if(!(h = f._methods[h])){
            var x, j, k, m = -1;
            for(i in f._methods){
                for(k = 0, j = -1, l = Math.max(args.length, x = f._methods[i][1]); ++j < l;
                    !empty[j] && (args[j] instanceof x[j] || args[j].constructor == x[j]) && ++k);
                k > m && (h = f._methods[i], m = k);
            }
        }
        return h ? h[0].apply(f, args) : undefined;
    };
    f._methods = {};
    f.overload = function(f, args){
        this._methods[(args = [].slice.call(arguments, 1)).join("")] = [f, args];
    };
    f.unoverload = function(args){
        return delete this._methods[[].slice.call(arguments).join("")];
    };
    return f;
};

//+ Jonas Raoni Soares Silva
//@ http://jsfromhell.com/classes/binary-parser [rev. #1]

BinaryParser = function(bigEndian, allowExceptions){
    this.bigEndian = bigEndian, this.allowExceptions = allowExceptions;
};
with({p: BinaryParser.prototype}){
    p.encodeFloat = function(number, precisionBits, exponentBits){
        var bias = Math.pow(2, exponentBits - 1) - 1, minExp = -bias + 1, maxExp = bias, minUnnormExp = minExp - precisionBits,
        status = isNaN(n = parseFloat(number)) || n == -Infinity || n == +Infinity ? n : 0,
        exp = 0, len = 2 * bias + 1 + precisionBits + 3, bin = new Array(len),
        signal = (n = status !== 0 ? 0 : n) < 0, n = Math.abs(n), intPart = Math.floor(n), floatPart = n - intPart,
        i, lastBit, rounded, j, result;
        for(i = len; i; bin[--i] = 0);
        for(i = bias + 2; intPart && i; bin[--i] = intPart % 2, intPart = Math.floor(intPart / 2));
        for(i = bias + 1; floatPart > 0 && i; (bin[++i] = ((floatPart *= 2) >= 1) - 0) && --floatPart);
        for(i = -1; ++i < len && !bin[i];);
        if(bin[(lastBit = precisionBits - 1 + (i = (exp = bias + 1 - i) >= minExp && exp <= maxExp ? i + 1 : bias + 1 - (exp = minExp - 1))) + 1]){
            if(!(rounded = bin[lastBit]))
                for(j = lastBit + 2; !rounded && j < len; rounded = bin[j++]);
            for(j = lastBit + 1; rounded && --j >= 0; (bin[j] = !bin[j] - 0) && (rounded = 0));
        }
        for(i = i - 2 < 0 ? -1 : i - 3; ++i < len && !bin[i];);

        (exp = bias + 1 - i) >= minExp && exp <= maxExp ? ++i : exp < minExp &&
            (exp != bias + 1 - len && exp < minUnnormExp && this.warn("encodeFloat::float underflow"), i = bias + 1 - (exp = minExp - 1));
        (intPart || status !== 0) && (this.warn(intPart ? "encodeFloat::float overflow" : "encodeFloat::" + status),
            exp = maxExp + 1, i = bias + 2, status == -Infinity ? signal = 1 : isNaN(status) && (bin[i] = 1));
        for(n = Math.abs(exp + bias), j = exponentBits + 1, result = ""; --j; result = (n % 2) + result, n = n >>= 1);
        for(n = 0, j = 0, i = (result = (signal ? "1" : "0") + result + bin.slice(i, i + precisionBits).join("")).length, r = [];
            i; n += (1 << j) * result.charAt(--i), j == 7 && (r[r.length] = String.fromCharCode(n), n = 0), j = (j + 1) % 8);
        r[r.length] = n ? String.fromCharCode(n) : "";
        return (this.bigEndian ? r.reverse() : r).join("");
    };
    p.encodeInt = function(number, bits, signed){
        var max = Math.pow(2, bits), r = [];
        (number >= max || number < -(max >> 1)) && this.warn("encodeInt::overflow") && (number = 0);
        number < 0 && (number += max);
        for(; number; r[r.length] = String.fromCharCode(number % 256), number = Math.floor(number / 256));
        for(bits = -(-bits >> 3) - r.length; bits--; r[r.length] = "\0");
        return (this.bigEndian ? r.reverse() : r).join("");
    };
    p.decodeFloat = function(data, precisionBits, exponentBits){
        var b = ((b = new this.Buffer(this.bigEndian, data)).checkBuffer(precisionBits + exponentBits + 1), b),
            bias = Math.pow(2, exponentBits - 1) - 1, signal = b.readBits(precisionBits + exponentBits, 1),
            exponent = b.readBits(precisionBits, exponentBits), significand = 0,
            divisor = 2, curByte = b.buffer.length + (-precisionBits >> 3) - 1,
            byteValue, startBit, mask;
        do
            for(byteValue = b.buffer[ ++curByte ], startBit = precisionBits % 8 || 8, mask = 1 << startBit;
                mask >>= 1; (byteValue & mask) && (significand += 1 / divisor), divisor *= 2);
        while(precisionBits -= startBit);
        return exponent == (bias << 1) + 1 ? significand ? NaN : signal ? -Infinity : +Infinity
            : (1 + signal * -2) * (exponent || significand ? !exponent ? Math.pow(2, -bias + 1) * significand
            : Math.pow(2, exponent - bias) * (1 + significand) : 0);
    };
    p.decodeInt = function(data, bits, signed){
        var b = new this.Buffer(this.bigEndian, data), x = b.readBits(0, bits), max = Math.pow(2, bits);
        return signed && x >= max / 2 ? x - max : x;
    };
    with({p: (p.Buffer = function(bigEndian, buffer){
        this.bigEndian = bigEndian || 0, this.buffer = [], this.setBuffer(buffer);
    }).prototype}){
        p.readBits = function(start, length){
            //shl fix: Henri Torgemane ~1996 (compressed by Jonas Raoni)
            function shl(a, b){
                for(++b; --b; a = ((a %= 0x7fffffff + 1) & 0x40000000) == 0x40000000 ? a * 2 : (a - 0x40000000) * 2 + 0x7fffffff + 1);
                return a;
            }
            if(start < 0 || length <= 0)
                return 0;
            this.checkBuffer(start + length);
            for(var offsetLeft, offsetRight = start % 8, curByte = this.buffer.length - (start >> 3) - 1,
                lastByte = this.buffer.length + (-(start + length) >> 3), diff = curByte - lastByte,
                sum = ((this.buffer[ curByte ] >> offsetRight) & ((1 << (diff ? 8 - offsetRight : length)) - 1))
                + (diff && (offsetLeft = (start + length) % 8) ? (this.buffer[ lastByte++ ] & ((1 << offsetLeft) - 1))
                << (diff-- << 3) - offsetRight : 0); diff; sum += shl(this.buffer[ lastByte++ ], (diff-- << 3) - offsetRight)
            );
            return sum;
        };
        p.setBuffer = function(data){
            if(data){
                for(var l, i = l = data.length, b = this.buffer = new Array(l); i; b[l - i] = data.charCodeAt(--i));
                this.bigEndian && b.reverse();
            }
        };
        p.hasNeededBits = function(neededBits){
            return this.buffer.length >= -(-neededBits >> 3);
        };
        p.checkBuffer = function(neededBits){
            if(!this.hasNeededBits(neededBits))
                throw new Error("checkBuffer::missing bytes");
        };
    }
    p.warn = function(msg){
        if(this.allowExceptions)
            throw new Error(msg);
        return 1;
    };
    p.toSmall = function(data){return this.decodeInt(data, 8, true);};
    p.fromSmall = function(number){return this.encodeInt(number, 8, true);};
    p.toByte = function(data){return this.decodeInt(data, 8, false);};
    p.fromByte = function(number){return this.encodeInt(number, 8, false);};
    p.toShort = function(data){return this.decodeInt(data, 16, true);};
    p.fromShort = function(number){return this.encodeInt(number, 16, true);};
    p.toWord = function(data){return this.decodeInt(data, 16, false);};
    p.fromWord = function(number){return this.encodeInt(number, 16, false);};
    p.toInt = function(data){return this.decodeInt(data, 32, true);};
    p.fromInt = function(number){return this.encodeInt(number, 32, true);};
    p.toDWord = function(data){return this.decodeInt(data, 32, false);};
    p.fromDWord = function(number){return this.encodeInt(number, 32, false);};
    p.toFloat = function(data){return this.decodeFloat(data, 23, 8);};
    p.fromFloat = function(number){return this.encodeFloat(number, 23, 8);};
    p.toDouble = function(data){return this.decodeFloat(data, 52, 11);};
    p.fromDouble = function(number){return this.encodeFloat(number, 52, 11);};
}

//+ Jonas Raoni Soares Silva
Array.prototype.getElementsAt = function(level){ 
  var r = [], x = [], c = this, l = level, i; 
  do 
    if(i = c.length, !l) 
      for(; i; r[r.length] = c[--i]); 
    else 
      for(; i; c[--i] instanceof Array && x.push(c[i], l - 1)); 
  while(l = x.pop(), (c = x.pop()) != undefined); 
  return r; 
} 

//+ Jonas Raoni Soares Silva
//@ http://jsfromhell.com/geral/utf-8 [rev. #1]

var UTF8 = {
    encode: function(s){
        for(var c, i = -1, l = (s = s.split("")).length, o = String.fromCharCode; ++i < l;
            s[i] = (c = s[i].charCodeAt(0)) >= 127 ? o(0xc0 | (c >>> 6)) + o(0x80 | (c & 0x3f)) : s[i]
        );
        return s.join("");
    },
    decode: function(s){
        for(var a, b, i = -1, l = (s = s.split("")).length, o = String.fromCharCode, c = "charCodeAt"; ++i < l;
            ((a = s[i][c](0)) & 0x80) &&
            (s[i] = (a & 0xfc) == 0xc0 && ((b = s[i + 1][c](0)) & 0xc0) == 0x80 ?
            o(((a & 0x03) << 6) + (b & 0x3f)) : o(128), s[++i] = "")
        );
        return s.join("");
    }
};

//+ Jonas Raoni Soares Silva
//@ http://jsfromhell.com/geral/equals [rev. #1]

var equals = function(a, b){
    for(var j, o = arguments, i = o.length, c = a instanceof Object; --i;)
        if(a === (b = o[i]))
            continue;
        else if(!c || !(b instanceof Object))
            return false;
        else for(j in b)
            if(!equals(a[j], b[j]))
                return false;
    return true;
};

//+ Carlos R. L. Rodrigues
//@ http://jsfromhell.com/number/zero-format [rev. #1]

Number.prototype.zeroFormat = function(n, f, r){
    return n = new Array((++n, f ? (f = (this + "").length) < n
        ? n - f : 0 : n)).join(0), r ? this + n : n + this;
};

//+ Carlos R. L. Rodrigues
//@ http://jsfromhell.com/number/base-conversor [rev. #1]

Number.prototype.toBase = function(b, c){
    var s = "", n = this;
    if(b > (c = (c ||
        "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    ).split("")).length || b < 2) return "";
    while(n)
        s = c[n % b] + s, n = Math.floor(n / b);
    return s;
};

String.prototype.chop = function(numberToChop) {
  return this.split('').slice(0,this.length-(numberToChop||1)).join('');
}

String.prototype.chip = function() {
  return this.split('').slice(1,this.length-1).join('');
}

String.prototype.parseInt = function(b, c){
    var s = 0, n, l = (n = this.split("")).length, i = 0;
    if(b > (c = c || "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz").length || b < 2) return NaN;
    while(l--)
        s += c.indexOf(n[i++]) * Math.pow(b, l);
    return s;
};

//+ Jonas Raoni Soares Silva
//@ http://jsfromhell.com/string/pad [rev. #1]

String.prototype.pad = function(l, s, t){
    return s || (s = " "), (l -= this.length) > 0 ? (s = new Array(Math.ceil(l / s.length)
        + 1).join(s)).substr(0, t = !t ? l : t == 1 ? 0 : Math.ceil(l / 2))
        + this + s.substr(0, l - t) : this;
};

//+ Carlos R. L. Rodrigues
//@ http://jsfromhell.com/string/mask [rev. #1]

String.prototype.mask = function(m) {
    var m, l = (m = m.split("")).length, s = this.split(""), j = 0, h = "";
    for(var i = -1; ++i < l;)
        if(m[i] != "#"){
            if(m[i] == "\\" && (h += m[++i])) continue;
            h += m[i];
            i + 1 == l && (s[j - 1] += h, h = "");
        }
        else{
            if(!s[j] && !(h = "")) break;
            (s[j] = h + s[j++]) && (h = "");
        }
    return s.join("") + h;
};

//+ Jonas Raoni Soares Silva
//@ http://jsfromhell.com/array/rotate [rev. #2]

Array.prototype.rotate = function(p){
    var a = this.slice(0);
    for(var l = a.length, p = (Math.abs(p) >= l && (p %= l), p < 0 && (p += l), p), i, x; p; p = (Math.ceil(l / p) - 1) * p - l + (l = p))
        for(i = l; i > p; x = a[--i], a[i] = a[i - p], a[i - p] = x);
    return a;
};

//+ Carlos R. L. Rodrigues
//@ http://jsfromhell.com/array/remove-duplicated [rev. #2]

Array.prototype.distinct = function(s){
    var a = this.slice(0);
    var p, i, j;
    if(s) for(i = a.length; i > 1;){
        if(a[--i] === a[i - 1]){
            for(p = i - 1; p-- && a[i] === a[p];);
            i -= a.splice(p + 1, i - p - 1).length;
        }
    }
    else for(i = a.length; i;){
        for(p = --i; p > 0;)
            if(a[i] === a[--p]){
                for(j = p; --p && a[i] === a[p];);
                i -= a.splice(p + 1, j - p).length;
            }
    }
    return a;
};
*/
/*
  Copyright (c) 2007, 2008 Alessandro Warth <awarth@cs.ucla.edu>

  Permission is hereby granted, free of charge, to any person
  obtaining a copy of this software and associated documentation
  files (the "Software"), to deal in the Software without
  restriction, including without limitation the rights to use,
  copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following
  conditions:

  The above copyright notice and this permission notice shall be
  included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
  OTHER DEALINGS IN THE SOFTWARE.
*/
/*
Array.prototype.map = function(f) {
  var r = []
  for (var idx = 0; idx < this.length; idx++)
    r[idx] = f(this[idx])
  return r
}

Array.prototype.reduce = function(f, z) {
  var r = z
  for (var idx = 0; idx < this.length; idx++)
    r = f(r, this[idx])
  return r
}
*/
var escapeStringFor = new Object()
for (var c = 0; c < 256; c++)
  escapeStringFor[c] = String.fromCharCode(c)
escapeStringFor["\\".charCodeAt(0)] = "\\\\"
escapeStringFor['"'.charCodeAt(0)]  = '\\"'
escapeStringFor["'".charCodeAt(0)]  = "\\'"
escapeStringFor["\r".charCodeAt(0)] = "\\r"
escapeStringFor["\n".charCodeAt(0)] = "\\n"
escapeStringFor["\t".charCodeAt(0)] = "\\t"
escapeStringFor["\f".charCodeAt(0)] = "\\f"
escapeChar = function(c) {
  //var charCode = c.charCodeAt(0)
  return c > 255 ? String.fromCharCode(c) : escapeStringFor[c]
}

function unescapeString(s) {
  if (s[0] == '\\')
    switch (s[1]) {
      case '\\': return '\\'
      case 'r':  return '\r'
      case 'n':  return '\n'
      case 't':  return '\t'
      case '\'':  return '\''
      case '"':  return '\"'
      default:   return s[1]
    }
  else
    return s
}

//String.prototype.programified = false;

String.prototype.toProgramString = function String_prototype_toProgramString() {
  //if (this.programified) return this;
  var ws = ["\""], c;
  for (var idx = 0, len = this.length; idx < len; idx++) {
    ws[idx+1] = ((c = this.charCodeAt(idx)) > 255 ? String.fromCharCode(c) : escapeStringFor[c]);
  }
  ws[idx+1] = "\"";
  var result = ws.join('');
  //result.programified = true;
  return result;
}
/*
// C-style tempnam function

function tempnam(s) { return (s ? s : "_tmpnam_") + tempnam.n++ }
tempnam.n = 0

function undefer(obj, context, args) {
  if (typeof(obj)=='object' && obj.isArray) {
    return obj.undefer(context);
  } else if (typeof(context)!='undefined' && context instanceof Func.Act && typeof(obj)=='function') {
    obj.apply(context, args || []);
    return context.result;
  } else {
    while (typeof(obj)=='function') obj = obj.apply(context || null, args || []);
    return obj;
  }
}

Array.prototype.undefer = function(context, args) {
  var res = new Array(this.length);
  for (var i = 0, len = this.length; i < len; i++) {
    res[i] = undefer(this[i], context, args);
  }
  return res;
}

String.prototype.withScope = function(context, uniqueString) {
  var newString = (typeof(uniqueString)=='undefined' || uniqueString) // default true
    ? new String(this + ' ' + rand())
    : new String(this);
  newString.myScope = context;
  newString.str = this;
  return newString;
}

function spawnOf(parent) {
  // obtain a reference to the function that created the object
  var Ctor = typeof(parent.constructor)!='undefined'
    ? parent.constructor
    : getGlobal()[ObjectToString.call(parent).match(/^\[object\s(.*)\]$/)[1]];
  // the below shouldn't ever happen (except on undefined or null)
  //if (typeof Ctor != 'function') {
  //  throw 'constructor not found';
  //}
  // save the original prototype of the constructor function
  var origProto = Ctor.prototype;
  var parentRef = parent;
  // replace the constructor function's prototype with the parent object
  Ctor.prototype = parentRef;
  //if (Ctor.prototype!==parent) {
  //  throw 'broken!';
  //}
  // create a new "child" object, using the same constructor function as the parent object.
  var childObject = new Ctor(true);
  // replace the constructor function's prototype.
  Ctor.prototype = origProto;
  return childObject;
}

// from http://www.nczonline.net/blog/2008/04/20/get-the-javascript-global/
// by Nicholas C. Zakas
function getGlobal() {
  return (function() {
    return this;
  }).call(null);
}
*/
1;







