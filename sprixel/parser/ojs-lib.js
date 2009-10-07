/*
  This software is copyright 2009 Matthew Wilson matthew@wilson.org
  and available under the Artistic License 2 ("AL2") as documented at
  http://jsmeta.googlecode.com
*/

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
function fixedCharCodeAt(str, idx) {
  var code = str.charCodeAt(idx);
  if (0xD800 <= code && code <= 0xDBFF) {
    // High surrogate (could change last hex to 0xDB7F to treat high private surrogates as single characters)
    var hi = code;
    var low = str.charCodeAt(idx+1);
    return ((hi - 0xD800) * 0x400) + (low - 0xDC00) + 0x10000;
  }
  if (0xDC00 <= code && code <= 0xDFFF) { // Low surrogate
    var hi = str.charCodeAt(idx-1);
    var low = code;
    return ((hi - 0xD800) * 0x400) + (low - 0xDC00) + 0x10000;
  }
  return code;
}

function popArgs() { arguments.length--; return arguments; }

Array.prototype.has = function Array_prototype_has(item) {
  return Array_prototype_indexOf.call(this, item) !== -1;
}

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
if (!Array.prototype.indexOf) {
  Array.prototype.indexOf = function Array_prototype_indexOf(elt /*, from*/) {
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
  var Array_prototype_indexOf = Array.prototype.indexOf
}

var ObjectToString = Object.prototype.toString;
var ArrayToString = Array.prototype.toString;
var ArrayJoin = Array.prototype.join;
/*
Array.prototype.Join = function Join() {
  if (typeof(this)=='object') {
    if (this.toProgramString) return this.toProgramString();
    if (typeof(this.length)=='undefined') return this.toString();
  }
  var len;
  var results = new Array(len = this.length);
  for (var i = 0; i < len; i++) {
    results[i] = Object_prototype_toString.call(this[i]);
  }
  return Array_prototype_join.call(results, ', ');
}
var ArrayJoin = function(joiner) {
  return Array_prototype_join.call(this, typeof(joiner)!='undefined' ? joiner : ', ');
}
*/

String.prototype.times = function String_prototype_times(number) {
  return new Array(number+1).join(this);
}

function inspect(obj) {
  var results = [], i = 0;
  if (typeof(obj)=='function') {
    return obj.toString();
    //.match(/^[\s\(]*function\s*([^(]*)\([^)]*\)/)[1]
  }
  for (var property in obj) {
    results[i] = i++ ? ' ' + property + ': ' + obj[property] : '{ ' + property + ': ' + obj[property];
  }
  results[i-1] += ' }';
  return i ? ArrayJoin.call(results, ', ') : '{}';
}

function inspectShallow(obj) {
  var results = [], i = 0;
  if (typeof(obj)=='function') {
    return 'function '+obj.name+'()';
  }
  for (var property in obj) {
    results[i] = i++ ? '\n\t' + property + ': ' + rawString(obj[property]) : '{\n\t' + property + ': ' + rawString(obj[property]);
  }
  results[i-1] += '\n}';
  return i ? ArrayJoin.call(results, ', ') : '{}';
}

function rawString(obj) {
  var Class = Object.getClass(obj);
  return typeof(obj)=='undefined' ? 'undefined' : typeof(obj)=='function' ? 'function' : Class!='builtins' ? eval(Class).prototype.toString.call(obj) : Class;
}

var Object_prototype_toString = Object.prototype.toString = function Object_prototype_toString() {
  var len, i;
  //if (printingResult) {
  //  Print(Object.getClass(this));
  //}
  if (this.toProgramString) {
    return this.toProgramString();
  } else if (typeof(len = this.length)!='undefined') {
    var results = new Array(len);
    var res;
    for (i = 0; i < len; i++) {
      results[i] = typeof(res = this[i])!='undefined' ? res.toProgramString ? res.toProgramString() : res.toString() : null;
    }
    return len > 0
      ? '[ ' + ArrayJoin.call(results, ', ') + ' ]'
      : '[]';
  } else if (typeof(this)!='object') {
    return this.toString();
  }
  var results = [];
  i = 0;
  var item;
  for (var property in this) {
    if ((item = this[property])!==Object_prototype_toString) {
      results[i] = i++ ? ' ' + property + ': ' + Object_prototype_toString.call(item) : '{ ' + property + ': ' + Object_prototype_toString.call(item);
    }
  }
  results[i-1] += ' }';
  return i ? ArrayJoin.call(results, ', ') : '{}';
}

Object.Length = function Object_Length() {
  var memberCount = 0;
  for (var i in this) if (this.hasOwnProperty(i)) memberCount++;
  return memberCount;
};

/*
var iDepth = 0;
var indent = '  ';

var objsPrinting = [];

Object.prototype.toString = function Object_prototype_toString() {
  var isRootPrinter = false;
  if (objsPrinting.length) {
    if (objsPrinting.has(this)) {
      return '_#SHARP#_';
    } else {
      objsPrinting.push(this);
    }
  } else {
    isRootPrinter = true;
    objsPrinting.push(this);
  }
  if (typeof(this.length)!='undefined') {
    var result = this.length
      ? '( ' + ArrayJoin.call(this, ', ') + ' )'
      : '()';
    if (isRootPrinter) {
      objsPrinting = [];
    }
    return result;
  }
  var results = [], i = 0;
  iDepth++;
  for (var property in this) {
    if (this[property]!==Object.prototype.toString) {
      results[i] = i++ ? '\n' + indent.times(iDepth) + property + ': ' + this[property] : '{\n' + indent.times(iDepth) + property + ': ' + this[property];
    }
  }
  iDepth--;
  results[i-1] += '\n' + indent.times(iDepth) + '}';
  if (isRootPrinter) {
    objsPrinting = [];
  }
  return i ? ArrayJoin.call(results, ', ') : '{}';
}
*/
Array.prototype.toString = function Array_prototype_toString() {
  return '[' + ArrayJoin.call(this, ', ') + ']';
}

Array.prototype.flatten = function(includeUndefined) {
  var result = [];
  var len = this.length;
  for (var i = 0; i < len; i++) {
    var el = this[i];
    if (typeof(el)!='undefined' && el.isArray) {
      result = result.concat(el.flatten(includeUndefined));
    } else if (includeUndefined || el !== undefined) {
      result.push(el);
    }
  }
  return result;
};

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
    results[i] = typeof(res = obj[i])!='undefined' ? res.toProgramString ? res.toProgramString() : res.toString() : null;
  }
  return len > 0
    ? '( ' + ArrayJoin.call(results, ', ') + ' )'
    : '()';
}

Array.prototype.sortNum = function() {
   return this.sort( function (a,b) { return a-b; } );
}

function selectNode (node) {
   var selection, range, doc, win;
   if ((doc = node.ownerDocument) && (win = doc.defaultView) && typeof
win.getSelection != 'undefined' && typeof doc.createRange != 'undefined'
&& (selection = window.getSelection()) && typeof
selection.removeAllRanges != 'undefined') {
     range = doc.createRange();
     range.selectNode(node);
     selection.removeAllRanges();
     selection.addRange(range);
   }
   else if (document.body && typeof document.body.createTextRange !=
'undefined' && (range = document.body.createTextRange())) {
     range.moveToElementText(node);
     range.select();
   }
}

Array.prototype.shuffle = function(){
  var v = this.slice(0);
  for(var j, x, i = v.length; i; j = parseInt(Math.random() * i), x = v[--i], v[i] = v[j], v[j] = x);
  return v;
};

Array.prototype.sum = function(){
  for(var s = 0, i = this.length; i; s += this[--i]);
  return s;
};

Number.prototype.randInt = function() {
  return randInt(this);
}

var __HexChars = ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'];

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
Array.prototype.filter = function (fn, context) {
	var results = [], k, j = 0;
	for (var i = 0, len = this.length; i < len; i++) {
		if (fn.call(context, (k = this[i]), i, this))
			results[j++] = k;
	}
	return results;
};

// http://stevenlevithan.com/regex/xregexp/
// Returns true if every element in the array satisfies the provided testing function
Array.prototype.every = function (fn, context) {
  var k;
	for (var i = 0, len = this.length; i < len; i++) {
		if (!fn.apply(context, [this[i], i, this]))
			return false;
	}
	return true;
};

//+ Jonas Raoni Soares Silva
//@ http://jsfromhell.com/classes/overloader [rev. #2]
/*
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
//@ http://jsfromhell.com/classes/timeline [rev. #0]

TimeLine = function(fps, f){
    this.fps = fps, this.frames = f;
};
with({o: TimeLine, $: TimeLine.prototype}){
    o.timers = [];
    $.running = !!($.current = +(o.timer = $.time = null));
    o.run = function(){
        var o = this;
        o.timer || (o.timer = setInterval(function(){
            for(var h, d = +(new Date), t = o.timers, i = t.length; i--;){
                (!t[i].running || ((d - t[i].time) / (1e3 / t[i].fps) > t[i].current + 1 &&
                t[i].onframe(++t[i].current), t[i].current >= t[i].frames)) &&
                (h = t.splice(i, 1)[0], h.stop(1));
            }
        }, 1));
    };
    $.start = function(c){
        var o = this, t = TimeLine;
        if(o.running) return;
        o.running = true, o.current = c || 0;
        o.time = new Date, o.onstart && o.onstart();
        if(!o.onframe || o.frames <= 0 || o.fps <= 0)
            return o.stop(1);
        t.timers.push(this), t.run();
    };
    $.stop = function(){
        var o = this;
        o.running = false;
        if(!TimeLine.timers.length)
            TimeLine.timer = clearInterval(TimeLine.timer), null;
        arguments.length && o.onstop && o.onstop();
    };
}
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

//+ Carlos R. L. Rodrigues
//@ http://jsfromhell.com/classes/template [rev. #2]

Template = function(s, oo, oc, co, cc){
    var o = this; o.oo = oo || "<js:", o.oc = oc || ">", o.co = co || "</js:", o.cc = cc || ">";
    if(o.oo == o.co || o.oo == o.oc || o.oo == o.cc || o.co == o.oc || o.co == o.cc) throw Error("Tag definition not allowed");
    o.tags = [], o.root = o.node("_root", s, null), o.parse(s);
}
Template.prototype.node = function(n, v, p){
    var _ = this;
    return {
        _default: v, _value: [v], _parent: p, _children: [], _name: n,
        _render: function(_o){
            var o = this._children, s = (v = this._value)[v.length - 1], p = [], i = -1,
            t = _.oo, c = _.co, tc = _.oc, cc = _.cc, y, v, a, f, j, to;
            while((y = o[++i]) && (v = s.slice(0, j = s.indexOf(to = t + (a = y._name) + tc)), f = s.indexOf(a = c + a + cc, j))){
                while((j = s.indexOf(to, j + 1)) + 1 && j < f) f = s.indexOf(a, f + 1);
                if(f + 1 && (s = v + y._value.join("") + s.slice(a.length + f)), y._children[0]) p.push(i, o), o = y._children, i = -1;
                else if(!o[i + 1]) while(p.length && !(o = p.pop())[(i = p.pop()) + 1]);
            }
            return _o ? s : (i = (v = this._value).length, v[i] = v[--i], v[i] = s);
        },
        _set: function(s){return this._value[this._value.length - 1] = s;},
        _get: function(){
            var r = /([(){}|*+?.,^$\[\]\\])/g, f = function(s){return s.replace(r, "\\\$1")};
            return this._value.join("").replace(new RegExp((f(_.oo) + ".*?" + f(_.oc) + "|" + f(_.co) + ".*?" + f(_.cc)), "gm"), "");
        },
        _reset: function(c){
            if(c)
                for(var a, o = this, t = _.tags, i = 0, l = t.length; i < l; i++)
                    ((a = t[i]._parent) == o || a == this) && ((o = t[i])._value = [o._default]);
            return this._value = [this._default];
        },
        _output: function(){return this._render(1);}
    };
}
Template.prototype.parse = function(s){
    var _ = this, p = 0, r = _.root, l = [[-1, s.length, r]], y = _.tags, i = _.oo,
    e = _.co, $ = _.oc, d = _.cc, h = $.length, g, a, f, j, c, t, v;
    while((p = s.indexOf(i, p)) > -1){
        if(p == s.indexOf(e, p) && ++p) continue;
        a = (a = p + i.length) + (t = s.slice(a, j = s.indexOf($, p))).length + h, f = s.indexOf(g = e + t + d, p);
        while((j = s.indexOf(i + t + d, j + 1)) + 1 && j < f) f = s.indexOf(g, f + 1);
        if(t.charAt() == "_") throw Error("Tag name not allowed [" + t + "]");
        if(f < 0) throw Error("End of tag \"" + i + t + $ + "\" expected");
        for(v = s.slice(a, f), j = l.length; j--;)
            if((c = l[j])[2][t] && p > c[0] && f < c[1]) throw Error("Ambiguous tag name \"" + t + "\"");
            else if(p > c[0] && f < c[1] && l.push([p++, f, (v = c[2][t] = _.node(t, v, c[2]))]) &&
                (!(a = c[2]._parent) ? c[2] : a[c[2]._name])._children.push(v) && y.push(v)) break;
    }
}*/
//+ Carlos R. L. Rodrigues
//@ http://jsfromhell.com/geral/event-listener [rev. #5]
/*
addEvent = function(o, e, f, s){
    var r = o[r = "_" + (e = "on" + e)] = o[r] || (o[e] ? [[o[e], o]] : []), a, c, d;
    r[r.length] = [f, s || o], o[e] = function(e){
        try{
            (e = e || event).preventDefault || (e.preventDefault = function(){e.returnValue = false;});
            e.stopPropagation || (e.stopPropagation = function(){e.cancelBubble = true;});
            e.target || (e.target = e.srcElement || null);
            e.key = (e.which + 1 || e.keyCode + 1) - 1 || 0;
        }catch(f){}
        for(d = 1, f = r.length; f; r[--f] && (a = r[f][0], o = r[f][1], a.call ? c = a.call(o, e) : (o._ = a, c = o._(e), o._ = null), d &= c !== false));
        return e = null, !!d;
    }
};

removeEvent = function(o, e, f, s){
    for(var i = (e = o["_on" + e] || []).length; i;)
        if(e[--i] && e[i][0] == f && (s || o) == e[i][1])
            return delete e[i];
    return false;
};
*/
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

/*
//+ Jonas Raoni Soares Silva
//@ http://jsfromhell.com/dhtml/drag-library [rev. #2]

Dragger = function(o, a){
    var $ = this;
    o.style.position = "absolute", $.object = o, $.d = {x: 0, y: 0}, $.f = [];
    a && (addEvent(o, "mousedown", function(){return this.start(), false;}, $),
        addEvent(document, "mouseup", function(){this.dragging && this.stop();}, $));
}
with({p: Dragger.prototype, c: Dragger}){
    p._updateMouse = function(e){
        var w = window, b = document.body;
        p.mouse = {x: e.clientX + (w.scrollX || b.scrollLeft || b.parentNode.scrollLeft || 0),
            y: e.clientY + (w.scrollY || b.scrollTop || b.parentNode.scrollTop || 0)};
    };
    addEvent(document, "mousemove", p._updateMouse);
    p.mouse = {x: 0, y: 0};
    p.dragging = false;
    p.start = function(center){
        var r, $ = this, m = $.mouse, o = $.object;
        for(var r = {l: o.offsetLeft, t: o.offsetTop, w: o.offsetWidth, h: o.offsetHeight};
            o = o.offsetParent; r.l += o.offsetLeft, r.t += o.offsetTop);
        !$.dragging && ($.dragging = true, o = $.object, $.d = center &&
            (m.x < r.l || m.x > r.l + r.w || m.y < r.t || m.y > r.t + r.h) ?
            {x: r.w / 2, y: r.h / 2} : {x: m.x - o.offsetLeft, y: m.y - o.offsetTop},
            addEvent(document, "mousemove", $.drag, $),
            this.callEvent("onstart"));
    };
    p.drag = function(e){
        var i, p, $ = this, o = $.object, m = ($._updateMouse(e), (m = $.mouse).x -= $.d.x, m.y -= $.d.y, m);
        for(i = $.f.length; i; $.f[--i] && $.f[i][0].apply(m, $.f[i][1]));
        o.style.left = m.x + "px", o.style.top = m.y + "px";
        return !!this.callEvent("ondrag", e);
    };
    p.stop = function(){
        this.dragging = false;
        removeEvent(document, "mousemove", this.drag, this);
        this.callEvent("onstop");
    };
    p.addFilter = function(f, arg0, arg1, arg2, argN){
        this.f[this.f.length] = [f, [].slice.call(arguments, 1)];
    };
    p.callEvent = function(e){
        return this[e] instanceof Function ? this[e].apply(this, [].slice.call(arguments, 1)) : undefined;
    };
}
*/
//+ Jonas Raoni Soares Silva
//@ http://jsfromhell.com/geral/utf-8 [rev. #1]

UTF8 = {
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
/*
//+ Jonas Raoni Soares Silva
//@ http://jsfromhell.com/geral/focuser [rev. #2]

focuser = function(o){
    var x, $ = document.body.appendChild(document.createElement("input")), s = $.style,
    h = function(e){(o["on" + e.type] instanceof Function) && o["on" + e.type].call(o, e.key);};
    $.type = "text", s.position = "absolute", s.left = s.top = "-100px";
    o.blur = function(){$.blur();}, addEvent(o, "click", o.focus = function(){$.focus();});
    for(x in {keypress: 0, keydown: 0, keyup: 0, blur: 0, focus: 0}) addEvent($, x, h);
};
*/

//+ Jonas Raoni Soares Silva
//@ http://jsfromhell.com/geral/equals [rev. #1]

equals = function(a, b){
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
    return n = new Array((++n, f ? (f = (this + "").length) < n ? n - f : 0 : n)).join(0), r ? this + n : n + this;
};
//+ Carlos R. L. Rodrigues
//@ http://jsfromhell.com/number/base-conversor [rev. #1]

Number.prototype.toBase = function(b, c){
    var s = "", n = this;
    if(b > (c = (c || "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz").split("")).length || b < 2) return "";
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
/*
//+ Jonas Raoni Soares Silva
//@ http://jsfromhell.com/forms/selection [rev. #1]

Selection = function(input){
    this.isTA = (this.input = input).nodeName.toLowerCase() == "textarea";
};
with({o: Selection.prototype}){
    o.setCaret = function(start, end){
        var o = this.input;
        if(Selection.isStandard)
            o.setSelectionRange(start, end);
        else if(Selection.isSupported){
            var t = this.input.createTextRange();
            end -= start + o.value.slice(start + 1, end).split("\n").length - 1;
            start -= o.value.slice(0, start).split("\n").length - 1;
            t.move("character", start), t.moveEnd("character", end), t.select();
        }
    };
    o.getCaret = function(){
        var o = this.input, d = document;
        if(Selection.isStandard)
            return {start: o.selectionStart, end: o.selectionEnd};
        else if(Selection.isSupported){
            var s = (this.input.focus(), d.selection.createRange()), r, start, end, value;
            if(s.parentElement() != o)
                return {start: 0, end: 0};
            if(this.isTA ? (r = s.duplicate()).moveToElementText(o) : r = o.createTextRange(), !this.isTA)
                return r.setEndPoint("EndToStart", s), {start: r.text.length, end: r.text.length + s.text.length};
            for(var $ = "[###]"; (value = o.value).indexOf($) + 1; $ += $);
            r.setEndPoint("StartToEnd", s), r.text = $ + r.text, end = o.value.indexOf($);
            s.text = $, start = o.value.indexOf($);
            if(d.execCommand && d.queryCommandSupported("Undo"))
                for(r = 3; --r; d.execCommand("Undo"));
            return o.value = value, this.setCaret(start, end), {start: start, end: end};
        }
        return {start: 0, end: 0};
    };
    o.getText = function(){
        var o = this.getCaret();
        return this.input.value.slice(o.start, o.end);
    };
    o.setText = function(text){
        var o = this.getCaret(), i = this.input, s = i.value;
        i.value = s.slice(0, o.start) + text + s.slice(o.end);
        this.setCaret(o.start += text.length, o.start);
    };
    new function(){
        var d = document, o = d.createElement("input"), s = Selection;
        s.isStandard = "selectionStart" in o;
        s.isSupported = s.isStandard || (o = d.selection) && !!o.createRange();
    };
}
*/
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
/*
//+ Carlos R. L. Rodrigues
//@ http://jsfromhell.com/classes/countdown [rev. #1]

CountDown = function(){
    this.date = !(this.finished = this.paused = false);
}
CountDown.prototype.start = function(n, e, t, r){
    var o = this;
    r ? o.onResume && o.onResume(o.n) :
    (o.n = o.date ? new Date(n).getTime() : n, o.e = o.date ? new Date(e).getTime() : e,
    o.t = t, o.finished = false, o.onStart && o.onStart(o.n));
    o.d = e < n ? 1 : -1, o.paused = false, o.i = setInterval(function(){
        o.d * (o.n -= o.d * (o.date ? 1e3 : 1)) <= o.e * o.d &&
        (o.finished = !o.stop()) && !clearInterval(o.i) ||
        o.onUpdate && o.onUpdate(o.n);
    }, (o.t || 1) * 1e3);
}
CountDown.prototype.pause = function(t){
    var o = this;
    clearTimeout(o.x), o.paused ? o.start(o.n, o.e, o.t, 1) :
    (o.paused = !clearInterval(o.i), o.onPause && o.onPause(o.n),
    t && (o.x = setTimeout(function(){
        o.start(o.n, o.e, o.t, 1);
    }, t * 1e3)));
}
CountDown.prototype.stop = function(){
    var o = this;
    clearInterval(o.i), clearTimeout(o.x), o.onStop && o.onStop(o.n), o.n = 0;
}

//+ Jonas Raoni Soares Silva
//@ http://jsfromhell.com/classes/bignumber [rev. #3]

BigNumber = function(n, p, r){
    var o = this, i;
    if(n instanceof BigNumber){
        for(i in {precision: 0, roundType: 0, _s: 0, _f: 0}) o[i] = n[i];
        o._d = n._d.slice();
        return;
    }
    o.precision = isNaN(p = Math.abs(p)) ? BigNumber.defaultPrecision : p;
    o.roundType = isNaN(r = Math.abs(r)) ? BigNumber.defaultRoundType : r;
    o._s = (n += "").charAt(0) == "-";
    o._f = ((n = n.replace(/[^\d.]/g, "").split(".", 2))[0] = n[0].replace(/^0+/, "") || "0").length;
    for(i = (n = o._d = (n.join("") || "0").split("")).length; i; n[--i] = +n[i]);
    o.round();
};
with({$: BigNumber, o: BigNumber.prototype}){
    $.ROUND_HALF_EVEN = ($.ROUND_HALF_DOWN = ($.ROUND_HALF_UP = ($.ROUND_FLOOR = ($.ROUND_CEIL = ($.ROUND_DOWN = ($.ROUND_UP = 0) + 1) + 1) + 1) + 1) + 1) + 1;
    $.defaultPrecision = 40;
    $.defaultRoundType = $.ROUND_HALF_UP;
    o.add = function(n){
        if(this._s != (n = new BigNumber(n))._s)
            return n._s ^= 1, this.subtract(n);
        var o = new BigNumber(this), a = o._d, b = n._d, la = o._f,
        lb = n._f, n = Math.max(la, lb), i, r;
        la != lb && ((lb = la - lb) > 0 ? o._zeroes(b, lb, 1) : o._zeroes(a, -lb, 1));
        i = (la = a.length) == (lb = b.length) ? a.length : ((lb = la - lb) > 0 ? o._zeroes(b, lb) : o._zeroes(a, -lb)).length;
        for(r = 0; i; r = (a[--i] = a[i] + b[i] + r) / 10 >>> 0, a[i] %= 10);
        return r && ++n && a.unshift(r), o._f = n, o.round();
    };
    o.subtract = function(n){
        if(this._s != (n = new BigNumber(n))._s)
            return n._s ^= 1, this.add(n);
        var o = new BigNumber(this), c = o.abs().compare(n.abs()) + 1, a = c ? o : n, b = c ? n : o, la = a._f, lb = b._f, d = la, i, j;
        a = a._d, b = b._d, la != lb && ((lb = la - lb) > 0 ? o._zeroes(b, lb, 1) : o._zeroes(a, -lb, 1));
        for(i = (la = a.length) == (lb = b.length) ? a.length : ((lb = la - lb) > 0 ? o._zeroes(b, lb) : o._zeroes(a, -lb)).length; i;){
            if(a[--i] < b[i]){
                for(j = i; j && !a[--j]; a[j] = 9);
                --a[j], a[i] += 10;
            }
            b[i] = a[i] - b[i];
        }
        return c || (o._s = n._s), o._f = d, o._d = b, o.round();
    };
    o.multiply = function(n){
        var o = new BigNumber(this), r = o._d.length >= (n = new BigNumber(n))._d.length, a = (r ? o : n)._d,
        b = (r ? n : o)._d, la = a.length, lb = b.length, x = new BigNumber, i, j, s;
        for(i = lb; i; r && s.unshift(r), x.set(x.add(new BigNumber(s.join("")))))
            for(s = (new Array(lb - --i)).join("0").split(""), r = 0, j = la; j; r += a[--j] * b[i], s.unshift(r % 10), r = (r / 10) >>> 0);
        return o._f = ((r = la + lb - o._f - n._f) >= (j = (o._d = x._d).length) ? this._zeroes(o._d, r - j + 1, 1).length : j) - r, o.round();
    };
    o.divide = function(n){
        if((n = new BigNumber(n)) == "0")
            throw new Error("Division by 0");
        else if(this == "0")
            return new BigNumber;
        var o = new BigNumber(this), a = o._d, b = n._d, la = a.length - o._f,
        lb = b.length - n._f, r = new BigNumber, i = 0, j, s, l, f = 1, c = 0, e = 0;
        r._s = o._s != n._s, r.precision = Math.max(o.precision, n.precision),
        r._f = +r._d.pop(), la != lb && o._zeroes(la > lb ? b : a, Math.abs(la - lb));
        n._f = b.length, b = n, b._s = false, b = b.round();
        for(n = new BigNumber; a[0] == "0"; a.shift());
        out:
        do{
            for(l = c = 0, n == "0" && (n._d = [], n._f = 0); i < a.length && n.compare(b) == -1; ++i){
                (l = i + 1 == a.length, (!f && ++c > 1 || (e = l && n == "0" && a[i] == "0")))
                && (r._f == r._d.length && ++r._f, r._d.push(0));
                (a[i] == "0" && n == "0") || (n._d.push(a[i]), ++n._f);
                if(e)
                    break out;
                if((l && n.compare(b) == -1 && (r._f == r._d.length && ++r._f, 1)) || (l = 0))
                    while(r._d.push(0), n._d.push(0), ++n._f, n.compare(b) == -1);
            }
            if(f = 0, n.compare(b) == -1 && !(l = 0))
                while(l ? r._d.push(0) : l = 1, n._d.push(0), ++n._f, n.compare(b) == -1);
            for(s = new BigNumber, j = 0; n.compare(y = s.add(b)) + 1 && ++j; s.set(y));
            n.set(n.subtract(s)), !l && r._f == r._d.length && ++r._f, r._d.push(j);
        }
        while((i < a.length || n != "0") && (r._d.length - r._f) <= r.precision);
        return r.round();
    };
    o.mod = function(n){
        return this.subtract(this.divide(n).intPart().multiply(n));
    };
    o.pow = function(n){
        var o = new BigNumber(this), i;
        if((n = (new BigNumber(n)).intPart()) == 0) return o.set(1);
        for(i = Math.abs(n); --i; o.set(o.multiply(this)));
        return n < 0 ? o.set((new BigNumber(1)).divide(o)) : o;
    };
    o.set = function(n){
        return this.constructor(n), this;
    };
    o.compare = function(n){
        var a = this, la = this._f, b = new BigNumber(n), lb = b._f, r = [-1, 1], i, l;
        if(a._s != b._s)
            return a._s ? -1 : 1;
        if(la != lb)
            return r[(la > lb) ^ a._s];
        for(la = (a = a._d).length, lb = (b = b._d).length, i = -1, l = Math.min(la, lb); ++i < l;)
            if(a[i] != b[i])
                return r[(a[i] > b[i]) ^ a._s];
        return la != lb ? r[(la > lb) ^ a._s] : 0;
    };
    o.negate = function(){
        var n = new BigNumber(this); return n._s ^= 1, n;
    };
    o.abs = function(){
        var n = new BigNumber(this); return n._s = 0, n;
    };
    o.intPart = function(){
        return new BigNumber((this._s ? "-" : "") + (this._d.slice(0, this._f).join("") || "0"));
    };
    o.valueOf = o.toString = function(){
        var o = this;
        return (o._s ? "-" : "") + (o._d.slice(0, o._f).join("") || "0") + (o._f != o._d.length ? "." + o._d.slice(o._f).join("") : "");
    };
    o._zeroes = function(n, l, t){
        var s = ["push", "unshift"][t || 0];
        for(++l; --l;  n[s](0));
        return n;
    };
    o.round = function(){
        if("_rounding" in this) return this;
        var $ = BigNumber, r = this.roundType, b = this._d, d, p, n, x;
        for(this._rounding = true; this._f > 1 && !b[0]; --this._f, b.shift());
        for(d = this._f, p = this.precision + d, n = b[p]; b.length > d && !b[b.length -1]; b.pop());
        x = (this._s ? "-" : "") + (p - d ? "0." + this._zeroes([], p - d - 1).join("") : "") + 1;
        if(b.length > p){
            n && (r == $.DOWN ? false : r == $.UP ? true : r == $.CEIL ? !this._s
            : r == $.FLOOR ? this._s : r == $.HALF_UP ? n >= 5 : r == $.HALF_DOWN ? n > 5
            : r == $.HALF_EVEN ? n >= 5 && b[p - 1] & 1 : false) && this.add(x);
            b.splice(p, b.length - p);
        }
        return delete this._rounding, this;
    };
}



function permute(a, b) { // two arrays
  if (a.isOptional) {
    a.push('');
  }
  if (b.isOptional) {
    b.push('');
  }
  var resA = [], resI = 0, resH = {}, i = a.length, j = b.length, k, str;
  while (i--) {
    k=j;
    while (k--) {
      if (typeof(resH[str = a[i] + b[k]])=='undefined') {
        resH[str] = resA[resI++] = str;
      }
    }
  }
  return resA;
}

function permuteWithSelfTimes(a, b) {
  var result = b;
  while (--a) {
    b = permute(b, b);
  }
  return b;
}
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

// try to use StringBuffer instead of string concatenation to improve performance
/*
function StringBuffer() {
  this.strings = []
  for (var idx = 0; idx < arguments.length; idx++)
    this.nextPutAll(arguments[idx])
}
StringBuffer.prototype.nextPutAll = function(s) { this.strings.push(s) }
StringBuffer.prototype.contents   = function()  { return this.strings.join("") }
String.prototype.writeStream      = function() { return new StringBuffer(this) }

// make Arrays print themselves sensibly

String.prototype.printOn = Number.prototype.printOn = Boolean.prototype.printOn = function(ws) { ws.nextPutAll(this) }

//Array.prototype.toString = function() { var ws = "".writeStream(); this.printOn(ws); return ws.contents() }
Array.prototype.printOn = function(ws) {
  ws.nextPutAll(this.toString());
}
*/
/*
// delegation

Object.prototype.delegated = function(props) {
  var f = function() { }
  f.prototype = this
  var r = new f()
  for (var p in props)
    if (props.hasOwnProperty(p))
      r[p] = props[p]
  return r
}

// some reflective stuff

Object.prototype.ownPropertyNames = function() {
  var r = []
  for (name in this)
    if (this.hasOwnProperty(name))
      r.push(name)
  return r
}

Object.prototype.hasProperty = function(p) { return this[p] != undefined }

Object.prototype.isNumber    = function() { return false }
Number.prototype.isNumber    = function() { return true }

Object.prototype.isString    = function() { return false }
String.prototype.isString    = function() { return true }

Object.prototype.isCharacter = function() { return false }

String.prototype.isCharacter = function() { return this.length == 1 }
String.prototype.isSpace     = function() { return this.isCharacter() && this.charCodeAt(0) <= 32   }
String.prototype.isDigit     = function() { return this.isCharacter() && this >= "0" && this <= "9" }
String.prototype.isLower     = function() { return this.isCharacter() && this >= "a" && this <= "z" }
String.prototype.isUpper     = function() { return this.isCharacter() && this >= "A" && this <= "Z" }
  
String.prototype.digitValue  = function() { return this.charCodeAt(0) - "0".charCodeAt(0) }

Object.prototype.isSequenceable = false
Array.prototype.isSequenceable  = true
String.prototype.isSequenceable = true

// some functional programming stuff
*/
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
/*
Array.prototype.delimWith = function(d) {
  return this.reduce(
    function(xs, x) {
      if (xs.length > 0)
        xs.push(d)
      xs.push(x)
      return xs
    },
   [])
}

// Squeak's ReadStream, kind of

function ReadStream(anArrayOrString) {
  this.src = anArrayOrString
  this.pos = 0
}
ReadStream.prototype.atEnd = function() { return this.pos >= this.src.length }
ReadStream.prototype.next  = function() { return this.src.at(this.pos++) }
*/
// escape characters

escapeStringFor = new Object()
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

String.prototype.programified = false;

String.prototype.toProgramString = function String_prototype_toProgramString() {
  if (this.programified) return this;
  var ws = ["\""], c;
  for (var idx = 0, len = this.length; idx < len; idx++) {
    ws[idx+1] = ((c = this.charCodeAt(idx)) > 255 ? String.fromCharCode(c) : escapeStringFor[c]);
  }
  ws[idx+1] = "\"";
  var result = ws.join('');
  result.programified = true;
  return result;
}

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

String.prototype.failedAt = function(index) {
  this.indexFailedAt = index;
  return this;
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









