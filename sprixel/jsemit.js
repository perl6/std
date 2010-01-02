/*
  Copyright (c) 2009, 2010 Matthew Wilson <diakopter@gmail.com>

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

// Begin portions written by and copyright of Alessandro Warth in OMeta/JS
var escapeStringFor = {};
for (var c = 0; c < 256; c++)
  escapeStringFor[c] = String.fromCharCode(c);
escapeStringFor["\\".charCodeAt(0)] = "\\\\";
escapeStringFor["\"".charCodeAt(0)] = '\\"';
escapeStringFor['\''.charCodeAt(0)] = "\\'";
escapeStringFor["\r".charCodeAt(0)] = "\\r";
escapeStringFor["\n".charCodeAt(0)] = "\\n";
escapeStringFor["\t".charCodeAt(0)] = "\\t";
escapeStringFor["\f".charCodeAt(0)] = "\\f";
escapeChar = function(c) {
  return c > 255 ? String.fromCharCode(c) : escapeStringFor[c];
}

function unescapeString(s) {
  if (s[0] == '\\')
    switch (s[1]) {
      case '\\': return '\\';
      case 'r':  return '\r';
      case 'n':  return '\n';
      case 't':  return '\t';
      case "'":  return '\'';
      case '"':  return '\"';
      default:   return s[1];
    }
  else
    return s;
}

String.prototype.toProgramString = function toProgramString() {
  var ws = [], c;
  for (var idx = 0, len = this.length; idx < len; ++idx)
    ws[idx] = ((c = this.charCodeAt(idx)) > 255 ? String.fromCharCode(c) : escapeStringFor[c]);
  return ws.join('');
}
// End portions written by and copyright of Alessandro Warth in OMeta/JS

String.prototype.toQuotedString = function toQuotedString() {
  return '"' + this.toProgramString() + '"';
}

var wasSurrogatePair = false;
// from https://developer.mozilla.org/En/Core_JavaScript_1.5_Reference/Global_Objects/String/CharCodeAt
String.prototype.codepoint = function codepoint(idx) {
  var code = this.charCodeAt(idx);
  if (0xD800 <= code && code <= 0xDBFF) {
    wasSurrogatePair = true;
    return ((code - 0xD800) * 0x400) + (this.charCodeAt(idx+1) - 0xDC00) + 0x10000;
  }
  wasSurrogatePair = false;
  return code;
}

Array.prototype.mapVals = function(fun) {
  var len = this.length >>> 0;
  var res = new Array(len);
  for (var i = 0; i < len; i++)
    res[i] = fun.call(this[i]);
  return res;
};


var derives = (function() {
  function F() {} // cause a constructor's prototype chain to inherit from another's
  return function(ctor, parent) {
    F.prototype = parent.prototype;
    ctor.prototype = new F();
  };
})();


var top = this;

function make_unit_ctor() { return function(){ } }
function make_unary_ctor() { return function(l){ this.l=l } }
function make_binary_ctor() { return function(l,r){ this.l=l;this.r=r } }
function make_trinary_ctor() { return function(l,m,r){ this.l=l;this.m=m;this.r=r } }
function make_quarnary_ctor() { return function(l,m,n,r){ this.l=l;this.m=m;this.n=n;this.r=r } }

function Expr_val(v) { this.v = v }
Expr_val.prototype.emit = function() { return this.v };
function val(v) { return new Expr_val(v) }

var Expr_func = make_binary_ctor();
Expr_func.prototype.emit = function() {
  var code = 'new Function('+(
    this.l.length > 0
      ? this.l.mapVals(String.prototype.toQuotedString, this.l).join(',') + ','
      : ''
  )+'"';
  for(var i=0,l=this.r.length;i<l-1;++i)
    code += this.r[i].emit().toProgramString() + ';';
  if (l > 0) {
    var last = this.r[l-1].emit().toProgramString();
    if (!/^(?:return|if|while|var)\s/.test(last))
      code += 'return '+ last;
    else
      code += last;
  }
  return code + '")';
};
function func(args,stmts) { return new Expr_func(args,stmts) }

var Expr_cond = make_binary_ctor();
Expr_cond.prototype.emit = function() {
  var code = 'if('+this.l.emit()+'){', l=this.r.length;
  for(var i=0,l2=this.r[0].length;i<l2;++i)
    code += this.r[0][i].emit().toProgramString() + ';';
  code += '}';
  for(var i=1;i<l-1;i+=2) {
    code += 'else if('+this.r[i].emit()+'){';
    for(var j=0,l2=this.r[i+1].length;j<l2;++j)
      code += this.r[i+1][j].emit().toProgramString() + ';';
    code += '}';
  }
  if (l % 2 == 0) {
    code += 'else{';
    for(var i=0,l2=this.r[l-1].length;i<l2;++i)
      code += this.r[l-1][i].emit().toProgramString() + ';';
    code += '}';
  }
  return code;
};
function cond(condition,clauses) { return new Expr_cond(condition,clauses) }

function infix(space, name, sym) {
  space[name] = new Function("l","r","return new Expr_"+name+"(l,r)");
  var f = space['Expr_'+name] = make_binary_ctor();
  f.prototype.emit =
    new Function('return "("+this.l.emit()+"'+sym+'"+this.r.emit()+")"');
}

function prefix(space, name, sym, noparens) {
  space[name] = new Function("l","return new Expr_"+name+"(l)");
  var f = space['Expr_'+name] = make_unary_ctor();
  f.prototype.emit = noparens
    ? new Function('return "("+this.l.emit()+"'+sym+')"')
    : new Function('return "('+sym+'("+this.l.emit()+"))"');
}

function postfix(space, name, sym, noparens) {
  space[name] = new Function("l","return new Expr_"+name+"(l)");
  var f = space['Expr_'+name] = make_unary_ctor();
  f.prototype.emit = noparens
    ? new Function('return "'+sym+'("+this.l.emit()+")"')
    : new Function('return "('+sym+'("+this.l.emit()+"))"');
}

function ternary(space, name, lsym, rsym) {
  space[name] = new Function("l","m","r","return new Expr_"+name+"(l,m,r)");
  var f = space['Expr_'+name] = make_trinary_ctor();
  f.prototype.emit =
    new Function('return "("+this.l.emit()+"'+lsym+'"+this.m.emit()+"'+rsym+'"+this.r.emit()+")"');
}

// merely some codegen deferred macro-builders.  Eventually each of the emitter
// methods will accept an object representing the current routine as the first
// parameter.

infix(top, "add", "+");
infix(top, "sub", "-");
infix(top, "mul", "*");
infix(top, "div", "/");
infix(top, "mod", "%");
infix(top, "lt", "<");
infix(top, "le", "<=");
infix(top, "gt", ">");
infix(top, "ge", ">=");
infix(top, "eq", "==");
infix(top, "bor", "|");
infix(top, "bxor", "^");
infix(top, "band", "&");
infix(top, "bsl", "<<");
infix(top, "bsr", ">>");
infix(top, "lor", "||");
infix(top, "land", "&&");
infix(top, "same", "===");
infix(top, "assign", "=");
infix(top, "add_assign", "+=");
infix(top, "sub_assign", "-=");
infix(top, "mul_assign", "*=");
infix(top, "div_assign", "/=");
infix(top, "mod_assign", "%=");
infix(top, "label", ": ", true);

prefix(top, "num", "+");
prefix(top, "preinc", "++", true);
prefix(top, "ret", "return ", true);
prefix(top, "loc", "var ", true);
prefix(top, "predec", "--", true);

postfix(top, "postinc", "++", true);
postfix(top, "postdec", "--", true);

ternary(top, "tern", "?", ":");


var f = eval(func(["zz","yy"],[assign(val("yy"),add(sub(val("zz"),val(2)),val(8))),val("yy"),cond(lt(val(62),val("yy")),[[val("66")],[val("999")]]),val("99")]).emit());
print(f);
print(f(58,44));


var gtr = (function() {
  var count = -1;
  return function() { // grammar transition record constructor.
    this.id = ++count;
  };
})();

function gts() { // base grammar transition set
  this.fail = new gtr(); // each transition set has a fail label destination
}

function lit(literal) { return new glit(literal) }
function glit(literal) { // grammar literal parser builder
  gts.call(this); // call the parent constructor
  this.stateful = false;
  this.v = literal;
}
derives(glit, gts);
glit.prototype.emit = function(c) {
  // TODO: check if past end of input
  c.r.push(val("print(44)"));
};

function both(l,r) {
  return l.stateful || r.stateful
    ? !r.stateful
      ? new gbothls(l,r) // only left stateful
      : !l.stateful
        ? new gbothrs(l,r) // only right stateful
        : new gbothlrs(l,r) // both stateful
    : new gboth(l,r); // fully stateless; neither stateful (yay, fully inlined)
}

function utf32str_charAt(offset) {
  return String.fromCharCode(this[offset]);
}

function utf32str_substr(offset, length) {
  return offset <= 0
    ? offset + length > this.l
      ? this.str.substring(0)
      : this.str.substring(0, this.breaks[offset + length - 1] + 1)
    : offset + length > this.l
      ? this.str.substring(this.breaks[offset - 1] + 1)
      : this.str.substring(this.breaks[offset - 1] + 1, this.breaks[offset + length - 1] + 1);
}

function utf32str(str) {
  var arr = [], i = 0, breaks = [];
  for(var j=0,l=str.length; j<l; ++j) {
    arr[i] = str.codepoint(j);
    breaks[i++] = wasSurrogatePair ? ++j : j;
  }
  arr.l = i - 1; // stash length statically since .length is usually an accessor
  arr.str = str; // original string (encoded in UTF-16, of course)
  arr.strl = l; // length of original UTF-16 string
  arr.breaks = breaks; // offsets of the final position of the char at each offset
  arr.hasSurrogatePair = l > i; // whether 
  arr.constructor = utf32str; // fake having called "new utf32str()"
  arr.charAt = utf32str_charAt;
  arr.substr = utf32str_substr;
  return arr; // array of ints representing the string's unicode codepoints
}

var gl = val("gl"); // the next target goto label variable expression
var i = val("i"); // the input variable expression


var routine = func(["i"],[val("var gl=0;for(;;){switch(gl){case 0:")]); // empty parser routine

var grammar = lit("hi"); // a grammar expression definition

grammar.emit(routine); // have the grammar emit its specialize parser code to this routine

routine.r.push(val("}}")); // finalize the routine

var input = utf32str("\uD80C\uDF14\uD80C\uDF15\uD80C\uDF16");
print(input.str);
print(input.substr(0,1));
print(input.substr(1,1));
print(input.substr(2,1));
print("\uD80C\uDF14\uD80C\uDF15\uD80C\uDF16");
print("\uD80C\uDF14");
print("\uD80C\uDF15");
print("\uD80C\uDF16");

var parser = eval(routine.emit());

parser(input);



















