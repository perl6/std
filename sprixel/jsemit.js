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
      case 'f':  return '\f';
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

function keys(o) {
  var res = [], j=-1; for(var i in o) res[++j] = i; return res;
}

function values(o) {
  if (o.__prototype__===Array.prototype) {
    for(var res=[],j=-1,l=o.length;++j<l;res[j]=v(o[j]));
    return res;
  }
  var res = [], j=-1;
  for(var i in o) res[++j] = o[i]; return res;
}

var derives = (function() {
  function F() {} // cause a constructor's prototype chain to inherit from another's
  return function(ctor, parent) {
    F.prototype = parent.prototype;
    ctor.prototype = new F();
  };
})();

var derive = (function() {
  function F() {} // derive an object whose prototype chain inherits from another
  return function(parent) {
    F.prototype = parent;
    return new F();
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
function dval(v) { return new Expr_val(dbg ? v : '') }

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
    var last = this.r[l-1].emit();
    code += (/^(?:return|if|while|var|break|continue|default:|case)\s/.test(last) ? '' : 'return ') + last;
  }
  return code + '")';
};
function func(args,stmts) { return new Expr_func(args,stmts) }

var Expr_cond = make_binary_ctor();
Expr_cond.prototype.emit = function() {
  var code = 'if'+this.l.emit()+'{', l=this.r.length;
  for(var i=0,l2=this.r[0].length;i<l2;++i)
    code += this.r[0][i].emit() + ';';
  code += '}';
  for(var i=1;i<l-1;i+=2) {
    code += 'else if'+this.r[i].emit()+'{';
    for(var j=0,l2=this.r[i+1].length;j<l2;++j)
      code += this.r[i+1][j].emit() + ';';
    code += '}';
  }
  if (l % 2 == 0) {
    code += 'else{';
    for(var i=0,l2=this.r[l-1].length;i<l2;++i)
      code += this.r[l-1][i].emit() + ';';
    code += '}';
  }
  return code;
};
function cond(condition,clauses) { return new Expr_cond(condition,clauses) }

function infix(space, name, sym) {
  space[name] = new Function("l","r","return new Expr_"+name+"(l,r)");
  var f = space['Expr_'+name] = make_binary_ctor();
  f.prototype.emit =
    new Function('return"("+this.l.emit()+"'+sym+'"+this.r.emit()+")"');
}

function prefix(space, name, sym, noparens) {
  space[name] = new Function("l","return new Expr_"+name+"(l)");
  var f = space['Expr_'+name] = make_unary_ctor();
  f.prototype.emit = noparens
    ? new Function('return'+(sym+'(').toQuotedString()+'+this.l.emit()+")"')
    : new Function('return'+('('+sym+'(').toQuotedString()+'+this.l.emit()+"))"');
}

function postfix(space, name, sym, noparens) {
  space[name] = new Function("l","return new Expr_"+name+"(l)");
  var f = space['Expr_'+name] = make_unary_ctor();
  f.prototype.emit = noparens
    ? new Function('return this.l.emit()+'+sym.toQuotedString())
    : new Function('return"("+this.l.emit()+'+(sym+')').toQuotedString());
}

function ternary(space, name, lsym, rsym) {
  space[name] = new Function("l","m","r","return new Expr_"+name+"(l,m,r)");
  var f = space['Expr_'+name] = make_trinary_ctor();
  f.prototype.emit =
    new Function('return"("+this.l.emit()+"'+lsym+'"+this.m.emit()+"'+rsym+'"+this.r.emit()+")"');
}

// merely some codegen deferred macro-builders.

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
infix(top, "ne", "!=");
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

var gtr = (function() {
  var count = 2;
  return function() { // grammar transition record constructor.
    this.id = ++count;
    return this;
  };
})();
gtr.prototype.toString = function() { return this.id.toString() };

function gts() { // base grammar transition set
  //print('building '+this);
  this.fail = new gtr(); // each transition set has a fail label destination
}
gts.prototype.cur = function() { // whether it contains an unresolved named reference
  return this.u = this.u || (typeof this.l != 'undefined' && this.l.cur())
    || (typeof this.r != 'undefined' && this.r.cur());
};
gts.prototype.u = false;
gts.prototype.computeRefs = function(pats, name, refs) {
  if (typeof this.l != 'undefined')
    this.l.computeRefs(pats, name, refs);
  if (typeof this.r != 'undefined')
    this.r.computeRefs(pats, name, refs);
};
gts.prototype.regen = function(grammar) {
  return this.root(
    this.l ? this.l.regen(grammar) : (this.v ? this.v.str : this.lo ? this.lo.str : null ),
    this.r ? this.r.regen(grammar) : this.hi ? this.hi.str : null );
};
gts.prototype.resolveSym = function(sym) {
  if (typeof this.l != 'undefined')
    this.l.resolveSym(sym, this, false);
  if (typeof this.r != 'undefined')
    this.r.resolveSym(sym, this, true);
  return this;
};

var gl = val("gl"); // the next target goto label variable expression
var i = val("i"); // the input variable expression
var o = val("o"); // the offset variable expression
var t = val("t"); // the State variable expression
var b = val("break"); // macro for "break" instruction
var d = val("t={i:t,s:o}"); // macro for "descend into new State object"
var a = val("t=t.i"); // macro for "ascend to parent State object"
var dls = val("t.i.l=t;t=t.i"); // macro for "store the last in my left"
var dl = val("t=t.l"); // macro for "descend into my left"
var drs = val("t.i.r=t;t=t.i");
var dr = val("t=t.r");
var ros = val("o=t.s"); // macro for "reset offset to my start"

function gotol(lbl) { // macro for goto label with id of the transition object
  if (!lbl)
    print(arguments.callee.caller);
  return val("gl="+lbl.id+";break");
}

function casel(lbl) { // macro for MARK label with id of the transition object
  if (!lbl)
    print(arguments.callee.caller);
  return val("case "+lbl.id+":");
}

function lit(literal) { return new (literal.length > 1 ? glit : gcc)(literal) }
function glit(literal) { // grammar literal parser builder
  gts.call(this); // call the parent constructor
  this.b = false;
  this.v = utf32str(literal);
}
derives(glit, gts);
glit.prototype.emit = function(c) {
  c.r.push(cond(ne(val("i.substr(o,"+this.v.l+")"),val(this.v.str.toQuotedString())),[[
    gotol(this.fail)
  ],[
    add_assign(o,val(this.v.l))
  ]]));
  // TODO: below, but only on strings shorter than 100. Deconstruct
  //   strings 100 chars or longer, esp if they are the result of a
  //   "x" operation.
  // TODO: inline each char check instead of calling substr
/*  c.r.push(cond(ne(val(0),bxor(val("i[o]"),val(this.v[0]))),[[
    assign(gl,val(this.fail.id)),
    b
  ]]));
  for (var chars=this.v,k=1,l=chars.l;k<l;++k) {
    c.r.push(cond(ne(val(0),bxor(val("i[o+"+k+"]"),val(chars[k]))),[[
      assign(gl,val(this.fail.id)),
      b
    ]]));
  }*/
};
glit.prototype.toString = function() {
  return 'lit('+this.v.str.toQuotedString()+')';
};
glit.prototype.root = lit;

function lb(literal) { return new glb(literal) }
function glb(literal) { // grammar literal lookbehind parser builder
  gts.call(this); // call the parent constructor
  this.b = false;
  this.v = utf32str(literal);
}
derives(glb, gts);
glb.prototype.emit = function(c) {
  c.r.push(cond(ne(val("i.substr(o-"+this.v.l+","+this.v.l+")"),val(this.v.str.toQuotedString())),[[
    gotol(this.fail)
  ]]));
};
glb.prototype.toString = function() {
  return 'lb('+this.v.str.toQuotedString()+')';
};
glb.prototype.root = lb;

function prior() { return new gprior() }
function gprior() { // grammar '<.prior> (literal constant string of last successful match)' parser builder
  gts.call(this); // call the parent constructor
  this.b = false;
}
derives(gprior, gts);
var sprixel$$last_match;
function sprixel$$get_last_match_str() {
  return (typeof sprixel$$last_match == 'undefined' ? 'sprixel$$last_match' : sprixel$$last_match).toString();
}
gprior.prototype.emit = function(c) {
  c.r.push(cond(ne(val('(cp=sprixel$$get_last_match_str())'),val("i.substr(o,cp.length)")),[[
    gotol(this.fail)
  ],[
    add_assign(o,val('cp.length'))
  ]]));
};
gprior.prototype.toString = function() {
  return 'prior()';
};
gprior.prototype.root = prior;

function fail() { return new gfail() }
function gfail() { // grammar '<!> (never match anything)' parser builder
  gts.call(this); // call the parent constructor
  this.b = false;
}
derives(gfail, gts);
gfail.prototype.emit = function(c) {
  c.r.push(
    gotol(this.fail)
  );
};
gfail.prototype.toString = function() {
  return 'fail()';
};
gfail.prototype.root = fail;

function cc() { return new gcc(arguments) }
function gcc(chars) { // grammar 'character class' parser builder
  gts.call(this); // call the parent constructor
  this.b = false;
  this.chars = chars;
  var con = '(cp=='+utf32str(chars[0])[0]
  for (var i=1; i<chars.length; ++i) {
    con += '||cp=='+utf32str(chars[i])[0];
  }
  this.con = val(con+')');
}
derives(gcc, gts);
gcc.prototype.emit = function(c) {
  c.r.push(
    val('cp=+i[o]'), // grab the unicode codepoint at the current offset
    cond(this.con,[[
      val('++o')
    ],[
      gotol(this.fail)
    ]])
  );
};
gcc.prototype.toString = function() {
  var chars = this.chars[0].toProgramString();
  for (var i=1;i<this.chars.length;++i)
    chars += "','"+this.chars[i].toProgramString();
  return "cc('"+chars+"')";
};
gcc.prototype.regen = function(grammar) {
  return new gcc(this.chars);
};
gcc.prototype.root = cc;

function notchar(literal) { return new gnotchar(literal) }
function gnotchar(literal) { // grammar literal parser builder
  gts.call(this); // call the parent constructor
  this.b = false;
  this.v = utf32str(literal);
}
derives(gnotchar, gts);
gnotchar.prototype.emit = function(c) {
  c.r.push(cond(land(ne(o,val("l")),ne(val("i.substr(o,"+this.v.l+")"),val(this.v.str.toQuotedString()))),[[
    add_assign(o,val(this.v.l))
  ],[
    gotol(this.fail)
  ]]));
};
gnotchar.prototype.toString = function() {
  return 'notchar('+this.v.str.toQuotedString()+')';
};
gnotchar.prototype.root = notchar;

function range(lo,hi) { return new grange(lo,hi) }
function grange(lo,hi) { // grammar range parser builder
  gts.call(this); // call the parent constructor
  this.b = false;
  this.lo = utf32str(lo);
  this.hi = utf32str(hi);
}
derives(grange, gts);
grange.prototype.emit = function(c) {
  c.r.push(
    val('cp=+i[o]'),
    cond(lor(ge(o,val("l")),lor(gt(val(this.lo[0]),val("cp")),lt(val(this.hi[0]),val("cp")))),[[
      gotol(this.fail)
    ],[
      val("++o")
    ]])
  );
};
grange.prototype.toString = function() {
  return 'range('+this.lo.str.toQuotedString()+','+this.hi.str.toQuotedString()+')';
};
grange.prototype.root = range;

function ranges(rangestr) { return new granges(rangestr) }
function granges(rangestr) { // grammar ranges parser builder
  gts.call(this); // call the parent constructor
  this.b = false;
  this.rangestr = rangestr;
  // TODO: memoize the generation of this...
  var nextrange = /([a-fA-F0-9]{4})(-([a-fA-F0-9]{4}))?/g;
  var cond = '(cp';
  var mya, lower, upper;
  while ((mya = nextrange.exec(rangestr)) != null) {
    lower = parseInt(mya[1],16);
    if (typeof mya[3] != 'undefined') {
      upper = parseInt(mya[3],16);
      cond += '>'+(lower-1)+'&&cp<'+(upper+1)+'||cp';
    } else {
      cond += '=='+lower+'||cp';
    }
  }
  this.con = val(''+cond.slice(0,cond.length-4)+')');
}
derives(granges, gts);
granges.prototype.emit = function(c) {
  c.r.push(
    val('cp=+i[o]'), // grab the unicode codepoint at the current offset
    cond(this.con,[[
      val('++o')
    ],[
      gotol(this.fail)
    ]])
  );
};
granges.prototype.toString = function() {
  return 'ranges("'+this.rangestr+'")';
};
granges.prototype.root = ranges;
granges.prototype.regen = function() {
  return new granges(this.rangestr);
}

function doublec() { return new gdoublec() }
function gdoublec() { // grammar 'double colon (fail to last alternation)' builder
  gts.call(this); // call the parent constructor
  this.b = true;
  this.init = new gtr();
  this.bt = new gtr();
  this.notd = new gtr();
  this.done = new gtr();
}
derives(gdoublec, gts);
gdoublec.prototype.emit = function(c) {
  c.r.push(
    casel(this.init),
    d,
    gotol(this.notd),
    
    casel(this.bt),
    val("t=cg;gl=cl;break")
  );
};
gdoublec.prototype.toString = function() {
  return 'doublec()';
};
gdoublec.prototype.root = doublec;

function singlec(l) { return new gsinglec(l) }
function gsinglec(l) { // grammar 'single colon (fail if backtracked over)' builder
  // note: a Perl 6 parser generator must wrap the prior atom in this, just like
  // the repetition operators.
  gts.call(this); // call the parent constructor
  this.b = true;
  this.l = l;
  if (!l.b) { // fake the labels
    this.l.done = new gtr();
    this.l.notd = new gtr();
  }
  this.init = new gtr();
  this.bt = new gtr();
  this.notd = new gtr();
  this.done = new gtr();
}
derives(gsinglec, gts);
gsinglec.prototype.emit = function(c) {
  c.r.push(
    casel(this.init),
    d
  );
  this.l.emit(c);
  c.r.push(
    casel(this.l.done),
    a,
    gotol(this.done),
    
    casel(this.l.notd),
    // val("t.i.l=t"), // don't need to save it since we're never backtracking.
    a,
    gotol(this.notd),
    
    casel(this.l.fail),
    // backtrack does the same thing as fail
    casel(this.bt),
    a,
    gotol(this.fail)
  );
};
gsinglec.prototype.toString = function() {
  return 'singlec('+this.l+')';
};
gsinglec.prototype.root = singlec;

function commit() { return new gcommit() }
function gcommit() { // grammar 'commit (fail entire match)' builder
  gts.call(this); // call the parent constructor
  this.b = true;
  this.init = new gtr();
  this.bt = new gtr();
  this.notd = new gtr();
  this.done = new gtr();
}
derives(gcommit, gts);
gcommit.prototype.emit = function(c) {
  c.r.push(
    casel(this.init),
    d,
    dval('print("in commit")'),
    gotol(this.notd),
    
    casel(this.bt),
    dval('print("backtracking commit")'),
    val("gl=1;break") // do not pass Go.  do not collect 200 Perl 6 implementations.
  );
};
gcommit.prototype.toString = function() {
  return 'commit()';
};
gcommit.prototype.root = commit;

function gend() { // grammar "end anchor" parser builder
  gts.call(this); // call the parent constructor
  this.b = false;
}
derives(gend, gts);
gend.prototype.emit = function(c) {
  c.r.push(cond(ne(o,val("l")),[[
    gotol(this.fail)
  ]]));
};
function end() { return new gend() }
gend.prototype.toString = function() {
  return 'end()';
};
gend.prototype.root = end;

function gdot() { // grammar "any char (dot)" parser builder
  gts.call(this); // call the parent constructor
  this.b = false;
}
derives(gdot, gts);
gdot.prototype.emit = function(c) {
  c.r.push(
    cond(ge(o,val("l")),[[
      gotol(this.fail)
    ]]),
    val("++o")
  );
};
function dot() { return new gdot() }
gdot.prototype.toString = function() {
  return 'dot()';
};
gdot.prototype.root = dot;

function gpanic(msg) { // grammar "panic" parser builder
  gts.call(this); // call the parent constructor
  this.b = false;
  this.msg = msg;
}
derives(gpanic, gts);
gpanic.prototype.emit = function(c) {
  c.r.push(
    val('throw '+this.msg.toQuotedString())
  );
};
function panic() { return new gpanic() }
gpanic.prototype.toString = function() {
  return 'panic('+this.msg.toQuotedString+')';
};
gpanic.prototype.regen = function(grammar) {
  return new gpanic(this.msg);
};
gpanic.prototype.root = panic;

function gpref(name) { // grammar "named pattern reference" parser builder
  this.name = name;
  this.b = true;
  gts.call(this); // call the parent constructor
  this.u = true; // mark as unresolved
  this.init = new gtr();
  this.bt = new gtr();
  this.notd = new gtr();
  this.done = new gtr();
}
derives(gpref, gts);
gpref.prototype.emit = function(c) {
  var pats = c.g.pats;
  var pat = pats[this.name];
  if (!pat.isRecursive) { // inline a replica of the callsite here, deeply.
    var clone = pat.b ? pat.regen(c.g) : pat.l.regen(c.g);
    clone.fail = this.fail; // fixup the label references
    if (clone.b) {
      clone.notd = this.notd;
      clone.done = this.done;
      clone.init = this.init;
      clone.bt = this.bt;
    }
    // still backtrack even if we're in a cancellable alternation.
    var prior_calt_count = calt_count;
    calt_count = 0;
    clone.emit(c);
    calt_count = prior_calt_count;
    if (!clone.b) // some combinators aren't aligned perfectly
      //c.r.push(gotol(this.done));
      c.r.push(val('t={i:t}'));
  } else {
    // emit a callsite such that the target site routes correctly (dynamically).
    c.r.push(
      casel(this.init),d,
      val("t.dl="+this.done.id+";t.nl="+this.notd.id+";t.fl="+this.fail.id),
      gotol(pat.init),
      // the other half of this routine is constructed by gcallt (call target)
      casel(this.bt),
      gotol(pat.bt)
    );
  }
};
gpref.prototype.computeRefs = function(pats, name, refs) {
  var hasIt = refs.hasOwnProperty(this.name);
  if (typeof pats[this.name] == 'undefined')
    throw 'pattern '+this.name+' has not been declared';
  refs[this.name] = true;
  if (!hasIt && name != this.name) // prevent cyclical recursion
    pats[this.name].computeRefs(pats, name, refs);
};
function pref(name) { return new gpref(name) }
gpref.prototype.toString = function() {
  return 'pref('+this.name.toQuotedString()+')';
};
gpref.prototype.regen = function(grammar) {
  return new gpref(this.name);
};
gpref.prototype.root = pref;

function gcallt(l) { // grammar "call target" parser builder
  gts.call(this); // call the parent constructor
  this.l = l;
  this.init = new gtr();
  this.bt = new gtr();
  this.notd = new gtr();
  this.done = new gtr();
}
derives(gcallt, gts);
gcallt.prototype.emit = function(c) {
  c.r.push(
    casel(this.init)
    //gotol(this.l.init)
  );
  this.l.emit(c);
  c.r.push(
    casel(this.l.done),
    val("t=t.i;gl=t.dl;break"),
    
    casel(this.l.notd),
    val("t.i.r=t;t=t.i;gl=t.nl;break"),
    
    casel(this.l.fail),
    val("gl=t.fl;t=t.i;break"),
    
    casel(this.bt),
    val("t=t.r"),
    gotol(this.l.bt)
  );
};
gcallt.prototype.toString = function() {
  return this.l.toString();
}

function group(l,name) { return new ggroup(l,name) }
function ggroup(l,name) { // grammar "group (named & anonymous)" parser builder
  gts.call(this); // call the parent constructor
  if (typeof name == 'undefined') {
    this.anon = true;
    throw 'anonymous captures not yet supported';
  } else {
    this.anon = false;
    this.name = name;
  }
  this.l = l;
  if (this.b = this.l.b) { // inherit backtrackability from the child
    this.init = new gtr();
    this.bt = new gtr();
    this.notd = new gtr();
    this.done = new gtr();
  }
}
derives(ggroup, gts);
ggroup.prototype.emit = function(c) {
  c.r.push(
    this.b?casel(this.init):val(''),
    val("m={m:m,s:o,c:{},i:[]"+(
      this.anon ? "" : ",n:"+this.name.toQuotedString()
    )+"};m.m.c["+this.name.toQuotedString()+"]=m") // append to the end of the match linked list
  );
  this.l.emit(c);
  if (this.b) { // need these iff we're backtrackable (non-deterministic)
    c.r.push(
      casel(this.l.done),
      //val("for(var q in m.c){if(!m.m.c.hasOwnProperty(q))m.m.c[q]=m.c[q]};m=m.m"),
      val("m=m.m"),
      gotol(this.done),
      
      casel(this.l.notd),
      //val("for(var q in m.c){if(!m.m.c.hasOwnProperty(q))m.m.c[q]=m.c[q]};m=m.m"),
      val("m=m.m"),
      gotol(this.notd),
      
      casel(this.l.fail),
      val("m=m.m"),
      gotol(this.fail),
      
      casel(this.bt),
      val("m=m.c["+this.name.toQuotedString()+"]||{m:m,s:o,c:{},i:[]"+(
        this.anon ? "" : ",n:"+this.name.toQuotedString()
      )+"}"),
      gotol(this.l.bt)
    );
  }
};
ggroup.prototype.root = group;
ggroup.prototype.regen = function(grammar) {
  return new ggroup(this.l.regen(grammar),this.name);
};
ggroup.prototype.toString = function() {
  return 'group('+this.l+','+this.name.toQuotedString()+')';
};

var calt_count = 0;
function calt(l) { return new gcalt(l) }
function gcalt(l) { // grammar "cancelable alternation" parser builder
  gts.call(this); // call the parent constructor
  this.l = l;
  if (!(this.b = l.b))
    throw "nonsensical usage: cancellable alternation of a deterministic pattern?";
  this.init = new gtr();
  this.bt = new gtr();
  this.notd = new gtr();
  this.done = new gtr();
}
derives(gcalt, gts);
gcalt.prototype.emit = function(c) {
  c.r.push(
    casel(this.init),
    d,
    val("t.cg=cg;t.cl=cl;cg=t;cl="+this.fail.id) // save the last cg; make myself cg
  );
  ++calt_count;
  this.l.emit(c);
  --calt_count;
  c.r.push(
    casel(this.l.done),
    a,
    val("cg=t.cg;cl=t.cl"),
    gotol(this.done),
    
    casel(this.l.notd),
    val("t.i.l=t;t=t.i;cg=t.cg;cl=t.cl"),
    gotol(this.notd),
    
    casel(this.l.fail),
    val("cg=t.cg;cl=t.cl"),
    a,
    gotol(this.fail),
    
    casel(this.bt),
    val("cg=t;t=t.l;cl="+this.fail.id),
    gotol(this.l.bt)
  );
};
gcalt.prototype.root = group;
gcalt.prototype.regen = function(grammar) {
  return new gcalt(this.l.regen(grammar));
};

function action(l,code) { return new gaction(l,code) }
function gaction(l,code) { // grammar "group (named & anonymous)" parser builder
  gts.call(this); // call the parent constructor
  this.code = code;
  this.l = l;
  this.b = this.l.b;
  this.init = new gtr();
  this.bt = new gtr();
  this.notd = new gtr();
  this.done = new gtr();
}
derives(gaction, gts);
gaction.prototype.emit = function(c) {
  this.l.fail = this.fail;
  if (this.l.b)
    this.l.bt = this.bt;
  this.l.emit(c);
  if (this.l.b) {
    c.r.push(
      casel(this.l.done),
      dval('print("doing done action")'),
      val(this.code),
      gotol(this.done),
      
      casel(this.l.notd),
      dval('print("doing notd action")'),
      val(this.code),
      gotol(this.notd)
    );
  } else {
    c.r.push(
      val(this.code)
    );
  }
};
gaction.prototype.root = action;
gaction.prototype.regen = function(grammar) {
  return new gaction(this.l.regen(grammar),this.code);
};
gaction.prototype.toString = function(grammar) {
  return 'action('+this.l+')'; // TODO: allow the action user to give the action a label.
};

function poslook(l) { return new gposlook(l) }
function gposlook(l) { // grammar "positive lookahead" parser builder
  gts.call(this); // call the parent constructor
  this.l = l;
  this.b = true; // it's fully deterministic, even if its child isn't, but we
  // can't mark it as such so it's never used recursively.
  this.init = new gtr();
  this.bt = new gtr();
  this.notd = new gtr();
  this.done = new gtr();
}
derives(gposlook, gts);
gposlook.prototype.emit = function(c) {
  c.r.push(
    casel(this.init),d
  );
  if (!this.l.b)
    this.l.done = this.l.notd = new gtr();
  this.l.emit(c);
  c.r.push(
    casel(this.l.done),
    casel(this.l.notd),
    this.l.b?a:val(''),
    ros,
    gotol(this.done),
    
    casel(this.bt),
    casel(this.l.fail),
    a,
    gotol(this.fail)
  );
};
gposlook.prototype.root = poslook;
gposlook.prototype.regen = function(grammar) {
  return new gposlook(this.l.regen(grammar));
};
gposlook.prototype.toString = function(grammar) {
  return 'poslook('+this.l+')';
};

function neglook(l) { return new gneglook(l) }
function gneglook(l) { // grammar "negative lookahead" parser builder
  gts.call(this); // call the parent constructor
  this.l = l;
  this.b = true; // it's fully deterministic, even if its child isn't, but we
  // can't mark it as such so it's never used recursively.
  this.init = new gtr();
  this.bt = new gtr();
  this.notd = new gtr();
  this.done = new gtr();
}
derives(gneglook, gts);
gneglook.prototype.emit = function(c) {
  c.r.push(
    casel(this.init),d
  );
  if (!this.l.b)
    this.l.done = this.l.notd = new gtr();
  this.l.emit(c);
  c.r.push(
    casel(this.l.done),
    casel(this.l.notd),
    this.l.b?a:val(''),
    a,
    gotol(this.fail),
    
    casel(this.bt),
    casel(this.l.fail),
    ros,
    gotol(this.done)
  );
};
gneglook.prototype.root = neglook;
gneglook.prototype.regen = function(grammar) {
  return new gneglook(this.l.regen(grammar));
};
gneglook.prototype.toString = function(grammar) {
  return 'neglook('+this.l+')';
};

function gbeg() { // grammar "beginning anchor" parser builder
  gts.call(this); // call the parent constructor
  this.b = false;
}
derives(gbeg, gts);
gbeg.prototype.emit = function(c) {
  c.r.push(val("o=0")); // just set current offset to 0. TODO: make parametric
  // on the match start, for when we don't start at offset 0.
};
function beg() { return new gbeg() }
gbeg.prototype.toString = function() {
  return 'beg()';
};
gbeg.prototype.root = beg;

function gempty() { // grammar "empty" (epsilon) parser builder
  gts.call(this); // call the parent constructor
  this.b = false;
}
derives(gempty, gts);
gempty.prototype.emit = function() {
  // no-op; fall through to succeed with finality
};
function empty() { return new gempty() }
gempty.prototype.toString = function() {
  return 'empty()';
};
gempty.prototype.root = empty;

function psym() { return new gpsym() }
function gpsym() { // placeholder for proto sym
  this.b = false; // because it's going to transform into a literal.
}
derives(gpsym, gts);
gpsym.prototype.toString = function() {
  return 'psym()';
};
gpsym.prototype.resolveSym = function(sym, parent, is_right) {
  parent[is_right ? "r" : "l"] = lit(sym);
};

/* Code block generation conventions/rules:
 *  - both non-deterministic (possibly-backtracking) nodes and
      deterministic nodes "fall through" to the end of their code emission
      block upon "done" success.
    - non-deterministic nodes leave their State object ("t" in the generated code)
      on the "t" stack (the "t" local) when returning notdone or done, so that
      its invoker can stash a copy of it if necessary.  When failing, a nondeterministic
      node must pop itself from the t stack ("t=t.i") (where .i is short for
      .invoker) to allow for "fastfail" gotos.
    - the constructors for non-deterministic nodes (compile-time) create their own
      ".bt" (backtrack entry point) and ".init" (initial entry point) transition
      records, but each's parent will create their .notd (not done return path)
      and .done (done return path) transition records.
    - each nondeterministic node creates its own State object, and when backtracking,
      the parent of a nondeterministic node descends the binary tree into the proper
      path branch by setting "t=t.l" (or "t=t.r") as the case may be.
    - failing paths in nondeterministic nodes do not reset the offset (into the
      input) themselves, but rather let the backtracking nodes do it.
 */

function both(l,r) {
  return l.b || r.b
    ? !r.b
      ? new gbothls(l,r) // only left nondeterministic
      : !l.b
        ? new gbothrs(l,r) // only right nondeterministic
        : new gbothlrs(l,r) // both nondeterministic
    : new gboth(l,r); // fully deterministic; neither      (yay, fully inlined)
}
  
function gboth(l, r) { // grammar "deterministic both" parser builder
  gts.call(this, this.l = l, this.r = r); // call the parent constructor
  this.b = false;
}
derives(gboth, gts);
gboth.prototype.emit = function(c) {
  this.l.fail = this.r.fail = this.fail; // short-circuit the children's fail goto
  this.l.emit(c);
  this.r.emit(c);
};
gboth.prototype.root = both;
gboth.prototype.toString = function() {
  return 'both('+this.l+','+this.r+')';
};

function gbothls(l, r) { // grammar "both left nondeterministic" parser builder
  gts.call(this, this.l = l, this.r = r); // call the parent constructor
  this.b = true;
  this.init = new gtr();
  this.bt = new gtr();
  this.notd = new gtr();
  this.done = new gtr();
}
derives(gbothls, gts);
gbothls.prototype.emit = function(c) {
  c.r.push(
    casel(this.init),d // initial entry point
  );
  this.l.emit(c);
  var rightinit = new gtr(); // a label for initial right when backtracking
  c.r.push(
    casel(this.l.done), // left succeeded with finality
    a, // ascend; we don't need a reference to left; it's done
    val("t.ld=true"),
    
    casel(rightinit)
  );
  this.r.emit(c);
  c.r.push(
    // right succeeded; we're either done or not.
    val("gl=t.ld?"+this.done.id+":"+this.notd.id+";break"),

    casel(this.l.notd),
    val("t.i.l=t;t=t.i;t.ld=false"),
    gotol(rightinit),
    
    casel(this.l.fail),
    a,
    gotol(this.fail),
    
    casel(this.r.fail),
    cond(val("(t.ld)"),[[
      a,
      dval("print('bothls fail at '+o)"),
      gotol(this.fail)
    ]]),
    ros,
    
    casel(this.bt),
    val("t=t.l"),
    gotol(this.l.bt)
  );
};
gbothls.prototype.root = both;
gbothls.prototype.toString = function() {
  return 'both('+this.l+','+this.r+')';
};

function gbothrs(l, r) { // grammar "both right nondeterministic" parser builder
  gts.call(this, this.l = l, this.r = r); // call the parent constructor
  this.b = true;
  this.init = new gtr();
  this.bt = new gtr();
  this.notd = new gtr();
  this.done = new gtr();
}
derives(gbothrs, gts);
gbothrs.prototype.emit = function(c) {
  this.l.fail = this.r.fail;
  c.r.push(
    casel(this.init),d // initial entry point
  );
  this.l.emit(c);
  this.r.emit(c);
  c.r.push(
    casel(this.r.done), // right succeeded with finality
    a, // ascend from right
    val("t.r=null"), // nullify the reference to help the GC
    gotol(this.done), // return "done"
    
    casel(this.r.notd), // right succeeded, but is not done.
    val("t.i.r=t;t=t.i"), // stash right in myself; ascend
    gotol(this.notd), // return "not done"
    
    casel(this.bt), // backtrack entry point
    val("t=t.r;o=t.s"),
    gotol(this.r.bt),
    
    casel(this.r.fail), // right failed
    a, // ascend
    gotol(this.fail) // goto our fail
  );
};
gbothrs.prototype.root = both;
gbothrs.prototype.toString = function() {
  return 'both('+this.l+','+this.r+')';
};

function gbothlrs(l, r) { // grammar "both left and right nondeterministic" parser builder
  gts.call(this, this.l = l, this.r = r); // call the parent constructor
  this.b = true;
  this.init = new gtr();
  this.bt = new gtr();
  this.notd = new gtr();
  this.done = new gtr();
}
derives(gbothlrs, gts);
gbothlrs.prototype.emit = function(c) {
  c.r.push(
    casel(this.init),d // initial entry point
  );
  this.l.emit(c);
  var rightinit = new gtr(); // a label for initial right when backtracking
  c.r.push(
    casel(this.l.done), // left succeeded with finality
    a, // ascend; we don't need a reference to left; it's done
    val("t.ld=true"),
    gotol(rightinit),
    
    casel(this.l.notd),
    val("t.i.l=t;t=t.i;t.ld=false"),
    casel(rightinit)
  );
  this.r.emit(c);
  c.r.push(
    casel(this.r.done), // right succeeded with finality
    a, // ascend from right
    val("t.r=null"), // nullify the reference to help the GC
    cond(val("(t.ld)"),[[
      gotol(this.done) // return "done"
    ],[
      gotol(this.notd) // return "not done"
    ]]),
    
    casel(this.r.notd), // right succeeded, but is not done.
    val("t.i.r=t;t=t.i"), // stash right in myself; ascend
    gotol(this.notd), // return "not done"
    
    casel(this.l.fail),
    a,
    gotol(this.fail),
    
    casel(this.bt), // backtrack entry point
    dval('print('+this.toString().toQuotedString()+'+"backtracked at "+t.s)'),
    cond(val("(!t.ld)"),[[
      val("o=t.s;t=t.l"),
      gotol(this.l.bt)
    ]]),
    val("t=t.r;o=t.s"),
    gotol(this.r.bt),
    
    casel(this.r.fail), // right failed
    dval('print('+this.r.toString().toQuotedString()+'+"failed at "+t.s)'),
    cond(val("(!t.ld)"),[[
      val("o=t.s;t=t.l"),
      gotol(this.l.bt)
    ]]),
    a, // ascend
    gotol(this.fail) // goto our fail
  );
};
gbothlrs.prototype.root = both;
gbothlrs.prototype.toString = function() {
  return 'both('+this.l+','+this.r+')';
};

function either(l,r) {
  return l.b || r.b
    ? !r.b
      ? new geitherls(l,r) // only left nondeterministic
      : !l.b
        ? new geitherrs(l,r) // only right nondeterministic
        : new geitherlrs(l,r) // both nondeterministic
    : new geither(l,r); // neither nondeterministic
}

function geither(l,r) { // grammar "deterministic alternation" parser builder
  gts.call(this, this.l = l, this.r = r); // call the parent constructor
  this.b = true;
  this.init = new gtr(); // add a transition record for my initial entry point
  this.bt = new gtr(); // add a transition record for my backtrack entry point
  this.notd = new gtr();
  this.done = new gtr();
}
derives(geither, gts);
geither.prototype.emit = function(c) {
  c.r.push(
    casel(this.init),d
  );
  this.l.emit(c);
  c.r.push( // left succeeded
    gotol(this.xor ? this.done : this.notd), // return "not done"
    
    casel(this.r.fail), // right failed
    a,
    gotol(this.fail), // goto our fail
    
    casel(this.l.fail),
    ros,
    casel(this.bt)
  );
  this.r.emit(c);
  c.r.push(
    gotol(this.done)
  );
};
geither.prototype.root = either;
geither.prototype.toString = function() {
  return 'either('+this.l+','+this.r+')';
};

function geitherls(l,r) { // grammar "left nondeterministic alternation" parser builder
  gts.call(this, this.l = l, this.r = r); // call the parent constructor
  this.b = true;
  this.notd = new gtr();
  this.done = new gtr();
  this.init = new gtr(); // add a transition record for my initial entry point
  this.bt = new gtr(); // add a transition record for my backtrack entry point
}
derives(geitherls, gts);
geitherls.prototype.emit = function(c) {
  c.r.push(
    casel(this.init),d // initial entry point
  );
  this.l.emit(c);
  c.r.push(
    casel(this.l.done), // left succeeded with finality
    a, // ascend
    val("t.ld=true"),
    gotol(this.xor ? this.done : this.notd), // return "not done"
    
    casel(this.l.notd), // left succeeded, but is not done.
    val("t.i.l=t;t=t.i"), // stash left in myself; ascend
    val("t.ld=false"),
    gotol(this.notd), // return "not done"
    
    casel(this.bt), // backtrack entry point
    cond(val("(t.ld)"),[[
      gotol(this.l.fail)
    ],[
      val("t=t.l"),
      gotol(this.l.bt)
    ]]),
    
    casel(this.r.fail), // right failed
    a, // ascend
    gotol(this.fail), // goto our fail
    
    casel(this.l.fail), // left failed
    ros
  );
  this.r.emit(c);
  c.r.push(
    gotol(this.done)
  );
};
geitherls.prototype.root = either;
geitherls.prototype.toString = function() {
  return 'either('+this.l+','+this.r+')';
};

function geitherrs(l,r) { // grammar "right nondeterministic alternation" parser builder
  gts.call(this, this.l = l, this.r = r); // call the parent constructor
  this.b = true;
  this.notd = new gtr();
  this.done = new gtr();
  this.init = new gtr(); // add a transition record for my initial entry point
  this.bt = new gtr(); // add a transition record for my backtrack entry point
}
derives(geitherrs, gts);
geitherrs.prototype.emit = function(c) {
  c.r.push(
    casel(this.init),d // initial entry point
  );
  this.l.emit(c);
  c.r.push(
    val("t.ri=false"), // mark that r has not been inited
    gotol(this.xor ? this.done : this.notd),
    
    casel(this.l.fail),
    val("t.ri=true"), // mark that r has been inited
    ros
  );
  this.r.emit(c);
  c.r.push(
    casel(this.r.done), // right succeeded with finality
    a, // ascend from right
    val("t.r=null"), // nullify the reference to help the GC
    gotol(this.done), // return "done"
    
    casel(this.r.notd), // right succeeded, but is not done.
    val("t.i.r=t;t=t.i"), // stash right in myself; ascend
    gotol(this.xor ? this.done : this.notd), // return "not done"
    
    casel(this.bt), // backtrack entry point
    cond(val("(t.ri)"),[[ // if right has been inited before
      val("t=t.r"),
      gotol(this.r.bt)
    ],[
      gotol(this.l.fail)
    ]]),
    
    casel(this.r.fail), // right failed
    a, // ascend
    gotol(this.fail) // goto our fail
  );
};
geitherrs.prototype.root = either;
geitherrs.prototype.toString = function() {
  return 'either('+this.l+','+this.r+')';
};

function geitherlrs(l,r) { // grammar "left and right nondeterministic alternation" parser builder
  gts.call(this, this.l = l, this.r = r); // call the parent constructor
  this.b = true;
  this.init = new gtr(); // add a transition record for my initial entry point
  this.bt = new gtr(); // add a transition record for my backtrack entry point
  this.notd = new gtr();
  this.done = new gtr();
}
derives(geitherlrs, gts);
geitherlrs.prototype.emit = function(c) {
  c.r.push(
    casel(this.init),d // initial entry point
  );
  this.l.emit(c);
  c.r.push(
    casel(this.l.done), // left succeeded with finality
    a, // ascend
    val("t.ld=true;t.ri=false"), // mark that r has not been inited
    gotol(this.xor ? this.done : this.notd), // return "not done"
    
    casel(this.l.notd), // left succeeded, but is not done.
    val("t.i.l=t"),
    a, // ascend
    val("t.ld=false"), // mark that r has not been inited
    gotol(this.xor ? this.done : this.notd), // return "not done"
    
    casel(this.l.fail),
    val("t.ld=t.ri=true"), // mark that r has been inited
    ros
  );
  this.r.emit(c);
  c.r.push(
    casel(this.r.done), // right succeeded with finality
    a, // ascend from right
    val("t.r=null"), // nullify the reference to help the GC
    gotol(this.done), // return "done"
    
    casel(this.r.notd), // right succeeded, but is not done.
    val("t.i.r=t;t=t.i"), // stash right in myself; ascend
    gotol(this.xor ? this.done : this.notd), // return "not done"
    
    casel(this.bt), // backtrack entry point
    cond(val("(!t.ld)"),[[ // if left isn't done
      val("t=t.l"),
      gotol(this.l.bt)
    ],val("(!t.ri)"),[ // else if
      gotol(this.l.fail)
    ],[ // else
      val("t=t.r"),
      gotol(this.r.bt)
    ]]),
    
    casel(this.r.fail), // right failed
    a, // ascend
    gotol(this.fail) // goto our fail
  );
};
geitherlrs.prototype.root = either;
geitherlrs.prototype.toString = function() {
  return 'either('+this.l+','+this.r+')';
};

function plus(l) {
  var r = l.b
    ? new grepeatb(l,1,-1) // nondeterministic version of plus
    : new grepeat(l,1,-1); // deterministic version of plus
  /*r.toString = function() {
    return 'plus('+l+')';
  };
  r.regen = function(grammar) {
    return plus(this.l.l);
  }*/
  return r;
}

function star(l) {
  var r = either(plus(l),empty());
  /*r.toString = function() {
    return 'star('+l+')';
  };
  r.regen = function(grammar) {
    return star(this.l.l);
  }*/
  return r;
}

function repeat(l,min,max) {
  max = max || -1;
  return min<=0
    ? (max==0) // max==-1 means max is infinite (plus or star)
      ? empty()
      : either(repeat(l,1,max),empty())
    : l.b
      ? new grepeatb(l,min,max) // nondeterministic version of repeat
      : new grepeat(l,min,max); // deterministic version of repeat
}

function grepeat(l,min,max) { // grammar "deterministic" edition of repeat
  this.min = min;
  this.max = max;
  gts.call(this, this.l = l); // call the parent constructor
  this.b = true;
  this.init = new gtr(); // add a transition record for my initial entry point
  this.bt = new gtr(); // add a transition record for my backtrack entry point
  this.notd = new gtr();
  this.done = new gtr();
}
derives(grepeat, gts);
grepeat.prototype.emit = function(c) {
  var retry = new gtr(); // label for retry spot
  c.r.push(
    casel(this.init),d,
    dval('print("repeat init at "+o)'),
    val("t.z=[]"),
    casel(retry)
  );
  this.l.emit(c);
  c.r.push(
    cond(val("(o==t.s)"),[[
        dval('print("repeat done at "+o)'),
      gotol(this.done)
    ]]),
    val("t.z.push(o)")
  );
  if (this.max==-1) {
    c.r.push(gotol(retry));
  } else {
    c.r.push(
      cond(val("(t.z.length<"+this.max+")"),[[
        dval('print("repeat retrying at "+o)'),
        gotol(retry) // try another one
      ],val("(t.z.length>"+this.min+")"),[
        dval('print("repeat not done at "+o)'),
        gotol(this.notd)
      ],[
        dval('print("repeat done at "+o)'),
        gotol(this.done)
      ]])
    );
  }
  c.r.push(
    casel(this.bt),
    dval('print("repeat backtrack at "+o)'),
    val("t.z.pop();o=t.z[t.z.length-1]"),
    
    casel(this.l.fail),
    cond(val("(t.z.length<"+this.min+")"),[[
      a,
      dval('print("repeat fail at "+o)'),
      gotol(this.fail)
    ],val("(t.z.length=="+this.min+")"),[
        dval('print("repeat done at "+o)'),
      gotol(this.done)
    ],[
        dval('print("repeat not done at "+o)'),
      gotol(this.notd)
    ]])
  );
};
grepeat.prototype.toString = function() {
  return 'repeat('+this.l+','+(
    this.min==this.max
      ? this.min.toString()
      : this.min.toString()+','+this.max.toString()
  )+')';
};
grepeat.prototype.root = repeat;
grepeat.prototype.regen = function(grammar) {
  return repeat(this.l.regen(grammar),this.min,this.max);
};

function grepeatb(l,min,max) { // grammar "non-deterministic" edition of plus
  this.min = min;
  this.max = max;
  gts.call(this, this.l = l); // call the parent constructor
  this.b = true;
  this.init = new gtr(); // add a transition record for my initial entry point
  this.bt = new gtr(); // add a transition record for my backtrack entry point
  this.notd = new gtr();
  this.done = new gtr();
}
derives(grepeatb, gts);
grepeatb.prototype.emit = function(c) {
  //if (this.l.b instanceof gpref && !c.g.pats[this.l.name].isRecursive) {
  //  repeat(c.g.pats[this.l.name].regen(c.g),this.min,this.max).emit(c);
  //  return;
  //}
  var retry = new gtr(); // label for retry spot
  c.r.push(
    casel(this.init),d,
    dval('print("repeatb init at "+o)'),
    val("t.z=[];t.b=0;t.tr={};t.ret={}"), // create containers for the child objects
    casel(retry),
    val("t.tr[t.z.length+'.'+o]=1")
  );
  this.l.emit(c);
  c.r.push(
    casel(this.l.done), // left succeeded
    dval('print("repeatb l.done at "+o)'),
    cond(val("(o==t.i.s)"),[[
      dval('print("repeatb done at "+o)'),
      a,
      gotol(this.done)
    ]]),
    val("t.i.z.push(t);t.nd=false;t.e=o"),
    a,
    cond(val("(t.ret[o])"),[[
      dval('print("repeatb going to bt at "+o)'),
      gotol(this.bt)
    ]]),
    //dval("print([t.z.length+'.'+o,t.tr[t.z.length+'.'+o]])"),
    cond((this.max==-1
        ? val("(!t.tr[t.z.length+'.'+o])")
        : val("(t.z.length<"+this.max+"&&!t.tr[t.z.length+'.'+o])")),[[
      dval('print("repeatb retrying at "+o)'),
      gotol(retry) // try another one
    ],val("((t.z.length>"+this.min+")||(t.z.length=="+this.min+"&&t.b>0))"),[
      val("(t.ret[o]=1)"),
      dval('print("repeatb notd at "+o)'),
      gotol(this.notd)
    ],val("(t.z.length<"+this.min+")"),[
      dval('print("repeatb going to bt at "+o)'),
      gotol(this.bt)
    ],[ // we are at a base case for this repetition.
      gotol(this.done)
    ]]),
    
    casel(this.l.notd), // left succeeded
    cond(val("(o==t.i.s)"),[[
      dval('print("repeatb done at "+o)'),
      a,
      gotol(this.done)
    ]]),
    dval('print("repeatb l.notd at "+o)'),
    val("t.i.z.push(t);t.nd=true;t.e=o"),
    a,
    val("++t.b"), // increment the number of possible backtracks.
    cond(val("(t.ret[o])"),[[
      dval('print("repeatb going to bt at "+o)'),
      gotol(this.bt)
    ]]),
    cond((this.max==-1
        ? val("(!t.tr[t.z.length+'.'+o])")
        : val("(t.z.length<"+this.max+"&&!t.tr[t.z.length+'.'+o])")),[[
      dval('print("repeatb retrying at "+o)'),
      gotol(retry) // try another one
    ],val("(t.z.length<"+this.min+")"),[
      dval('print("repeatb going to bt at "+o)'),
      gotol(this.bt)
    ],[
      val("(t.ret[o]=1)"),
      dval('print("repeatb notd at "+o)'),
      gotol(this.notd)
    ]]),
    
    casel(this.bt),
    //dval("print(keys(t))"),
    dval('print("repeatb bt at "+o)'),
    val("u=t.z.pop()"),
    cond(val("(u.nd)"),[[
      val("--t.b;o=u.s"),
      cond(val("(t.ret[o])"),[[
        dval('print("repeatb going to bt at "+o)'),
        gotol(this.bt)
      ]]),
      val("t=u"),
      dval('print("repeatb going to l.bt at "+o)'),
      gotol(this.l.bt)
    ]]),
    val("o=u.s"),
    cond(val("(!t.ret[o])"),[[
      cond(val("((t.z.length>"+this.min+")||(t.z.length=="+this.min+"&&t.b>0))"),[[
        val("(t.ret[o]=1)"),
        dval('print("repeatb notd at "+o)'),
        gotol(this.notd)
      ],val("(t.b>0)"),[
        dval('print("repeatb going to bt at "+o)'),
        gotol(this.bt)
      ],[
        dval('print("repeatb done at "+o)'),
        gotol(this.done)
      ]]),
    ]]),
    
    casel(this.l.fail),
    dval('print("repeatb l.fail at "+o)'),
    cond(val("(t.z.length<"+this.min+")"),[[
      val("o=(t.z[t.z.length-1]||{e:t.s}).e"),
      cond(val("(!t.tr[t.z.length+'.'+o])"),[[
        dval('print("repeatb retrying at "+o)'),
        gotol(retry)
      ]]),
      cond(val("(t.b>0)"),[[
        dval('print("repeatb going44 to bt at "+o)'),
        gotol(this.bt)
      ],[
        a,
        dval('print("repeatb fail at "+o)'),
        gotol(this.fail)
      ]])
    ],val("(t.b>0||t.z.length>"+this.min+")"),[
    // at least 1 of the children says it's not done, or we have > the minimum.
      val("o=(t.z[t.z.length-1]||{e:t.s}).e"),
      cond(val("(!t.ret[o])"),[[
        dval('print("repeatb notd at "+o)'),
        gotol(this.notd)
      ]]),
      dval('print("repeatb going55 to bt at "+o)'),
      gotol(this.bt)
    ],[ // we are at a base case for this repetition.
      val("o=t.z[t.z.length-1].e"), // back up to the last end, if necessary.
      dval('print("repeatb done at "+o)'),
      gotol(this.done)
    ]])
  );
};
grepeatb.prototype.toString = function() {
  return 'repeat('+this.l+','+(
    this.min==this.max
      ? this.min.toString()
      : this.min.toString()+','+this.max.toString()
  )+')';
};
grepeatb.prototype.regen = function(grammar) {
  return repeat(this.l.regen(grammar), this.min, this.max);
};

function utf32str_charAt(offset) {
  return String.fromCharCode(this[offset]);
};

function utf32str_substr(offset, length) {
  //if (dbg)
  //  print('checking offset at '+offset);
  return offset <= 0
    ? offset + length >= this.l
      ? this.str.substring(0)
      : this.str.substring(0, this.breaks[offset + length - 1] + 1)
    : offset + length >= this.l
      ? this.str.substring(this.breaks[offset - 1] + 1)
      : this.str.substring(this.breaks[offset - 1] + 1, this.breaks[offset + length - 1] + 1);
}

function utf32str(str) {
  var arr = [], i = 0, breaks = [];
  for(var j=0,l=str.length; j<l; ++j) {
    arr[i] = str.codepoint(j);
    breaks[i++] = wasSurrogatePair ? ++j : j;
  }
  arr.l = i; // stash length statically since .length is usually an accessor
  arr.str = str; // original string (encoded in UTF-16, of course)
  arr.strl = l; // length of original UTF-16 string
  arr.breaks = breaks; // offsets of the final position of the char at each offset
  arr.hasSurrogatePair = l > i; // whether 
  arr.constructor = utf32str; // fake having called "new utf32str()"
  arr.charAt = utf32str_charAt;
  arr.substr = utf32str_substr;
  return arr; // array of ints representing the string's unicode codepoints
}

function Grammar(name, parent) {
  parent = parent || Grammar.defaultGrammar;
  this.pats = derive(parent
    ? parent.pats
    : Grammar.defaultPatterns); // namespace for patterns
  this.isRoot = !parent; // boolify its definedness
  this.parent = parent;
  this.name = name;
  this.compiled = false;
  this.protos = parent
    ? derive(parent.protos)
    : {};
}
Grammar.prototype.addPattern = function(name, pattern) {
  pattern = group(pattern,name);
  if (this.pats.hasOwnProperty(name))
    throw 'Pattern '+name+' already defined in grammar '+this.name;
  this.pats[name] = pattern;
  if (typeof this.TOP == 'undefined')
    this.TOP = pattern;
  return pattern;
};
Grammar.prototype.compile = function(interpreterState) {
  
  // preprocess the protos
  for (var name in this.protos) {
    var proto = this.protos[name];
    var alts = [];
    for (var sym in proto) {
      var holder = {};
      var result = proto[sym].resolveSym(sym, holder, false);
      alts.push(result || holder.l);
    }
    if (alts.length>0)
      this.addPattern(name, alt.apply(null, alts));
  }
  
  // recursively compute full set of named references for each 
  //   pattern, preventing descent into a cyclical reference.
  for (var name in this.pats) {
    if (dbg)
      print('recursing pattern '+name.toQuotedString());
    var pat = this.pats[name];
    var refs = {};
    var isRecursive = false;
    pat.computeRefs(this.pats, name, refs);
    for (var refName in refs) {
      if (refName == name)
        isRecursive = true;
      if (typeof this.pats[refName] == 'undefined')
        throw 'Pattern '+name+' in grammar '+this.name+' contains unresolved reference to '+refName;
    }
    pat.namedRefs = refs;
    // if a pattern is not recursive, it can be inlined (but cloned
    //   so the labels/gotos are still unique) into its referer.
    if (pat.isRecursive = isRecursive) {
      if (dbg)
        print('recursive pattern: '+name.toQuotedString());
      // wrap the pattern in a call target
      this.pats[name] = new gcallt(pat);
      this.pats[name].isRecursive = true;
      if (pat===this.TOP)
        this.TOP = this.pats[name]; // fixup the TOP one
    }
  }
  
  // now that all the named patterns have been resolved, make another pass to discover/mark
  //   what we can learn about the possible lengths of the patterns (whether it's zero-length,
  //   whether it can possibly be zero-length, & what length it would be if it's not zero-length).
  //   We'll then be able to use this knowledge to detect whether a pattern recursion is left-
  //   recursive or right-recursive (or both, pathologically self-referential - no termination
  //   pattern).  
  // Eventually we'll be able to use those data to definitively compute whether a pattern can be
  //   Boyer-Moore'd, and if there are no named groups (named captures) contained, the entire 
  //   match procedure can be swapped out for a mere JS literal RegExp object (or Perl if we're
  //   targeting Perl 5 by then).  
  for (var name in this.pats) {
    
  }
  
  var preamble = "var m={t:t,c:{}},l=i.l,cp,cg,cl;m.m=m;t.i=t;last:for(;;){";
  var postamble = "case -1:if(dbg)print('parse succeeded');return new Match(t,m.m,g,i,o,true);case -2:if(dbg)print('parse succeeded, but is not completed');return new Match(t,m.m,g,i,o,false);case 1:default:print('parse failed');break last}}";
  
  var routine = dbg
    ? func(["i","g","gl","o","t"],[val(preamble+"print('op: '+gl+' '+o);next:switch(gl){case -4:")])
    : func(["i","g","gl","o","t"],[val(preamble+"next:switch(gl){case -4:")]); // empty parser routine

  var g = this.TOP.regen();

  g.fail = new gtr(); g.fail.id = 1;
  g.done = new gtr(); g.done.id = -1;
  g.notd = new gtr(); g.notd.id = -2;
  g.bt = new gtr(); g.bt.id = -3;
  
  routine.g = this;
  
  // second pass: resolve & emit the named references, now that
  //   we know which ones are recursive.
  for (var name in this.pats) {
    var pat = this.pats[name];
    if (dbg)
      print('compiling pattern '+name.toQuotedString()+' '+pat.l);
    if (pat.isRecursive && pat!==this.TOP) {
      // reset the cancellable alternations counter.
      // TODO: comment following line if it's not needed.
      calt_count = 0;
      pat.emit(routine);
    }
  }
  
  if (dbg)
    print('done compiling patterns');
  
  routine.r.push(val("case 0:"));
  
  g.emit(routine); // have the grammar emit its specialize parser code to this routine

  routine.r.push(val(postamble)); // finalize the routine

  var parserf = routine.emit();

  //if (dbg)
  //  print(parserf);

  this.parser = eval(parserf); // compile the javascript function to machine code
  
  if (dbg)
    print('compiled');
  
  this.compiled = true;
};
Grammar.prototype.parse = function(input) {
  if (!this.compiled)
    this.compile();
  var res = this.parser(input,this,0,0,{});
  if (typeof res != 'undefined')
    return sprixel$$last_match = res;
  // TODO: return Failure
};
Grammar.prototype.next = function(input,t,o) {
  return this.parser(input,this,-3,o,t);
};
Grammar.prototype.addProto = function(name) {
  if (this.protos.hasOwnProperty(name))
    throw 'protopattern '+name+' already declared';
  this.protos[name] = (typeof this.protos[name] != 'undefined')
    ? derive(this.protos[name]) // inherit from the parent if there is one
    : {};
};
Grammar.prototype.addToProto = function(name,sym,pattern) {
  var proto = this.protos[name];
  proto[sym] = pattern;
};

function Match(t,match,grammar,input,offset,completed) {
  this.t = t;
  //print(keys(t));
  this.match = match;
  this.grammar = grammar;
  this.input = input;
  this.offset = offset;
  this.completed = completed;
}
Match.prototype.next = function() {
  if (this.completed)
    return this;
  return this.grammar.next(this.input,this.t,this.offset);
};
Match.prototype.toString = function() {
  return this.input.substr(0,this.offset);
};

// steal some macros/combinators from jsmeta
function opt(l) { return either(l,empty()) }
function xopt(l) { return xor(l,empty()) }
function ws() { return pref('ws') }
function poil(l) { return both(opt(ws()),l) }
function foil(l) { return both(l,opt(ws())) }
function pfoil(l) { return poil(foil(l)) }
function oisep(l,sep) { return both(pfoil(l),star(both(pfoil(sep),pfoil(l.regen())))) }
function oiplus(l) { return both(pfoil(l),star(pfoil(l.regen()))) }
function ows() { return xor(ws(),empty()) }
function popArgs() { arguments.length--; return arguments; }
function seq() {
  return arguments.length == 1
    ? arguments[0]
    : both(arguments[0], 
      arguments.length > 2
        ? seq.apply(null, Array.prototype.slice.call(arguments, 1))
        : arguments[1]
      );
}
function alt() {
  return arguments.length == 1
    ? arguments[0]
    : either(arguments[0], 
      arguments.length > 2
        ? alt.apply(null, Array.prototype.slice.call(arguments, 1))
        : arguments[1]
      );
}
function oneof() { // exclusive-or. only tested for deterministic patterns
  return arguments.length == 1
    ? arguments[0]
    : xor(arguments[0], 
      arguments.length > 2
        ? oneof.apply(null, Array.prototype.slice.call(arguments, 1))
        : arguments[1]
      );
}
function xor(l,r) {
  var res = either(l,r);
  res.xor = true;
  res.regen = function(grammar) { return xor(this.l.regen(grammar),this.r.regen(grammar)) };
  res.toString = function() { return 'xor('+this.l+','+this.r+')' };
  //res.b = false; // mark myself as deterministic, since I am.
  return res;
}
//var xor = either;
function lits() {
  var args = Array(arguments.length);
  for(var i=0;i<arguments.length;++i)
    args[i] = lit(arguments[i]);
  return alt.apply(null, args);
}

function notchars() {
  return seq(neglook(lits.apply(null, arguments)),dot());
}

var g = Grammar.defaultGrammar = new Grammar('sprixel$$defaultGrammar');

g.addPattern('alpha', ranges("0041-005A0061-007A00AA00B500BA00C0-00D600D8-00F600F8-02C102C6-02D102E0-02E402EC02EE0370-037403760377037A-037D03860388-038A038C038E-03A103A3-03F503F7-0481048A-05230531-055605590561-058705D0-05EA05F0-05F20621-064A066E066F0671-06D306D506E506E606EE06EF06FA-06FC06FF07100712-072F074D-07A507B107CA-07EA07F407F507FA0904-0939093D09500958-096109710972097B-097F0985-098C098F09900993-09A809AA-09B009B209B6-09B909BD09CE09DC09DD09DF-09E109F009F10A05-0A0A0A0F0A100A13-0A280A2A-0A300A320A330A350A360A380A390A59-0A5C0A5E0A72-0A740A85-0A8D0A8F-0A910A93-0AA80AAA-0AB00AB20AB30AB5-0AB90ABD0AD00AE00AE10B05-0B0C0B0F0B100B13-0B280B2A-0B300B320B330B35-0B390B3D0B5C0B5D0B5F-0B610B710B830B85-0B8A0B8E-0B900B92-0B950B990B9A0B9C0B9E0B9F0BA30BA40BA8-0BAA0BAE-0BB90BD00C05-0C0C0C0E-0C100C12-0C280C2A-0C330C35-0C390C3D0C580C590C600C610C85-0C8C0C8E-0C900C92-0CA80CAA-0CB30CB5-0CB90CBD0CDE0CE00CE10D05-0D0C0D0E-0D100D12-0D280D2A-0D390D3D0D600D610D7A-0D7F0D85-0D960D9A-0DB10DB3-0DBB0DBD0DC0-0DC60E01-0E300E320E330E40-0E460E810E820E840E870E880E8A0E8D0E94-0E970E99-0E9F0EA1-0EA30EA50EA70EAA0EAB0EAD-0EB00EB20EB30EBD0EC0-0EC40EC60EDC0EDD0F000F40-0F470F49-0F6C0F88-0F8B1000-102A103F1050-1055105A-105D106110651066106E-10701075-1081108E10A0-10C510D0-10FA10FC1100-1159115F-11A211A8-11F91200-1248124A-124D1250-12561258125A-125D1260-1288128A-128D1290-12B012B2-12B512B8-12BE12C012C2-12C512C8-12D612D8-13101312-13151318-135A1380-138F13A0-13F41401-166C166F-16761681-169A16A0-16EA1700-170C170E-17111720-17311740-17511760-176C176E-17701780-17B317D717DC1820-18771880-18A818AA1900-191C1950-196D1970-19741980-19A919C1-19C71A00-1A161B05-1B331B45-1B4B1B83-1BA01BAE1BAF1C00-1C231C4D-1C4F1C5A-1C7D1D00-1DBF1E00-1F151F18-1F1D1F20-1F451F48-1F4D1F50-1F571F591F5B1F5D1F5F-1F7D1F80-1FB41FB6-1FBC1FBE1FC2-1FC41FC6-1FCC1FD0-1FD31FD6-1FDB1FE0-1FEC1FF2-1FF41FF6-1FFC2071207F2090-209421022107210A-211321152119-211D212421262128212A-212D212F-2139213C-213F2145-2149214E218321842C00-2C2E2C30-2C5E2C60-2C6F2C71-2C7D2C80-2CE42D00-2D252D30-2D652D6F2D80-2D962DA0-2DA62DA8-2DAE2DB0-2DB62DB8-2DBE2DC0-2DC62DC8-2DCE2DD0-2DD62DD8-2DDE2E2F300530063031-3035303B303C3041-3096309D-309F30A1-30FA30FC-30FF3105-312D3131-318E31A0-31B731F0-31FF34004DB54E009FC3A000-A48CA500-A60CA610-A61FA62AA62BA640-A65FA662-A66EA67F-A697A717-A71FA722-A788A78BA78CA7FB-A801A803-A805A807-A80AA80C-A822A840-A873A882-A8B3A90A-A925A930-A946AA00-AA28AA40-AA42AA44-AA4BAC00D7A3F900-FA2DFA30-FA6AFA70-FAD9FB00-FB06FB13-FB17FB1DFB1F-FB28FB2A-FB36FB38-FB3CFB3EFB40FB41FB43FB44FB46-FBB1FBD3-FD3DFD50-FD8FFD92-FDC7FDF0-FDFBFE70-FE74FE76-FEFCFF21-FF3AFF41-FF5AFF66-FFBEFFC2-FFC7FFCA-FFCFFFD2-FFD7FFDA-FFDC"));
g.addPattern('ident', seq(pref('alpha'),star(pref('\\w'))));
g.addPattern('\\w', oneof(pref('alpha'),range('0','9'),cc('_')));
g.addPattern('upper', range('A','Z'));
g.addPattern('lower', range('a','z'));
g.addPattern('digit', range('0','9'));
g.addPattern('xdigit', oneof(range('0','9'),range('a','f'),range('A','F')));


var dbg = 0;

var g = new Grammar('wp6'); // wannabe Perl 6.  heh.

//var input = utf32str("|^^|<[a..z]>?");

g.addPattern('TOP', seq(pref('statement_list'),end()));

g.addPattern('pblock', seq(
  xopt(seq(
    pref('lambda')
    //,    pref('signature')
  )),
  pref('blockoid')
));

g.addPattern('lambda', lits('->','<->'));

g.addPattern('xblock', seq(
  expr(),
  ws(),
  pref('block')
));

g.addPattern('block', seq(
  pref('blockoid')
));

g.addPattern('blockoid', seq(
  cc('{'),
  pref('statement_list'),
  ows(),
  cc('}'),
  commit()
));

g.addPattern('statement_list', seq(
  star(xor(seq(pref('stmt_sep'),commit()),ws())),
  xopt(pref('statement')),
  star(seq(
    alt(
      seq(lb('}\n'),star(xor(pref('stmt_sep'),ws()))),
      plus(xor(pref('stmt_sep'),ws()))
    ),
    opt(pref('statement')),
    commit()
  ))
));

g.addPattern('stmt_sep', seq(ows(), cc(';')));

g.addPattern('\\s', cc(' ','\t','\n'));

g.addPattern('\\d', range('0','9'));

g.addPattern('ws', plus(xor(plus(pref('\\s')),both(lit('#'),star(notchar('\n'))))));

g.addPattern('apostrophe', cc("'",'-'));

g.addPattern('identifier', seq(
  pref('ident'),
  star(seq(
    xopt(cc("'")),
    pref('ident')
  ))
));

g.addPattern('label', seq(
  pref('identifier'),
  cc(':')
));

g.addPattern('statement', seq(
  neglook(cc(']',')','}')), // don't need to look for end; that occurs far too infrequently
  ows(),
  alt(
    seq(pref('label'),pref('statement')),
    pref('statement_control'),
    seq(
      expr(),
      ows(),
      oneof(
        seq(pref('statement_mod_cond'),opt(pref('statement_mod_loop'))),
        pref('statement_mod_loop'),
        empty()
      )
    )
  )
));

g.addPattern('statement_mod_cond', seq(
  lits('if','unless','when'),
  ws(),
  expr()
));

g.addPattern('statement_mod_loop', seq(
  lits('while','until','for','given'),
  ws(),
  expr()
));

function expr() { return lit('hi') }

g.addProto('statement_control');

g.addToProto('statement_control', 'if', seq(
  psym(),
  ws(),
  pref('xblock'),
  star(seq(ows(),lit('elsif'),ws(),pref('xblock'))),
  xopt(seq(ows(),lit('else'),ows(),pref('pblock')))
));

g.addToProto('statement_control', 'unless', seq(
  psym(),
  ws(),
  pref('xblock')
));

g.addToProto('statement_control', 'while', seq(
  lits('while','until'),
  ws(),
  pref('xblock')
));

g.addToProto('statement_control', 'repeat', seq(
  psym(),
  ws(),
  alt(
    seq(lits('while','until'),ws(),pref('xblock')),
    seq(pref('pblock'),ws(),lits('while','until'),ws(),expr())
  )
));

g.addToProto('statement_control', 'loop', seq(
  psym(),
  ws(),
  xopt(seq(
    cc('('),
    xopt(expr()),
    ows(),
    cc(';'),
    xopt(expr()),
    ows(),
    cc(';'),
    xopt(expr()),
    ows(),
    cc(')')
  )),
  ws(),
  pref('block')
));

g.addToProto('statement_control', 'for', seq(
  psym(),
  ws(),
  pref('xblock')
));

g.addToProto('statement_control', 'given', seq(
  psym(),
  ws(),
  pref('xblock')
));

g.addToProto('statement_control', 'when', seq(
  psym(),
  ws(),
  pref('xblock')
));

g.addToProto('statement_control', 'CATCH', seq(
  psym(),
  ws(),
  pref('block')
));

g.addToProto('statement_control', 'CONTROL', seq(
  psym(),
  ws(),
  pref('block')
));

g.addPattern('termishes', seq(
  ows(),
  opt(lits('||','|','&&','&')),
  action(empty(), 'm.c.termishes=[]'),
  action(pref('termish'), 'm.c.termishes.push(m.c.termish)'),
  action(empty(), 'print("zz: "+m.c.termishes.length+" "+o)'),
  star(seq(lits('||','|'),action(pref('termish'), 'm.c.termishes.push(m.c.termish)'))),
  action(empty(), 'print("yy: "+m.c.termishes.length+" "+o)')
));

g.addProto('quantifier');

g.addToProto('quantifier', '+', psym());
g.addToProto('quantifier', '?', psym());
g.addToProto('quantifier', '**', seq(
  psym(),
  ows(),
  pref('backmod'),
  ows(),
  either(seq(group(plus(pref('\\d')),'min'),
    opt(seq(lit('..'),group(alt(plus(pref('\\d')),lit('*')),'max')))
  ),pref('quantified_atom'))));
g.addToProto('quantifier', '*', psym());

g.addPattern('termish', plus(pref('quantified_atom')));

g.addPattern('quantified_atom', seq(pref('atom'),ows(),
  opt(seq(either(pref('quantifier'),seq(poslook(lit(':')),pref('backmod'),
    neglook(pref('alpha'))))))));

g.addPattern('alpha', alt(range('a','z'),range('A','Z')));

g.addPattern('back$w', alt(pref('alpha'),range('0','9'),cc('_')));

g.addPattern('atom', alt(
  seq(pref('back$w'),opt(seq(plus(pref('back$w')),poslook(pref('back$w'))))),
  pref('metachar')));

g.addPattern('backmod', seq(opt(cc(':')),alt(cc('?','!'),neglook(cc(':')))));

g.addProto('metachar');

//g.addToProto('metachar', 'ws', pref('normspace'));
g.addToProto('metachar', '[ ]', seq(cc('['), pref('termishes'), lit(']')));
g.addToProto('metachar', '( )', seq(cc('('), pref('termishes'), lit(')')));
g.addToProto('metachar', '\'', seq(poslook(lit('\'')), pref('termishes'), lit(')')));
g.addToProto('metachar', '.', psym());
g.addToProto('metachar', '^^', psym());
g.addToProto('metachar', '^', psym());
g.addToProto('metachar', '$$', psym());
g.addToProto('metachar', '$', psym());
g.addToProto('metachar', 'lwb', lits('<<','«'));
g.addToProto('metachar', 'rwb', lits('>>','»'));
g.addToProto('metachar', 'bs', seq(cc('\\'), pref('backslash')));

g.addToProto('metachar', 'assert', seq(cc('<'), pref('assertion'), cc('>')));

g.addProto('backslash');

g.addToProto('backslash', 'w', lit('nn'));

g.addProto('assertion');

g.addToProto('assertion', '?', seq(psym(), alt(poslook(cc('>')), pref('assertion'))));

g.addToProto('assertion', '!', seq(psym(), alt(poslook(cc('>')), pref('assertion'))));

g.addToProto('assertion', 'method', seq(cc('.'), pref('assertion')));

g.addToProto('assertion', 'name', seq(plus(pref('back$w')), opt(alt(
  poslook(cc('>')),
  seq(cc('='), pref('assertion'))//,
  //seq(lit(':'), pref('arglist')),
  //seq(lit('('), pref('arglist'), lit(')')),
  //seq(ows(), pref('termishes'))
))));

g.addToProto('assertion', '[', seq(poslook(cc('[','+','-')), plus(pref('cclass_elem'))));

g.addPattern('cclass_elem', seq(
  opt(cc('+','-')),
  ows(),
  alt(seq(
    //action(empty(), 'print("in cclass_elem")'),
    cc('['),
    star(seq(
      ows(),
      alt(seq(cc('\\'),dot()), notchars(']','\\')),
      opt(seq(ows()), lit('..'), ows(), dot())
    )),
    ows(),
    cc(']')
  ), plus(pref('back$w'))),
  ows()
));
/*
var g = new Grammar('doublectest');
g.addPattern('TOP', calt(alt(seq(lit('if'),doublec(),lit('not')),lit('ify'))));
g.compile(); g.parse(utf32str('ify'));

var g = new Grammar('doublectest');
g.addPattern('TOP', alt(calt(alt(seq(lit('if'),doublec(),lit('not')),lit('ify'))),lit('ify')));
g.compile(); g.parse(utf32str('ify'));

var g = new Grammar('singlectest');
g.addPattern('TOP', seq(singlec(star(cc('a'))),cc('a')));
g.compile(); g.parse(utf32str('aa'));

var g = new Grammar('committest');
g.addPattern('TOP', seq(star(cc('a')),commit(),cc('a')));
g.compile(); g.parse(utf32str('aa'));

var g = new Grammar('priortest');
g.addPattern('TOP', star(cc('a')));
g.compile(); g.parse(utf32str('aa'));
var g = new Grammar('priortest');
g.addPattern('TOP', repeat(prior(),2,2));
g.compile(); g.parse(utf32str('aaaa'));



dbg = 0;
var g = new Grammar('priortest');
g.addPattern('TOP', both(star(pref('alpha')),end()));
g.compile(); g.parse(utf32str('aaD'));


var g = new Grammar('wp6'); // wannabe Perl 6.  heh.

var input = utf32str("|^^|[a..z]?");

g.addPattern('TOP', seq(pref("termishes"),end()));

g.addPattern('termishes', seq(
  opt(lits('||','|','&&','&')),
  action(empty(), 'm.c.termishes=[]'),
  action(pref('termish'), 'm.c.termishes.push(m.c.termish)'),
  action(empty(), 'print("zz: "+m.c.termishes.length+" "+o)'),
  star(seq(lits('||','|'),pref('termish'))),
  action(empty(), 'print("yy: "+m.c.termishes.length+" "+o)')
));

g.addPattern('termish', lits('^^', '[a..z]?'));

*/
dbg = 0;
var input = utf32str(" ; ; ;;;;;    ; if hi {\n  ; hi  ; ; ;;;;;    ; if hi { ; ; ;;;;;  \n  ; hi  ; ; ;;;;;    ; ;}elsif hi {} else{  ; ; ;;;;;    ; ;}  ;;}elsif hi {} else{  ; ; ;;;;;    ; ;}\nif hi {hi}");

var sw = new Date();
g.compile();
print('Compile Time Elapsed: '+(new Date() - sw)+' ms');

var sw = new Date();
var m = g.parse(input);
print('Parse Time Elapsed: '+(new Date() - sw)+' ms');

var sw = new Date();
var m = g.parse(input);
print('Parse Time Elapsed: '+(new Date() - sw)+' ms');

print('parser function is '+(g.parser.toString().length)+' chars long.');
print('input text is '+(input.l)+' chars long.');

//print(keys(m.match.m.c));


//print(g.parser.toString());

/*
for (var te=1; te<10000000000000; te*=2) {
  var sw = new Date();
  //var code = 'switch(a){';
  var code = '';
  var a = func(['a'], []);
  for (var tf=0; tf<te; ++tf)
    //code+='case '+tf+':return '+tf+';';
    //code+='if (a=='+tf+')return '+tf+';';
    code+=';';
  //a.r.push(val(code+'}'));
  a.r.push(val('return a;'));
  var c = eval(a.emit());
  print(te+' compiled: '+(new Date() - sw)+' ms');
  c(te);
  print(te+'    executed: '+(new Date() - sw)+' ms');
  c(te);
  print(te+'       executed: '+(new Date() - sw)+' ms');
}
*/