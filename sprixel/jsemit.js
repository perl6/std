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
  return function(obj, parent) {
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
}
gts.prototype.u = false;
gts.prototype.computeRefs = function(pats, name, refs) {
  if (typeof this.l != 'undefined')
    this.l.computeRefs(pats, name, refs);
  if (typeof this.r != 'undefined')
    this.r.computeRefs(pats, name, refs);
}
gts.prototype.regen = function(grammar) {
  return this.root(
    this.l ? this.l.regen(grammar) : (this.v ? this.v.str : this.lo ? this.lo.str : null ),
    this.r ? this.r.regen(grammar) : this.hi ? this.hi.str : null);
}

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
  //if (!lbl)
  //  print(arguments.callee.caller);
  return val("gl="+lbl.id+";break");
}

function casel(lbl) { // macro for MARK label with id of the transition object
  return val("case "+lbl.id+":");
}

function lit(literal) { return new glit(literal) }
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
}
glit.prototype.root = lit;

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
    cond(lor(ge(o,val("i.l")),lor(gt(val(this.lo[0]),val("i[o]")),lt(val(this.hi[0]),val("i[o]")))),[[
      gotol(this.fail)
    ],[
      val("o+=1")
    ]])
  );
};
grange.prototype.toString = function() {
  return 'range('+this.lo.str.toQuotedString()+','+this.hi.str.toQuotedString()+')';
}
grange.prototype.root = range;

function gend() { // grammar "end anchor" parser builder
  gts.call(this); // call the parent constructor
  this.b = false;
}
derives(gend, gts);
gend.prototype.emit = function(c) {
  c.r.push(cond(ne(o,val("i.l")),[[
    gotol(this.fail)
  ]]));
};
function end() { return new gend() }
gend.prototype.toString = function() {
  return 'end()';
}
gend.prototype.root = end;

function gdot() { // grammar "any char (dot)" parser builder
  gts.call(this); // call the parent constructor
  this.b = false;
}
derives(gdot, gts);
gdot.prototype.emit = function(c) {
  c.r.push(
    cond(ge(o,val("i.l")),[[
      gotol(this.fail)
    ]]),
    val("++o")
  );
};
function dot() { return new gdot() }
gdot.prototype.toString = function() {
  return 'dot()';
}
gdot.prototype.root = dot;

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
    var clone = pat.regen(c.g);
    clone.fail = this.fail; // fixup the label references
    if (clone.b) {
      clone.notd = this.notd;
      clone.done = this.done;
      clone.init = this.init;
      clone.bt = this.bt;
    }
    clone.emit(c);
    if (!clone.b) // some combinators aren't aligned perfectly
      c.r.push(gotol(this.done));
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
  refs[this.name] = true;
  if (!hasIt && name != this.name) // prevent cyclical recursion
    pats[this.name].computeRefs(pats, name, refs);
};
function pref(name) { return new gpref(name) }
gpref.prototype.toString = function() {
  return 'gpref('+this.name.toQuotedString()+')';
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
}
gempty.prototype.root = empty;

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
}

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
}

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
    val("t=t.r"),
    gotol(this.r.bt),
    
    casel(this.r.fail), // right failed
    a, // ascend
    gotol(this.fail) // goto our fail
  );
};
gbothrs.prototype.root = both;
gbothrs.prototype.toString = function() {
  return 'both('+this.l+','+this.r+')';
}

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
  this.l.fail = this.r.fail;
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
    
    casel(this.bt), // backtrack entry point
    cond(val("(!t.ld)"),[[
      val("o=t.s;t=t.l"),
      gotol(this.l.bt)
    ]]),
    val("t=t.r;o=t.s"),
    gotol(this.r.bt),
    
    casel(this.r.fail), // right failed
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
}

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
    gotol(this.notd), // return "not done"
    
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
}

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
  var rinit = new gtr();
  c.r.push(
    casel(this.init),d // initial entry point
  );
  this.l.emit(c);
  c.r.push(
    casel(this.l.done), // left succeeded with finality
    a, // ascend
    val("t.ld=true"),
    gotol(this.notd), // return "not done"
    
    casel(this.l.notd), // left succeeded, but is not done.
    val("t.i.l=t;t=t.i"), // stash left in myself; ascend
    val("t.ld=false"),
    gotol(this.notd), // return "not done"
    
    casel(this.bt), // backtrack entry point
    cond(val("(t.ld)"),[[
      gotol(rinit)
    ],[
      val("t=t.l"),
      gotol(this.l.bt)
    ]]),
    
    casel(this.r.fail), // right failed
    a, // ascend
    gotol(this.fail), // goto our fail
    
    casel(this.l.fail), // left failed
    ros,
    casel(rinit) // right initial entry point
  );
  this.r.emit(c);
  c.r.push(
    gotol(this.done)
  );
};
geitherls.prototype.root = either;
geitherls.prototype.toString = function() {
  return 'either('+this.l+','+this.r+')';
}

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
    gotol(this.notd),
    
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
    gotol(this.notd), // return "not done"
    
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
}

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
    gotol(this.notd), // return "not done"
    
    casel(this.l.notd), // left succeeded, but is not done.
    val("t.i.l=t"),
    a, // ascend
    val("t.ld=false"), // mark that r has not been inited
    gotol(this.notd), // return "not done"
    
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
    gotol(this.notd), // return "not done"
    
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
}

function plus(l) {
  return l.b
    ? new gplusb(l) // nondeterministic version of plus
    : new gplus(l); // deterministic version of plus
}

function gplus(l) { // grammar "deterministic" edition of plus
  gts.call(this, this.l = l); // call the parent constructor
  this.b = true;
  this.init = new gtr(); // add a transition record for my initial entry point
  this.bt = new gtr(); // add a transition record for my backtrack entry point
  this.notd = new gtr();
  this.done = new gtr();
}
derives(gplus, gts);
gplus.prototype.emit = function(c) {
  var retry = new gtr(), check = new gtr(); // label for retry spot
  var lname = "l"+this.init.id;
  c.r.push(
    val("var "+lname),
    casel(this.init),
    cond(val("(("+lname+"||("+lname+"={}))[o])"),[[
      gotol(this.fail)
    ]]),
    val(lname+"[o]=true"),
    d,
    val("t.z=[]"), // create a container for the match offsets
    casel(retry)
  );
  this.l.emit(c);
  c.r.push( // left succeeded
    cond(val("(t.s==o)"),[[ // child is a zero-length assertion
      gotol(this.done)
    ]]),
    val("t.z.push(o)"),
    gotol(retry), // try another one
    
    casel(this.l.fail), // left hit its base case
    cond(val("(t.z.length==0)"),[[
      val(lname+"[o]=true"),
      a,
      gotol(this.fail)
    ]]),
    casel(check),
    cond(val("(t.z.length==1)"),[[
      val("o=t.z.pop()"),
      val(lname+"[o]=true"),
      gotol(this.done)
    ]]),
    val(lname+"[o]=true"),
    gotol(this.notd),
    
    casel(this.bt),
    val("o=t.z.pop()"),
    cond(val("(t.z.length>0)"),[[
      gotol(check)
    ]]),
    a,
    gotol(this.fail)
  );
};
gplus.prototype.root = plus;
gplus.prototype.toString = function() {
  return 'plus('+this.l+')';
}

function gplusb(l) { // grammar "deterministic" edition of plus
  gts.call(this, this.l = l); // call the parent constructor
  this.b = true;
  this.init = new gtr(); // add a transition record for my initial entry point
  this.bt = new gtr(); // add a transition record for my backtrack entry point
  this.notd = new gtr();
  this.done = new gtr();
}
derives(gplusb, gts);
gplusb.prototype.emit = function(c) {
  var retry = new gtr(); // label for retry spot
  c.r.push(
    casel(this.init),d,
    val("t.z8=[];t.ret={}"), // create containers for the child objects
    casel(retry)
  );
  this.l.emit(c);
  c.r.push(
    casel(this.l.done), // left succeeded
    val("t.i.z8.push(t);t.nd=false"),
    //cond(val("(t.s==o)"),[[ // child is a zero-length assertion
    //  gotol(this.done)
    //]]), // TODO: write tests for whether this is necessary
    cond(val("(t.i.ret[o])"),[[
      a,
      gotol(this.bt)
    ]]),
    a,
    gotol(retry), // try another one
    
    casel(this.l.notd), // left succeeded
    val("t.i.z8.push(t);t.nd=true"),
    cond(val("(t.i.ret[o])"),[[
      a,
      gotol(this.bt)
    ]]),
    a,
    gotol(retry), // try another one
    
    casel(this.l.fail), // left hit its base case
    cond(val("(t.z8.length==0)"),[[
      a,
      gotol(this.fail)
    ]]),
    
    cond(val("(t.z8.length==1&&t.z8[0].nd==false)"),[[
      gotol(this.done)
    ]]),
    val("t.ret[o]=true"),
    gotol(this.notd),
    
    casel(this.bt),
    val("u=t.z8.pop()"),
    cond(val("(u.nd)"),[[
      val("o=u.s"),
      cond(val("(t.ret[o])"),[[
        gotol(this.bt)
      ]]),
      val("t=u"),
      gotol(this.l.bt)
    ]]),
    cond(val("(t.z8.length==0)"),[[
      a,
      gotol(this.fail)
    ]]),
    val("o=u.s"),
    cond(val("(t.z8.length==1&&t.z8[0].nd==false)"),[[
      gotol(this.done)
    ]]),
    cond(val("(t.ret[o])"),[[
      gotol(this.bt)
    ]]),
    val("t.ret[o]=true"),
    gotol(this.notd)
  );
};
gplusb.prototype.root = plus;
gplusb.prototype.toString = function() {
  return 'plus('+this.l+')';
}

function star(l) {
  var r = either(plus(l),empty());
  r.toString = function() {
    return 'star('+l+')';
  };
  r.regen = function(grammar) {
    return new star(this.l.l);
  }
  return r;
}

function utf32str_charAt(offset) {
  return String.fromCharCode(this[offset]);
}

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
  this.pats = parent
    ? derive(parent.pats)
    : {}; // namespace for patterns
  this.isRoot = !parent; // boolify its definedness
  this.parent = parent;
  this.name = name;
}
Grammar.prototype.addPattern = function(name, pattern) {
  if (this.pats.hasOwnProperty(name))
    throw 'Pattern '+name+' already defined in grammar '+this.name;
  this.pats[name] = pattern;
  if (typeof this.TOP == 'undefined')
    this.TOP = pattern;
  return pattern;
}
var resolutionActivated = false;
Grammar.prototype.compile = function(interpreterState) {
  // recursively compute full set of named references for each 
  //   pattern, preventing descent into a cyclical reference.
  for (var name in this.pats) {
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
      // wrap the pattern in a call target
      this.pats[name] = new gcallt(pat);
      this.pats[name].isRecursive = true;
      if (pat===this.TOP)
        this.TOP = this.pats[name]; // fixup the TOP one
    }
    /*// discover/mark whether the pattern has any references at all
    resolutionActivated = false;
    pat.cur();*/
  }
  
  var routine = dbg
    ? func(["i"],[val("var gl=0,o=0,t={},c;t.i=t;last:for(;;){print('op: '+gl+' '+o);next:switch(gl){case -2:")])
    : func(["i"],[val("var gl=0,o=0,t={},c;t.i=t;last:for(;;){next:switch(gl){case -2:")]); // empty parser routine

  var g = this.TOP;

  //g = g.regen();

  g.fail = new gtr(); g.fail.id = 1;
  g.done = g.notd = new gtr(); g.done.id = -1;
  
  routine.g = this;
  
  // second pass: resolve & emit the named references, now that
  //   we know which ones are recursive.
  for (var name in this.pats) {
    var pat = this.pats[name];
    if (pat.isRecursive && pat!==this.TOP) {
      pat.emit(routine);
    }
  }
  
  routine.r.push(val("case 0:"));
  
  g.emit(routine); // have the grammar emit its specialize parser code to this routine

  routine.r.push(val("case -1:/*print('parse succeeded');*/break last;case 1:default:print('parse failed');break last}}")); // finalize the routine

  var parserf = routine.emit();

  if (dbg)
    print(parserf);

  this.parse = eval(parserf); // compile the javascript function to machine code

  if (dbg)
    print('compiled');
}
Grammar.prototype.parse = function(input) {
  this.compile();
  return this.parse(input);
}

var dbg = 0;

var g = new Grammar('wp6');

g.addPattern('stuff', either(both(dot(),pref('stuff')),dot()));
g.addPattern('final anchor', end());
g.TOP = g.addPattern('toplevel', both(pref('stuff'),end()));

var input = utf32str(Array(1<<10).join('\uffff'));

g.parse(input);


















