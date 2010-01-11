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
    this.r ? this.r.regen(grammar) : this.hi ? this.hi.str : null );
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

function notchar(literal) { return new gnotchar(literal) }
function gnotchar(literal) { // grammar literal parser builder
  gts.call(this); // call the parent constructor
  this.b = false;
  this.v = utf32str(literal);
}
derives(gnotchar, gts);
gnotchar.prototype.emit = function(c) {
  c.r.push(cond(land(ne(o,val("i.l")),ne(val("i.substr(o,"+this.v.l+")"),val(this.v.str.toQuotedString()))),[[
    add_assign(o,val(this.v.l))
  ],[
    gotol(this.fail)
  ]]));
};
gnotchar.prototype.toString = function() {
  return 'notchar('+this.v.str.toQuotedString()+')';
}
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

function group(l,name) { return new ggroup(l,name) }
function ggroup(l,name) { // grammar "group (named & anonymous)" parser builder
  gts.call(this); // call the parent constructor
  if (name === null)
    throw arguments.callee.caller;
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
    casel(this.init),
    val("m={m:m,s:o,c:{},i:[]"+(
      this.anon ? "" : ",n:"+this.name.toQuotedString()
    )+"};m.m.c["+this.name.toQuotedString()+"]=m") // append to the end of the match linked list
  );
  this.l.emit(c);
  if (this.b) { // need these iff we're backtrackable (non-deterministic)
    c.r.push(
      casel(this.l.done),
      val("for(var q in m.c){if(!m.m.c.hasOwnProperty(q))m.m.c[q]=m.c[q]};m=m.m"),
      gotol(this.done),
      
      casel(this.l.notd),
      val("for(var q in m.c){if(!m.m.c.hasOwnProperty(q))m.m.c[q]=m.c[q]};m=m.m"),
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
  this.compiled = false;
}
Grammar.prototype.addPattern = function(name, pattern) {
  pattern = group(pattern,name);
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
    /*// discover/mark whether the pattern has any references at all
    resolutionActivated = false;
    pat.cur();*/
  }
  
  var routine = dbg
    ? func(["i","g","gl","o","t"],[val("var m={t:t,c:{}};m.m=m;t.i=t;last:for(;;){print('op: '+gl+' '+o);next:switch(gl){case -4:")])
    : func(["i","g","gl","o","t"],[val("var m={t:t,c:{}};m.m=m;t.i=t;last:for(;;){next:switch(gl){case -4:")]); // empty parser routine

  var g = this.TOP;

  //g = g.regen();

  g.fail = new gtr(); g.fail.id = 1;
  g.done = new gtr(); g.done.id = -1;
  g.notd = new gtr(); g.notd.id = -2;
  g.bt = new gtr(); g.bt.id = -3;
  
  routine.g = this;
  
  // second pass: resolve & emit the named references, now that
  //   we know which ones are recursive.
  for (var name in this.pats) {
    var pat = this.pats[name];
    if (pat.isRecursive && pat!==this.TOP) {
      if (dbg)
        print('compiling pattern '+name.toQuotedString());
      pat.emit(routine);
    }
  }
  
  if (dbg)
    print('done compiling patterns');
  
  routine.r.push(val("case 0:"));
  
  g.emit(routine); // have the grammar emit its specialize parser code to this routine

  routine.r.push(val("case -1:if(dbg)print('parse succeeded');return new Match(t,m.m,g,i,o,true);case -2:if(dbg)print('parse succeeded, but is not completed');return new Match(t,m.m,g,i,o,false);case 1:default:print('parse failed');break last}}")); // finalize the routine

  var parserf = routine.emit();

  if (dbg)
    print(parserf);

  this.parser = eval(parserf); // compile the javascript function to machine code
  
  if (dbg)
    print('compiled');
  
  this.compiled = true;
}
Grammar.prototype.parse = function(input) {
  if (!this.compiled)
    this.compile();
  return this.parser(input,this,0,0,{});
}
Grammar.prototype.next = function(input,t,o) {
  return this.parser(input,this,-3,o,t);
}

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
}

// steal some macros/combinators from jsmeta
function opt(l) { return either(l,empty()) }
function ws() { return pref('ws') }
function poil(l) { return both(opt(ws()),l) }
function foil(l) { return both(l,opt(ws())) }
function pfoil(l) { return poil(foil(l)) }
function oisep(l,sep) { return both(pfoil(l),star(both(pfoil(sep),pfoil(l.regen())))) }
function oiplus(l) { return both(pfoil(l),star(pfoil(l.regen()))) }
function ows() { return opt(ws()) }
function popArgs() { arguments.length--; return arguments; }
function seq() {
  return arguments.length == 1
    ? arguments[0]
    : both(
      arguments.length > 2
        ? seq.apply(null, popArgs.apply(null,arguments))
        : arguments[0]
      , arguments[arguments.length - 1]);
}
function alt() {
  return arguments.length == 1
    ? arguments[0]
    : either(
      arguments.length > 2
        ? alt.apply(null, popArgs.apply(null,arguments))
        : arguments[0]
      , arguments[arguments.length - 1]);
}
function lits() {
  var args = Array(arguments.length);
  for(var i=0;i<arguments.length;++i)
    args[i] = lit(arguments[i]);
  return alt.apply(null, args);
}

var dbg = 0;

var g = new Grammar('wp6');

g.addPattern('toplevel', both(pref("grammar"),end()));

g.addPattern('ws', plus(either(lits(' ','\n','\t','\f','\r'),both(lit('#'),star(notchar('\n'))))));

g.addPattern('grammar', plus(pfoil(pref('pattern'))));

g.addPattern('pattern', seq(lit('{'),poslook(lit(' ')),poil(lit('}'))));

var input = utf32str("{ }\
# comment here \
   \
 # 'nother comment  \
{ }");

var m = g.parse(input);













