/*
  This software is copyright 2009 Matthew Wilson matthew@wilson.org
  and available under the Artistic License 2 ("AL2") as documented at
  http://jsmeta.googlecode.com
*/

function DefaultTokenize(){ this.result = this.result.isArray ? this.result.flatten().join('') : this.result; }
function Tokenize(){ this.result = this.result.isArray ? this.result.flatten().join('') : this.result; }

Module(function parserLoader(l) {
  
  l.__languagePath = 'Base.Parser.Combinators';
  
  var dummyForTextEditor = function() {};
  var maxInt = 1.7976931348623157e+308;
  
  Func(l, function both() {
    var t = this, first = this.first, second = this.second, returnTo;
    switch (this.phase) {
      
      case 0:
      this.index = l.idx;
      first = this.first = this.args[0].reactivate(this);
      second = this.second = this.args[1].reactivate(this);
      this.phase = 1;
      /*if (returnTo = this.checkFollowSet(l, first) || this.checkFollowSet(l, second, true)) {
        this.outcome = false;
        l.idx = this.index;
        this.advanced = 0;
        this.done = true;
        return this.invoker;
      }*/
      return first;
      
      case 1:
      if (first.outcome) {
        this.phase = 2;
        return second;
      } else {
        this.outcome = false;
        l.idx = this.index;
        this.advanced = 0;
        this.done = true;
        return this.invoker;
      }
      
      case 2:
      if (second.outcome) {
        this.deterministic = first.deterministic && second.deterministic;
        this.advanced = first.advanced + second.advanced;
        if (!second.done) {
          this.done = false;
          this.phase = 4;
        } else if (!first.done) {
          this.done = false;
          this.phase = 3;
        } else {
          this.done = true;
        }
        this.result = new Array(first.result, second.result);
        this.vars = {};
        if (typeof(first.vars)!='undefined') {
          for (var i in first.vars) {
            this.vars[i] = first.vars[i];
          }
        }
        if (typeof(second.vars)!='undefined') {
          this.vars = this.vars || {};
          for (var i in second.vars) {
            if (typeof(first.vars)=='undefined' || typeof(first.vars[i])=='undefined' || first.vars[i]=='') {
              this.vars[i] = second.vars[i];
            }
          }
        }
        if (typeof(first.label)!='undefined' /*&& (!this.vars || typeof(this.vars[first.label])=='undefined')*/) {
          (this.vars = this.vars || {})[first.label] = first.result;
        }
        if (typeof(second.label)!='undefined' /*&& (!this.vars || typeof(this.vars[second.label])=='undefined')*/) {
          (this.vars = this.vars || {})[second.label] = second.result;
        }
        this.doPostDo();
        return this.invoker;
      } else if (first.done) {
        this.deterministic = first.deterministic && second.deterministic;
        this.outcome = false;
        this.advanced = 0;
        this.done = true;
        return this.invoker;
      }
      // intentionally fall through to phase 3
      
      case 3:
      l.idx = this.index;
      //if (returnTo = this.checkFollowSet(l, second, true)) {
      //  return returnTo;
      //}
      this.phase = 1;
      second = this.second = this.args[1].reactivate(this);
      second.cachedStr = false;
      return first;
      
      case 4:
      l.idx = second.index;
      this.phase = 2;
      return second;
    }
  });
  
  Func(l, function either() {
    var first = this.first, second = this.second;
    switch (this.phase) {
    
      case 0:
      this.index = l.idx;
      this.first = this.args[0].reactivate(this);
      this.second = this.args[1].reactivate(this);
      this.phase = 1;
      return this.first;
      
      case 1: if (first.outcome) {
        this.done = false;
        this.phase = first.done ? 2 : 3;
        this.result = first.result;
        this.vars = first.vars;
        //this.label = first.label;
        this.advanced = first.advanced;
        this.doPostDo();
        return this.invoker;
      }
      // intentionally fall through to case 2.
      
      case 2:
      l.idx = this.index;
      if (first.finalized || second.finalized) {
        throw 'Parse failed after '+first.str;
      }
      this.phase = 4;
      return second;
      
      case 3:
      l.idx = this.index;
      this.phase = 1;
      return first;
      
      case 4: if (second.outcome) {
        this.done = second.done;
        this.phase = 2;
        this.result = second.result;
        this.vars = second.vars;
        //this.label = second.label;
        this.advanced = second.advanced;
        this.doPostDo();
        return this.invoker;
      } else {
        this.outcome = false;
        l.idx = this.index;
        this.advanced = 0;
        this.done = true;
        return this.invoker;
      }
    }
  });
  
  
  Func(l, function altRE() {
    var fullPattern, res, matched = false;
    switch (this.phase) {
    
      case 0:
      this.phase = 1;
      this.remaining = l.len - (this.index = l.idx);
      this.prefixedPattern = '^' + (this.index ? '(?:[\\S\\s]{' + this.index + '})' : '') + '(' + this.args[0] + ')';
      this.backoff = 0;
      this.failedStr = this.args[0].replace(/\?\:/g, '');
      fullPattern = this.prefixedPattern + '$';
      //print(fullPattern);
      while (true) {
        if (res = new RegExp(fullPattern).exec(l.input.str)) {
          this.result = typeof(res[1])=='undefined' ? '' : res[1];
          if (this.backoff++ == this.remaining && this.result.length > 0) {
            this.result = false;
          } else {
            matched = true;
          }
          break;
        } else if (this.backoff + this.index < l.len) {
          this.backoff++;
        } else {
          break;
        }
        fullPattern = this.prefixedPattern + '(?:[\\S\\s]{' + this.backoff + '})$';
        //print(fullPattern);
      }
      if (matched) {
        l.idx = this.index + (this.advanced = this.result.length);
        this.doPostDo();
        this.deterministic = false;
        this.done = this.result.length ? false : true;
        this.backoff++;
      } else {
        this.outcome = false;
        this.deterministic = true;
      }
      return this.invoker;
      
      case 1:
      fullPattern = this.prefixedPattern + '(?:[\\S\\s]{' + this.backoff + '})$';
      //print(fullPattern);
      while (true) {
        if (res = new RegExp(fullPattern).exec(l.input.str)) {
          this.result = typeof(res[1])=='undefined' ? '' : res[1];
          if (this.backoff++ == this.remaining && this.result.length > 0) {
            this.result = false;
          } else {
            matched = true;
          }
          break;
        } else if (this.backoff + this.index < l.len) {
          this.backoff++;
        } else {
          break;
        }
        fullPattern = this.prefixedPattern + '(?:[\\S\\s]{' + this.backoff + '})$';
        //print(fullPattern);
      }
      if (matched) {
        l.idx = this.index + (this.advanced = this.result.length);
        this.doPostDo();
        this.done = this.result.length ? false : true;
      } else {
        this.done = true;
        this.outcome = false;
      }
      return this.invoker;
    }
  });
  
  
  Func(l, function xor() {
    var first = this.first, second = this.second;
    switch (this.phase) {
    
      case 0:
      this.index = l.idx;
      this.first = this.args[0].reactivate(this);
      this.second = this.args[1].reactivate(this);
      this.phase = 1;
      this.firstSucceeded = false;
      return this.first;
      
      case 1: if (first.outcome) {
        this.firstSucceeded = true;
        this.done = false;
        this.phase = first.done ? 2 : 3;
        this.result = first.result;
        this.vars = first.vars;
        //this.label = first.label;
        this.advanced = first.advanced;
        this.doPostDo();
        return this.invoker;
      }
      // intentionally fall through to case 2.
      
      case 2:
      if (this.firstSucceeded) {
        this.outcome = false;
        l.idx = this.index;
        this.advanced = 0;
        this.done = true;
        return this.invoker;
      }
      l.idx = this.index;
      this.phase = 4;
      if (/^die/.test(second.str)) {
        second.failedActivation = first;
      }
      return second;
      
      case 3:
      l.idx = this.index;
      this.phase = 1;
      return first;
      
      case 4: if (second.outcome) {
        this.done = second.done;
        this.phase = 2;
        this.result = second.result;
        this.vars = second.vars;
        //this.label = second.label;
        this.advanced = second.advanced;
        this.doPostDo();
        return this.invoker;
      } else {
        this.outcome = false;
        l.idx = this.index;
        this.advanced = 0;
        this.done = true;
        return this.invoker;
      }
    }
  });
  
  Func(l, function converseImplication() {
    var first = this.first, second = this.second;
    switch (this.phase) {
    
      case 0:
      this.index = l.idx;
      this.first = this.args[0].reactivate(this);
      this.second = this.args[1].reactivate(this);
      this.phase = 1;
      this.firstSucceeded = false;
      return this.first;
      
      case 1: if (first.outcome) {
        this.firstSucceeded = true;
        this.done = false;
        this.phase = first.done ? 2 : 3;
        this.result = first.result;
        this.vars = first.vars;
        //this.label = first.label;
        this.advanced = first.advanced;
        this.doPostDo();
        return this.invoker;
      }
      // intentionally fall through to case 2.
      
      case 2:
      if (!this.firstSucceeded) {
        this.outcome = false;
        l.idx = this.index;
        this.advanced = 0;
        this.done = true;
        return this.invoker;
      }
      l.idx = this.index;
      this.phase = 4;
      return second;
      
      case 3:
      l.idx = this.index;
      this.phase = 1;
      return first;
      
      case 4: if (second.outcome) {
        this.done = second.done;
        this.phase = 2;
        this.result = second.result;
        this.vars = second.vars;
        //this.label = second.label;
        this.advanced = second.advanced;
        this.doPostDo();
        return this.invoker;
      } else {
        this.outcome = false;
        l.idx = this.index;
        this.advanced = 0;
        this.done = true;
        return this.invoker;
      }
    }
  });
  
  Func(l, function empty(isDeterministic) {
    this.index = l.idx;
    this.phase++;
    this.result = '';
    this.doPostDo();
    return this.invoker;
  });
  
  Func(l, function notAfter(isDeterministic) { // a.k.a. negative lookbehind
    this.phase++;
    this.index = l.idx;
    this.result = '';
    if (this.index) {
      var re = new RegExp((this.args[0] instanceof Func.Act ? this.args[0].args[0] : this.args[0]) + '(?:[\\S\\s]{'+(l.len - this.index)+'})$', 'g');
      this.outcome = !re.exec(l.input.str);
    }
    return this.invoker;
  });
  
  Func(l, function end(isDeterministic) {
    this.phase++;
    this.index = l.idx;
    this.result = '';
    if (this.outcome = l.idx >= l.len) {
      this.doPostDo();
    }
    return this.invoker;
  });
  
  Func(l, function regexp(isDeterministic) {
    this.phase++;
    this.index = l.idx;
    this.result = '';
    this.outcome = false;
    var re = new RegExp('^' + (this.index ? '(?:[\\S\\s]{'+this.index+'})' : '') + (this.args[0] instanceof Func.Act ? this.args[0].args[0] : this.args[0]), 'g');
    var res = re.exec(l.input.str);
    var result = (res || [''])[0];
    if (result.length && result.length > this.index) {
      if (this.index) {
        result = result.split('').slice(this.index).join('');
      }
      this.result = result;
      this.outcome = true;
      l.idx = this.index + (this.advanced = result.length);
      this.doPostDo();
    }
    return this.invoker;
  });
  
  Func(l, function regexpE(isDeterministic) {
    this.phase++;
    this.index = l.idx;
    this.result = '';
    this.outcome = false;
    var re = new RegExp('^' + (this.index ? '(?:[\\S\\s]{'+this.index+'})' : '') + (this.args[0] instanceof Func.Act ? this.args[0].args[0] : this.args[0]));
    var res = re.exec(l.input.str);
    if (res) {
      var result = res[0];
      if (this.index) {
        result = result.split('').slice(this.index).join('');
      }
      this.result = result;
      this.outcome = true;
      l.idx += (this.advanced = result.length);
      this.doPostDo();
    };
    return this.invoker;
  });
  
  Func(l, function any(isDeterministic) {
    this.phase++;
    var a = this.args;
    var n = l.input, x = l.idx;
    this.index = x;
    if (typeof(this.outcome = l.input[x])=='undefined') {
      this.outcome = false;
      return this.invoker;
    }
    if (a.length) {
      var v, m;
      for (var i = 0, j = a.length; i < j; i++) {
        if ((m = (v = (a[i] instanceof Func.Act ? a[i].args[0] : a[i])).length) <= l.len - x && (this.result = n.str.substring(x, x+m))==v) {
          l.idx += (this.advanced = m);
          this.doPostDo();
          return this.invoker;
        }
      }
      this.outcome = false;
      return this.invoker;
    }
    this.result = n[x];
    l.idx += (this.advanced = 1);
    this.doPostDo();
    return this.invoker;
  });
  
  Func(l, function none(isDeterministic) {
    this.phase++;
    var a = this.args,
        j = a.length,
        zz = new Array(j);
    for (var i = 0; i < j; i++) {
      zz[i] = a[i] instanceof Func.Act ? a[i].args[0] : a[i];
    }
    if (!((this.result = l.input[this.index = l.idx])!==undefined) || Array_prototype_indexOf.call(zz, this.result) !== -1) {
      this.outcome = false;
    } else {
      l.idx++;
      this.advanced = 1;
      this.doPostDo();
    }
    return this.invoker;
  });
  
  Func.Rule(l, function error() {
    var toTry = this.args[0].reactivate(this);
    return l.xor(toTry, l.die(toTry, typeof(this.args[1])!='undefined' ? this.args[1].args[0] : undefined));
  });
  
  Func.Rule(l, function die() {
    var succeededInput = l.input.slice(0, l.idx).join('');
    var succeededLines = succeededInput.split(/\r\n|\n/);
    var lineNumber = succeededLines.length;
    var lastLine = succeededLines[succeededLines.length - 1];
    var colNumber = lastLine.length + 1;
    var failedPoint = 'line ' + lineNumber + ', column ' + colNumber;
    if (typeof(this.failedActivation)!='undefined' &&
      (typeof(l.__lastFailed)=='undefined' || l.__lastFailed.index < this.failedActivation.index)) {
      l.__lastFailed = this.failedActivation;
    }
    throw (new String(typeof(this.args[1])!='undefined'
      ? this.args[1]
      : 'Missing '+this.failedActivation.str+' at '+failedPoint)).failedAt(l.idx);
  });
  
  Func(l, function fail(isDeterministic) {
    this.phase++;
    this.index = l.idx;
    this.result = '';
    this.outcome = false;
    return this.invoker;
  });
  
  Func(l, function charRange(isDeterministic) {
    this.phase++;
    this.index = l.idx;
    var min = (this.args[0] instanceof Func.Act ? this.args[0].args[0] : this.args[0]);
    var max = (this.args[1] instanceof Func.Act ? this.args[1].args[0] : this.args[1]);
    if (l.input===undefined || l.idx >= l.len || (this.result = l.input[l.idx]).charCodeAt(0) < min.charCodeAt(0) || this.result.charCodeAt(0) > max.charCodeAt(0)) {
      this.outcome = false;
      this.result = '';
    } else {
      l.idx++;
      this.advanced = 1;
      this.doPostDo();
    }
    return this.invoker;
  });
  
  Func(l, function not() {
    var first = this.first;
    switch (this.phase) {
    
      case 0:
      this.index = l.idx;
      this.phase = 1;
      this.first = this.args[0].reactivate(this);
      this.first.invoker = this;
      return this.first;
      
      case 1:
      this.deterministic = first.deterministic;
      if (first.outcome) {
        this.outcome = false;
        l.idx = this.index;
      } else {
        this.doPostDo();
        this.result = '';
      }
      return this.invoker;
    }
  });
  
  Func(l, function lookahead() {
    var first = this.first;
    switch (this.phase) {
    
      case 0:
      this.index = l.idx;
      this.phase = 1;
      this.first = this.args[0].reactivate(this);
      this.first.invoker = this;
      return this.first;
      
      case 1:
      this.deterministic = first.deterministic;
      l.idx = this.index;
      if (first.outcome) {
        this.result = '';
        // this.result = first.result; // TODO: revisit this... may need to use results of lookahead somehow.
        this.doPostDo();
      } else {
        this.outcome = false;
      }
      return this.invoker;
    }
  });
  
  Func.Rule(l, function alt() {
    var a = this.args;
    return l.either(
      a.length > 2
        ? l.alt.apply(l, popArgs.apply(null,a))
        : a[0].reactivate(this)
      , a[a.length - 1]/*.reactivate(this)*/);
  });
  
  Func.Rule(l, function seq() {
    var a = this.args;
    return a.length == 1
      ? a[0].reactivate(this)
      : l.both(
        a.length > 2
          ? l.seq.apply(l, popArgs.apply(null,a))
          : a[0]/*.reactivate(this)*/
        , a[a.length - 1]/*.reactivate(this)*/);
  });
  
  Func.Rule(l, function nor() {
    return l.both(l.not(this.args[0]),l.not(this.args[1]));
  });
  
  Func.Rule(l, function xnor() {
    return l.either(l.both(this.args[0], this.args[1]), l.nor(this.args[0],this.args[1]));
  });
  
  Func.Rule(l, function repeatBetween() {
    var a = this.args;
    a[1] = Math.min(l.len - l.idx, a[1]);
    return a[0]
      ? a[0] >= a[1]
        ? l.repeat(a[0], a[2])
        : l.converseImplication(l.repeat(a[0], a[2].reactivate(this)), l.repeatBetween((a[0]*1) + 1, a[1], a[2]))
      : l.either(l.empty(), l.repeatBetween((a[0]*1) + 1, a[1], a[2]));
  });
  
  l.oneOrMore = Func(l, function plus() {
    var acts = this.acts,
        last = this.last
       tried = this.tried;
    switch (this.phase) {
    
      case 0:
      this.index = l.idx;
      this.acts = [last = this.last = this.args[0].reactivate(this)];
      tried = this.tried = {};
      tried[l.idx] = last;
      /*if (last.computeRecogRE(l)!==false && this.checkFollowSet(l, last)) {
        this.outcome = last.outcome = false;
        this.done = last.done = true;
        last.returned++;
        this.deterministic = last.deterministic = true;
        return this.invoker;
      }*/
      this.phase = 1;
      return last;
      
      // greedily consuming
      case 1: if (!last.outcome || l.idx >= l.len) {
        // found _a_ base case (could have more following this location)
        if (!last.outcome) {
          acts.pop();
        }
        var len;
        this.result = new Array(len = acts.length);
        this.vars = {};
        if (len) {
          this.done = len <= 1 && acts[0].done;
          for (var i = 0; i < len; i++) {
            this.result[i] = (last = acts[i]).result;
            if (typeof(last.vars)!='undefined') {
              for (var j in last.vars) {
                this.vars[j] = last.vars[j];
              }
            }
          }
          l.idx = this.index + (this.advanced = (this.last = last).index + last.advanced - this.index);
          this.doPostDo();
          this.phase = 2; //backtrack
        } else {
          l.idx = this.index;
          this.result = '';
          this.advanced = 0;
          this.done = true;
          this.outcome = false;
        }
        return this.invoker;
      } else {
        if (l.idx < l.len) { // !tried[l.idx]) {
          acts.push(tried[l.idx] = last = this.last = this.args[0].reactivate(this));
          return last;
        }
      }
      
      case 2:
      if (!last.done) {
        l.idx = last.index;
        this.phase = 1; // consume more if this one succeeds
        return last;
      }
      acts.pop(); // it failed us for the last time
      this.result = new Array(len = acts.length);
      this.vars = {};
      if (len) {
        this.done = len <= 1 && acts[0].done;
        for (var i = 0; i < len; i++) {
          this.result[i] = (last = acts[i]).result;
          if (typeof(last.vars)!='undefined') {
            for (var j in last.vars) {
              this.vars[j] = last.vars[j];
            }
          }
        }
        l.idx = this.index + (this.advanced = last.index + last.advanced - this.index);
        this.doPostDo();
      } else {
        l.idx = this.index;
        this.result = '';
        this.advanced = 0;
        this.done = true;
        this.outcome = false;
      }
      return this.invoker;
      
    }
  });
  
  l.zeroOrMore = Func(l, function star() {
    var acts = this.acts,
        last = this.last
       tried = this.tried;
    switch (this.phase) {
    
      case 0:
      this.index = l.idx;
      this.acts = [last = this.last = this.args[0].reactivate(this)];
      tried = this.tried = {};
      tried[l.idx] = last;
      /*if (last.computeRecogRE(l)!==false && this.checkFollowSet(l, last)) {
        last.outcome = false;
        this.outcome = true;
        last.done = true;
        this.result = '';
        last.returned++;
        this.deterministic = last.deterministic = true;
        return this.invoker;
      }*/
      this.phase = 1;
      return last;
      
      // greedily consuming
      case 1: if (!last.outcome /* || !last.advanced */ ) {
        // found _a_ base case (could have more following this location)
        acts.pop();
        var len;
        this.result = new Array(len = acts.length);
        this.vars = {};
        if (len) {
          this.done = len <= 1 && acts[0].done;
          for (var i = 0; i < len; i++) {
            this.result[i] = (last = acts[i]).result;
            if (typeof(last.vars)!='undefined') {
              for (var j in last.vars) {
                this.vars[j] = last.vars[j];
              }
            }
          }
          l.idx = this.index + (this.advanced = (this.last = last).index + last.advanced - this.index);
          this.doPostDo();
          this.phase = 2; //backtrack
        } else {
          l.idx = this.index;
          this.result = '';
          this.advanced = 0;
          this.done = true;
          this.outcome = true;
        }
        return this.invoker;
      } else {
        if (!tried[l.idx]) {
          acts.push(tried[l.idx] = last = this.last = this.args[0].reactivate(this));
          return last;
        }
      }
      
      case 2:
      if (!last.done) {
        l.idx = last.index;
        this.phase = 1; // consume more if this one succeeds
        return last;
      }
      acts.pop(); // it failed us for the last time
      this.result = new Array(len = acts.length);
      this.vars = {};
      if (len) {
        this.done = len <= 1 && acts[0].done;
        for (var i = 0; i < len; i++) {
          this.result[i] = (last = acts[i]).result;
          if (typeof(last.vars)!='undefined') {
            for (var j in last.vars) {
              this.vars[j] = last.vars[j];
            }
          }
        }
        l.idx = this.index + (this.advanced = last.index + last.advanced - this.index);
        this.doPostDo();
      } else {
        l.idx = this.index;
        this.result = '';
        this.advanced = 0;
        this.done = true;
        this.outcome = true;
      }
      return this.invoker;
      
    }
  });
  
  l.optional = Func.Rule(l, function opt() {
    if (this.args[0].str=='il()') {
      return l.either(l.regexp('(?:' + this.args[0].computeRecogRE(l) + ')'), l.empty());
    }
    return l.either(this.args[0]/*.reactivate(this)*/, l.empty());
  });
  
  Func.Rule(l, function optR() {
    if (this.args[0].str=='il()') {
      return l.either(l.empty(), l.regexp('(?:' + this.args[0].computeRecogRE(l) + ')'));
    }
    return l.either(l.empty(), this.args[0]/*.reactivate(this)*/);
  });
  
  Func.Rule(l, function difference() {
    return l.both(l.not(this.args[1]/*.reactivate(this)*/), this.args[0]/*.reactivate(this)*/);
  });
  
  Func.Rule(l, function repeat() {
    var a = this.args;
    return a[0]
      ? a[0] > 1
        ? l.both(a[1]/*.reactivate(this)*/, l.repeat(a[0] - 1, a[1]/*.reactivate(this)*/))
        : a[1].reactivate(this)
      : l.empty();
  }, function repeatDeterministic() {
    return this[1].deterministic;
  });
  
  Func.Rule(l, function matchAll() {
    return l.useFirst(l.both(this.args[0], l.opil(l.end())));
  });
  
  l.project = function project(production, projection) {
    if (!production.postDo) {
      production.postDo = [projection];
    } else if (production.postDo.length==1 && production.postDo[0]===DefaultTokenize) {
      production.postDo[0] = projection;
    } else {
      production.postDo.push(projection);
    }
    return production;
  };
  
  l.projectFirst = function projectFirst(production, projection) {
    if (!production.postDo) {
      production.postDo = [projection];
    } else if (production.postDo.length==1 && production.postDo[0]===DefaultTokenize) {
      production.postDo[0] = projection;
    } else {
      production.postDo.unshift(projection);
    }
    return production;
  };
  
  l.bind = function bind(label, activation) {
    activation.label = label;
    return activation;
  };
  
  l.bindLast = function bindLast(label, activation) {
    return l.project(activation, function() { this.label = label });
  };
  
  l.arrayWrap = function arrayWrap() {
    this.result = [this.result];
  };
  
  l.arrayWrapRaw = function arrayWrapRaw() {
    this.result = [this.rawResult];
  };
  
  l.valuesof = function valuesof(anArray) {
    anArray.flattenValues = true;
    return anArray;
  };
  
  function Flattener() {
    this.result = this.result.isArray ? this.result.flatten() : [this.result];
  }
  
  l.flatten = function flatten(production) {
    return l.project(production, Flattener);
  };
  
  function UseRaw() { this.result = this.rawResult }
  
  l.resultRaw = function resultRaw(production) {
    return l.project(production, UseRaw);
  };
  
  l.resultRawArray = function resultRawArray(production) {
    return l.project(production, arrayWrapRaw);
  };
  
  l.array = function arrayify() {
    var positions = [];
    var members = Array.prototype.slice.call(arguments, 0);
    for (var i = 0; i < members.length; i++) {
      if (typeof(members[i].flattenValues)!='undefined') {
        positions.push(i);
      }
    }
    return positions.length
      ? Array.prototype.valuesOfAtPositions.apply(members, positions)
      : members;
  };
  
  /**
   * sequence with interleave(s)
   */
  Func.Rule(l, function iseq() {
    var a = this.args;
    return l.both(
      a.length > 2
        ? l.iseq.apply(l, popArgs.apply(null,a))
        : l.pil(a[0]/*.reactivate(this)*/)
      , l.pil(a[a.length - 1]/*.reactivate(this)*/));
  });
  
  /**
   * sequence with optional interleave(s)
   */
  Func.Rule(l, function oiseq() {
    var a = this.args;
    return l.both(
      a.length > 2
        ? l.oiseq.apply(l, popArgs.apply(null,a))
        : l.opil(a[0]/*.reactivate(this)*/)
      , l.opil(a[a.length - 1]/*.reactivate(this)*/));
  });
  
  /**
   * sequence with optional interleave(s), with syntax labeling.
   */
  Func.Rule(l, function syntax() {
    var a = this.args;
    return a.length > 1
      ? l.both(a.length > 2
        ? l.oiseq.apply(l, popArgs.apply(null,a))
        : l.opil(a[0]/*.reactivate(this)*/)
      , l.opil(a[a.length - 1]/*.reactivate(this)*/))
      : l.opil(a[0]/*.reactivate(this)*/);
  });
  
  /**
   * production with preceding interleave(s)
   */
  Func.Rule(l, function pil() {
    return l.useSecond(l.both(l.il(), this.args[0]/*.reactivate(this)*/));
  });
  
  /**
   * production with following interleave(s)
   */
  Func.Rule(l, function fil() {
    return l.useFirst(l.both(this.args[0]/*.reactivate(this)*/, l.il()));
  });
  
  /**
   * production with optional preceding interleave(s)
   */
  Func.Rule(l, function opil() {
    if (/^(oiplus|oistar|oiseq|oiplusSep|opil|syntax)\(/.test(this.args[0].str)
        || l.__interleaveStandIn.computeRecogRE(l)=='(?:)') {
      return this.args[0]/*.reactivate(this)*/;
    }
    return l.useSecond(l.both(l.__optInterleaveStandIn/*.reactivate(this)*/, this.args[0]/*.reactivate(this)*/));
  });
  
  /**
   * optional repeated sequence with optional interleave(s)
   */
  Func.Rule(l, function oistar() {
    return l.star(l.useSecond(l.both(l.__optInterleaveStandIn/*.reactivate(this)*/, this.args[0]/*.reactivate(this)*/)));
  });
  
  /**
   * repeated sequence with optional interleave(s)
   */
  Func.Rule(l, function oiplus() {
    if (/^(oiplus|oistar|oiseq|oiplusSep|opil)\(/.test(this.args[0].str)
        || l.__interleaveStandIn.computeRecogRE(l)=='(?:)') {
      return l.plus(this.args[0]/*.reactivate(this)*/);
    }
    return l.plus(l.useSecond(l.both(l.__optInterleaveStandIn/*.reactivate(this)*/, this.args[0]/*.reactivate(this)*/)));
  });
  
  function TrimEmptyLast() { if (this.result[this.result.length - 1] == '') this.result.length--; }
  
  /**
   * repeated sequence with optional interleave(s) and a separator
   */
  Func.Rule(l, function oiplusSep() {
    return l.flatten(l.project(
      l.both(
        l.opil(this.args[1]/*.reactivate(this)*/),
        l.either(l.plus(
          l.useSecond(l.both(
            l.opil(this.args[0]/*.reactivate(this)*/)
          , l.opil(this.args[1].reactivate(this)))))
        , l.empty()))
    , TrimEmptyLast));
  });
  
  /**
   * the actual interleave production
   */
  Func.Rule(l, function il() {
    var interleaves, len;
    if (typeof(l.interleaves)=='undefined' || !l.interleaves.length) {
      return l.fail();
    } else {
      interleaves = new Array(len = l.interleaves.length);
      //if (len==1) { return l.interleaves[0]; }
      for (var i = 0; i < len; i++) {
        interleaves[i] = (l.interleaves[i])();
      }
    }
    return len > 1
      ? len > 2
        ? l.plus(l.alt.apply(l, interleaves))
        : l.plus(l.either.apply(l, interleaves))
      : interleaves[0];
  });
  
  /**
   * installs a new interleave production in the current language
   */
  l.Interleave = function Interleave(newInterleaveProduction) {
    if (typeof(l.interleaves)=='undefined') {
      l.interleaves = [newInterleaveProduction];
    } else {
      l.interleaves.push(newInterleaveProduction);
    }
  }
  
  function walkActivationTree(predicate, application) {
    
  }
  
  Func.Rule(l, function token() {
//    var recogRE;
    var first = this.args[0].reactivate(this);
    //if ((recogRE = first.computeRecogRE(l)) && !first.containsProjection()) {
    //  return l.project(l.altRE(recogRE), Tokenize);
    //}
    return l.project(first, Tokenize);
  });
  
  Func.Rule(l, function deferred() {
    return this.deferredRule.apply(this.deferredRule, this.args);
  });
  
  Func.Rule(l, function proxied() {
    var deferredActivation = l.deferred();
    deferredActivation.deferredRule = this.args[0].reactivate(this);
    return deferredActivation;
  });
  
  Func.Rule(l, function resolveByName() {
    var rule;
    var ruleNameHolder = this.args[0],
         myScope = ruleNameHolder.myScope,
         ourScope,
         ruleName = ruleNameHolder.str;
    if (myScope.__currentModule && myScope.__currentLanguage) {
      ourScope = myScope[myScope.__currentModule][myScope.__currentLanguage];
    }
    var ruleRefArgs = [];
    var a = this.args[1];
    for (var i = 0; i < a.length; i++) {
      if ((rule = a[i]) instanceof Func.Act) {
        ruleRefArgs[i] = rule.reactivate(this);
        //rule.isArgument = false;
      } else if (typeof(rule)=='function') {
        if (rule.isDispatcher) {
          ruleRefArgs[i] = rule();
        } else {
          ruleRefArgs[i] = rule(l);
        }
      } else {
        // "auto-vivify" literals to literal "any()" activations
        ruleRefArgs[i] = l.any(rule);
      }
    }
    var result;
    if (typeof(rule = this.resolveVar(ruleName))!='undefined') {
      if (rule instanceof Func.Act) {
        result = rule.reactivate(this);
      } else if (typeof(rule)=='function') {
        result = rule.isDispatcher
          ? rule.apply(l, ruleRefArgs)
          : rule(l);
      } else if (rule!==null) {
        // "auto-vivify" literals to literal "any()" activations
        result = l.any(rule);
      }
    }
    var scope, name;
    if (!result) {
      scope = myScope;
      name = ruleName;
      while (/^\w+\.\w/.test(name)) {
        name = /^(\w+)\.(\w.*)$/.exec(name);
        if (typeof(scope[name[1]])!='undefined') {
          scope = scope[name[1]];
          name = name[2];
        }
      }
      if (typeof(scope[name])=='function') {
        //if (traceOn)
        //  Print('resolved to function '+Function.prototype.toString.call(scope[name])
        //    +' with args '+ruleRefArgs);
        result = scope[name].apply(l, ruleRefArgs);
      }
    }
    if (!result && ourScope) {
      scope = ourScope;
      name = ruleName;
      while (/^\w+\.\w/.test(name)) {
        name = /^(\w+)\.(\w.*)$/.exec(name);
        if (typeof(scope[name[1]])!='undefined') {
          scope = scope[name[1]];
          name = name[2];
        }
      }
      if (typeof(scope[name])=='function') {
        //if (traceOn)
        //  Print('resolved to function '+scope[name]);
        result = scope[name].apply(l, ruleRefArgs);
      }
    }
    if (!result) {
      scope = l;
      name = ruleName;
      while (/^\w+\.\w/.test(name)) {
        name = /^(\w+)\.(\w.*)$/.exec(name);
        if (typeof(scope[name[1]])!='undefined') {
          scope = scope[name[1]];
          name = name[2];
        }
      }
      if (typeof(scope[name])=='function') {
        //if (traceOn)
        //  Print('resolved to function '+scope[name]);
        result = scope[name].apply(l, ruleRefArgs);
      }
    }
    if (!result) {
      throw 'failed to resolve ' + ruleName + ' in '+myScope;
    }
    //if (traceOn)
    //  Print(this.actId+' resolved to '+result);
    //alert(this.actId+' resolved to '+result);
    return result;
  });
  
  function UseSecond() { this.result = this.second.result }
  
  l.useSecond = function(activation) {
    return l.project(activation, UseSecond);
  };
  
  function UseFirst() { this.result = this.first.result }
  
  l.useFirst = function(activation) {
    return l.project(activation, UseFirst);
  };
  
  function Finalize() { this.finalize(); }
  
  l.tokenizeDefault = function(activation) {
    if (!activation.postDo || !activation.postDo.length) {
      activation.postDo = [DefaultTokenize];
    }
    return activation;
  };
  
  l.finalize = function(activation) {
    return activation;
    //return l.project(activation, Finalize);
  };
  
  Func.Rule(l, function keywordDeclaration() { return l.useSecond(l.both(
    l.opil(l.keyword(this.args[0]))
  , this.args[1]/*.reactivate(this)*/
  ))});
  
  Func.Rule(l, function keyword() { return l.useFirst(l.both(
    l.any(this.args[0])
  , l.lookahead(l.regexpE('(?![\\w_])'))
  ))});
  
  function ClearLexicalScope() {
    //Print(['cleared scope of '+this,this.label]);
    this.vars = {};
  }
  
  function ClearLexicalScopeAndLabel() {
    //Print(['cleared scope and label of '+this,this.label]);
    this.vars = {};
    this.label = undefined;
  }
  
  function ClearLexicalScopeOfResultingRule() {
    if (typeof(this.result)=='function') {
      var ruleGenerator = this.result;
      this.result = function() {
        var rule = ruleGenerator.apply(this, arguments);
        //Print([rule, rule.label]);
        return l.projectFirst(rule, ClearLexicalScopeAndLabel);
      };
    }
  }
  
  l.clearLexicalScope = function clearLexicalScope(activation) {
    return l.projectFirst(activation, ClearLexicalScope);
  };
  
  l.clearLexicalScopeOfResultingRule = function clearLexicalScopeOfResultingRule(activation) {
    return l.project(activation, ClearLexicalScopeOfResultingRule);
  };
  
}, false, true);



























