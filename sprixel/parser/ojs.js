/*
  This software is copyright 2009 Matthew Wilson matthew@wilson.org
  and available under the Artistic License 2 ("AL2") as documented at
  http://jsmeta.googlecode.com
*/

var Func;

(function FuncInitializer() {
  
  var deterministicDetector = /(isDeterministic)/;
  var arityDetector = /^[\s\(]*function\s*[^(]*\([^)]*hasArity(\d+)/;
  var funcNameFromStr = /^([^(]+)/;
  var _actId = 0;
  var _reTests = 0;
  var maxInt = 1.7976931348623157e+308;
  
  Func = function _Func(l, func) {
    var self;
    var str;
    var fName = (str = func.toString()).match(functionNameExtractor)[1];
    var deterministic = str.match(deterministicDetector) ? true : false;
    self = function activator() {
      return new Act(fName, func, arguments, false, deterministic);
    }
    return l[fName] = self;
  };
  Func.activations = function() { return _actId };
  Func._reTests = function() { return _reTests };
  
  Func.Tracked = function(l, func) {
    var self;
    var str;
    var fName = (str = func.toString()).match(functionNameExtractor)[1];
    var deterministic = str.match(deterministicDetector) ? true : false;
    self = function activator() {
      return new Act(fName, func, arguments, false, deterministic);
    }
    return l[fName] = self;
  };
  
  var computingIlRE = false;
  
  var lastComputeRE, thisComputeRE = false;
  
  var frozenFields = [ 'index', 'result', 'returned', 'advanced', 'phase' ];
  
  function addInvoker(target, invoker) {
    if (target && target instanceof Act && !target.invoker) {
      target.invoker = invoker;
    }
  }
  
  Function.prototype.isSurrogate = false;
  
  function Act(fName, func, actArgs, cloneStr, deterministic) {
    this.func = func;
    this.actId = _actId++;
    this.deterministic = deterministic ? true : false;
    this.args = actArgs;
    this.str = cloneStr || fName + toArgArray(actArgs);
    //Print(this.str);
    if (traceActivations) {
      actsRegistry[_actId - 1] = this;
    }
    //return this;
  }
  Act.prototype = {
    done: true
  , finalized: false
  , index: false
  , result: NaN
  , inside: false
  , first: false
  , second: false
  , postDo: false
  , seed: false
  , lrPhase: 0
  , seedAdvanced: 0
  , deterministic: false
  , outcome: true
  , returned: 0 // count times returned; if .returned===1 and .done==true, can use the memo at that index.
  , advanced: 0
  , phase: 0
  , containsProjection: function containsProjection() {
      return false;
    }
  , toStringExpanded: function toStringExpanded(invoker, localContext) {
      if (this.func && !this.func.isSurrogate) {
        return this.str;
      }
      if (!this.invoker) {
        this.invoker = invoker;
      }
      if (this.func && this.func.isSurrogate) {
        this.localContext = localContext;
      }
      return this.func().toStringExpanded(this, localContext);
    }
  , computeRecogRE: function computeRecogRE(localContext) {
      lastComputeRE = thisComputeRE; thisComputeRE = this;
      var funcName, res, tres, q, keyName, a = this.args, _gP;
      if (typeof(res = (_gP = localContext._gP)[keyName = this.str])!='undefined') {
        //if (res===false) {
        //  alert('returning lack of recogRE for '+keyName);
        //}
        if (keyName=='il()') {
          return this.fastRE = res;
        }
        return res;
      }
      funcName = this.str.match(funcNameFromStr)[0];
      _gP[keyName] = false; // catch recursion (for now!)
      res = '(?:';
      switch (funcName) {
        case 'regexp':
        case 'regexpE':
          addInvoker(a[0], this);
          return _gP[keyName] = a[0];
        case 'resolveByName':
          //if (computingIlRE || debugMode) {
            this.localContext = localContext;
            this.invoker = this.invoker || lastComputeRE;
            res = this.func();
            this.phase = 0;
            return _gP[keyName] = res.computeRecogRE(localContext);
          //} else {
          //  try {
          //    this.localContext = localContext;
          //    res = this.func();
          //    return _gP[keyName] = res.computeRecogRE(localContext);
          //  } catch (e) { // some can't be pre-resolved
          //    return _gP[keyName] = false;
          //  }
          //}
        case 'converseImplication':
        case 'either':
        case 'alt':
          q = a.length;
          for (var i=0; i<q; i++) {
            addInvoker(a[i], this);
            if (!(tres = a[i].computeRecogRE(localContext))) {
              return _gP[keyName] = false;
            }
            res += tres + '|';
          }
          return _gP[keyName] = res.chop() + ')';
        case 'charRange':
          addInvoker(a[0], this); addInvoker(a[1], this);
          return _gP[keyName] = '[' + a[0].replace(/[-[\]{}()*+?.^\\$|,#\s\u0085]/g, "\\$&") + '-' +
              a[1].replace(/[-[\]{}()*+?.^\\$|,#\s\u0085]/g, "\\$&") + ']';
        //case 'keyword':
        case 'any':
          q = a.length;
          for (var i=0; i<q; i++) {
            addInvoker(a[i], this);
            res += a[i].replace(/[-[\]{}()*+?.^\\$|#\u0085]|(?=\s)[^ ]/g, "\\$&") + '|';
          }
          var result2 = _gP[keyName] = q ? res.chop() + ')' : '(?:[\\S\\s])';
          //Print(this + '    ' + result2);
          return result2;
        case 'opt':
          addInvoker(a[0], this);
          if (a[0].str=='il()') {
            if (typeof(localContext.interleaves)=='undefined' || !localContext.interleaves.length) {
              return _gP[keyName] = '(?:)';
            }
            return _gP[keyName] = '(?:' + localContext.__interleaveStandIn.computeRecogRE(localContext) + '?)';
          }
          return _gP[keyName] = (tres = a[0].computeRecogRE(localContext))
            ? '(?:(?:' + tres + ')?)'
            : false;
        case 'plus':
          addInvoker(a[0], this);
          return _gP[keyName] = (tres = a[0].computeRecogRE(localContext))
            ? '(?:(?:' + tres + ')+)'
            : false;
        case 'star':
          addInvoker(a[0], this);
          return _gP[keyName] = (tres = a[0].computeRecogRE(localContext))
            ? '(?:(?:' + tres + ')*)'
            : false;
        case 'both':
          addInvoker(a[0], this); addInvoker(a[1], this);
          return _gP[keyName] = (res = a[0].computeRecogRE(localContext))
            && (thisComputeRE = this)
            && (tres = a[1].computeRecogRE(localContext))
              ? '(?:' + res + tres + ')'
              : false;
        case 'not':
          addInvoker(a[0], this);
          return _gP[keyName] = (tres = a[0].computeRecogRE(localContext))
              ? '(?!' + tres + ')'
              : false;
        case 'none':
          res = '(?:(?!';
          q = a.length;
          for (var i=0; i<q; i++) {
            addInvoker(a[i], this);
            res += a[i].replace(/[-[\]{}()*+?.^\\$|,#\s\u0085]/g, "\\$&") + '|';
          }
          return _gP[keyName] = q ? res.chop() + ')[\\S\\s])' : '(?:[\\S\\s])';
        case 'seq':
          q = a.length;
          for (var i=0; i<q; i++) {
            addInvoker(a[i], this);
            if (!(tres = a[i].computeRecogRE(localContext))) return _gP[keyName] = false;
            res += tres;
          }
          return _gP[keyName] = res + ')';
        case 'end': return _gP[keyName] = '$';
        case 'matchAll':
          addInvoker(a[0], this);
          return _gP[keyName] = '^'+a[0].computeRecogRE(localContext)
            + ((thisComputeRE = this) ? '' : '')
            + localContext.__interleaveStandIn.computeRecogRE(localContext)+'*$';
        //case 'empty': return _gP[keyName] = '(?:)';
        case 'iseq':
          q = a.length;
          for (var i=0; i<q; i++) {
            addInvoker(a[i], this);
            tres = a[i].computeRecogRE(localContext);
            thisComputeRE = this;
            if (!tres) return _gP[keyName] = false;
            res += '(?:' + localContext.__interleaveStandIn.computeRecogRE(localContext)
              + tres + ')';
          }
          return _gP[keyName] = res + ')';
        case 'oiseq':
          q = a.length;
          for (var i=0; i<q; i++) {
            addInvoker(a[i], this);
            tres = a[i].computeRecogRE(localContext);
            thisComputeRE = this;
            if (!tres) return _gP[keyName] = false;
            res += '(?:' + localContext.__optInterleaveStandIn.computeRecogRE(localContext)
              + tres + ')';
          }
          return _gP[keyName] = res + ')';
        case 'opil':
          addInvoker(a[0], this);
          if (localContext.__interleaveStandIn.computeRecogRE(localContext)=='(?:)') {
            thisComputeRE = this;
            return _gP[keyName] = a[0].computeRecogRE(localContext);
          }
          //if (/^(oiplus|oistar|oiseq|oiplusSep|opil)\(/.test(this.args[0].str)) return localContext.__optInterleaveStandIn.computeRecogRE(localContext);
          tres = a[0].computeRecogRE(localContext);
          thisComputeRE = this;
          res = _gP[keyName] = tres
            ? '(?:' + localContext.__optInterleaveStandIn.computeRecogRE(localContext)
              + tres + ')'
            : false;
          //print(res);
          return res;
        case 'pil':
          addInvoker(a[0], this);
          return _gP[keyName] = '(?:' + localContext.__interleaveStandIn.computeRecogRE(localContext)
            + ((thisComputeRE = this) ? '' : '')
            + a[0].computeRecogRE(localContext) + ')';
        case 'empty': return _gP[keyName] = '(?:())';
        //case 'token': return _gP[keyName] = a[0].computeRecogRE(localContext);
        case 'lookahead':
          addInvoker(a[0], this);
          return _gP[keyName] = (tres = a[0].computeRecogRE(localContext))
              ? '(?=' + tres + ')'
              : false;
        case 'il':
          if (typeof(localContext.interleaves)=='undefined' || !localContext.interleaves.length) {
            return _gP[keyName] = '(?:)';
          } else {
            computingIlRE = true;
            var len = localContext.interleaves.length;
            for (var i = 0; i < len; i++) {
              if (!(tres = (localContext.interleaves[i])().computeRecogRE(localContext))) {
                computingIlRE = false;
                throw 'interleave production must be deterministic and acyclical!';
                return _gP[keyName] = false;
              }
              res += tres + '|';
            }
            computingIlRE = false;
            return _gP[keyName] = (this.fastRE = '(?:' + res.chop() + ')+)');
          }
        case 'repeat':
          addInvoker(a[1], this);
          return _gP[keyName] = (tres = a[1].computeRecogRE(localContext))
              ? '(?:' + tres + (a[0]==1
                ? ''
                : a[0]==Infinity
                  ? '+'
                  : '{' + a[0] + '}') + ')'
              : false;
        // case 'repeatBetweenUp':
          // return _gP[keyName] = (tres = a[2].computeRecogRE(localContext))
              // ? '(?:(?:' + tres + '){' + a[0] + ',' + deInf(a[1]) + '})'
              // : false;
        // case 'repeatBetweenDown':
          // return _gP[keyName] = (tres = a[2].computeRecogRE(localContext))
              // ? '(?:(?:' + tres + '){' + a[1] + ',' + deInf(a[0]) + '})'
              // : false;
        case 'repeatBetween':
          addInvoker(a[2], this);
          return _gP[keyName] = (tres = a[2].computeRecogRE(localContext))
              ? '(?:(?:' + tres + '{' + (a[0] || 1) + (a[1]==Infinity ? '})+' : ',' + a[1] + '})') + (a[0] ? '' : '?') + ')'
              : false;
      }
      if (this.func.isSurrogate) {
        this.localContext = localContext;
        if (!this.invoker) {
          this.invoker = lastComputeRE;
        }
        if (!this.first) {
          this.func();
        }
        this.phase = 0;
        //Print(keyName+' returning surrogate RE '+this.first);
        return _gP[keyName] = this.first.computeRecogRE(localContext);
      }
      if (typeof(a[0])!='undefined' && typeof(a[1])=='undefined' && a[0] instanceof Act) {
        addInvoker(a[0], this);
        return _gP[keyName] = a[0].computeRecogRE(localContext);
      }
      //alert('missed a RE for '+this);
      return _gP[keyName] = false;
    }
  , checkFollowSet: function checkFollowSet(localContext, second, notFromStart) {
      if (ignoreDisqualifyingREs || this.deterministic) return false;
      var aheadRegExp, re, slot, result;
      if (typeof(result = (slot = (typeof(slot = localContext.__memoTable.REs[second.str])!='undefined' ? slot : (localContext.__memoTable.REs[second.str] = [])))[this.index])=='boolean') {
        if (!result) {
          if (ConsoleTrace) Print(this + ' disqualified at ' + localContext.input.slice(this.index, this.index+10).join('') + ' by cached RegExp check; called by '+arguments.callee.caller.name);
          if (traceOn) {
            callTraceNodeStatus(this.actId, 'Disqualified at ' + this.index + ' by cached RegExp check');
            callTraceNodeStatus(this.actId, 'looking ahead, did not find a match in: '+aheadRegExp.toString().toProgramString());
          }
          this.deterministic = true;
          this.outcome = false;
          this.advanced = 0;
          localContext.idx = this.index;
          this.done = true;
          return this.invoker;
        } else {
          if (traceOn) {
            callTraceNodeStatus(this.actId, 'looking ahead, found a possible match for '+second+' in the remaining input');
          }
        }
      }
      if (typeof(aheadRegExp = second.computeRecogRE(localContext))=='string') {
        if (traceOn) {
          callTraceNodeStatus(this.actId, 'looking ahead at input index '+this.index+' at '+second+' in regexp: '+aheadRegExp.toString().toProgramString());
        }
        _reTests++;
        if (!(slot[this.index] = new RegExp((aheadRegExp = (aheadRegExp.charAt(0)=='^' ? aheadRegExp : notFromStart ? '^(?:[\\S\\s]*)' : '^') + (this.index ? '[\\S\\s]' + (this.index==1 ? '' : '{' + this.index + '}') : '') + aheadRegExp,'m') /*&& aheadRegExp.length < 4095*/ ? aheadRegExp : '()').test(localContext.input.str))) {
          localContext.__lastFailedRE = aheadRegExp;
          //Print('\n failed on '+ second.toString().toProgramString() + ' at '+this.index+'\n' + aheadRegExp.toProgramString());
          if (ConsoleTrace) Print(this + ' disqualified at ' + localContext.input.slice(this.index, this.index+10).join('') + ' by FIRST RegExp check');
          if (traceOn) {
            callTraceNodeStatus(this.actId, 'Disqualified at ' + this.index + ' by RegExp check: '+aheadRegExp.toProgramString());
            //callTraceNodeStatus(this.actId, 'looking ahead, did not find a match in: '+aheadRegExp.toString().toProgramString());
          }
          this.deterministic = true;
          this.outcome = false;
          this.advanced = 0;
          localContext.idx = this.index;
          this.done = true;
          return this.invoker;
        //} else {
          //Print('\n succeeded on '+ second.toString().toProgramString() + ' at '+this.index+'\n' + aheadRegExp.toProgramString());
          //if (traceOn) {
          //  callTraceNodeStatus(this.actId, 'looking ahead, found a possible match for '+second+' in the remaining input');
          //}
        }
      }
      return false;
    }
  , finalize: function finalize() {
      this.deterministic = true;
      this.done = true;
      this.finalized = true;
      return this;
    }
  , doPostDo: function doPostDo() {
      if (this.postDo) {
        this.rawResult = this.result;
        for (var i = 0, j = this.postDo.length; i < j; i++) {
          if (traceOn) {
            callTraceNodeStatus(this.actId, 'rawResult: '+this.rawResult+' running postDo: '+this.postDo[i]);
            this.postDo[i].call(this);
            callTraceNodeStatus(this.actId, 'new result: '+this.result);
          } else {
            this.postDo[i].call(this);
          }
        }
      }
    }
  , doPostDoMemoization: function doPostDoMemoization(item) {
      if (this.postDo) {
        if (item.postDo && this.postDo.length==item.postDo.length) {
          var foundDifference = false;
          for (var i = 0, j = this.postDo.length; i < j; i++) {
            if (Function.prototype.toString.call(this.postDo[i])!=Function.prototype.toString.call(item.postDo[i])) {
              foundDifference = true;
            }
          }
          if (!foundDifference) {
            this.rawResult = item.rawResult;
            this.result = item.result;
            return;
          }
        }
        var oldResult = item.result;
        var oldRawResult = this.rawResult = item.result = typeof(item.rawResult)!='undefined'
          ? item.rawResult
          : item.result;
        for (var i = 0, j = this.postDo.length; i < j; i++) {
          if (traceOn) {
            callTraceNodeStatus(this.actId, 'rawResult: '+item.rawResult+' running postDo: '+this.postDo[i]);
            this.postDo[i].call(item);
            callTraceNodeStatus(this.actId, 'new result: '+item.result);
          } else {
            this.postDo[i].call(item);
          }
        }
        this.result = item.result;
        item.rawResult = oldRawResult;
        item.result = oldResult;
      } else {
        this.rawResult = this.result = typeof(item.rawResult)!='undefined'
          ? item.rawResult
          : item.result;
      }
    }
  , freeze: function freeze() {
      var frozen = {};
      var field;
      for (var i = 0, j = frozenFields.length; i < j; i++) {
        frozen[field = frozenFields[i]] = this[field];
      }
      return frozen;
    }
  , toString: function actToString() {
      return this.str;
    }
  , errorTrace: function errorTrace(msg) {
      Print(msg);
      this.stackTrace();
      return msg;
    }
  , stackTrace: function stackTrace() {
      var act = this;
      Print('called at ' + act.index + ' by... ' + act.actId + ' ' + act.str);
      while (act.invoker && act.invoker!==this) {
        Print('called at ' + act.invoker.index + ' by... ' + act.invoker.actId + ' ' + (act = act.invoker).str);
      }
    }
  , run: function run(l) {
      parseRunning = true;
      var act = this, m = l.__memoTable, last, seed, item;
      var str = act.str + '.' + l.idx;
      var addedNodes = {};
      var inLR = 0;
      //var startTime = (new Date()).getTime();
      l.__furthest = 0;
      l.__lastFailed = null;
      l.__interleaveStandIn = l.il() || l.empty();
      l.__optInterleaveStandIn = l.opt(l.__interleaveStandIn);
      if (stepIntoIE) throw 'start stepping!';
      while (act && continueCurrentParse) {
        //if ((new Date()).getTime() - startTime > 5000) {
        //  continueCurrentParse = false;
        //  emitCoreDump = true;
        //}
        if (traceOn && !addedNodes[act.actId]) {
          addedNodes[act.actId] = true;
          callTraceNodeAdd(act.actId, (act.invoker ? act.invoker.actId : null), str + ' at position ' + l.idx/* + ' with lrPhase: ' + act.lrPhase*/);
        }
        if ((item = m[str])) { // got a memo item here
          if (!item.inside) { // non-left-recursive invocation
            if (  !act.phase && item.deterministic && item.returned) {
              if (traceOn) {
                callTraceNodeStatus(act.actId, act+'   '+(last!==act.invoker ? 'returned from ' : 'reinvoked by ')+last.actId+' using memo from ' + item.actId + ' outcome: ' + (item.outcome ? item.outcome + ' result: ' + item.result : false));
                //callTraceNodeStatus(act.actId, act+'   '+'act lrPhase: ' + act.lrPhase);
              } else if (ConsoleTrace) {
                Print(str + ' using memo from ' + item.actId + ' outcome: ' + (item.outcome ? item.outcome + ' result: ' + item.result.toString().toProgramString() : false));
              }
              act.index = item.index;
              act.phase = item.phase;
              act.deterministic = true;
              act.returned++;
              if (traceActivations) {
                act.usedMemoSource = item;
              }
              if (act.outcome = item.outcome) {
                if (typeof(item.vars)!='undefined') {
                  act.vars = {};
                  for (var i in item.vars) {
                    act.vars[i] = item.vars[i];
                  }
                }
                l.idx += (act.advanced = item.advanced);
                act.doPostDoMemoization(item);
              }
              act = (last = act).invoker;
              if (act) {
                str = act.cachedStr || (act.cachedStr = act.str + '.' + l.idx);
              }
              continue;
            } else {
              if (traceOn) {
                callTraceNodeStatus(act.actId, act+'   '+(last!==act.invoker ? 'returned from ' : 'reinvoked by ')+last.actId+' at position ' + l.idx);
              }
              if (!act.phase && act.func.isSurrogate) {
                act.localContext = l;
              }
              if (!act.phase && !ignoreDisqualifyingREs && act.computeRecogRE(l)!==false) {
                act.index = l.idx;
                var nextAct = act;
                if (act = (last = act).checkFollowSet(l, act)) {
                  last.outcome = false;
                  last.done = true;
                  last.deterministic = true;
                } else {
                  act = (act = nextAct).func();
                }
              } else {
                 act = (last = act).func();
              }
            }
          } else if (act.lrPhase == 3) {
            if (traceOn) {
              callTraceNodeStatus(act.actId, act+'   '+(last!==act.invoker ? 'returned from ' : 'reinvoked by ')+last.actId+" ignoring memo result since I'm the one growing a seed");
            }
            act = (last = act).func();
            str = act.cachedStr || (act.cachedStr = act.str + '.' + l.idx);
            continue;
          } else if (item===act) {
            if (traceOn) {
              callTraceNodeStatus(act.actId, act+'   '+(last!==act.invoker ? 'returned from ' : 'reinvoked by ')+last.actId+' at position ' + l.idx /*+ ' with lrPhase: ' + act.lrPhase*/);
            }
            act = (last = act).func();
            if (act) {
              str = act.cachedStr || (act.cachedStr = act.str + '.' + l.idx);
            }
          } else if ((item.lrPhase == 3 || item.lrPhase == 2) && item.seed) { // the root act of this left recursion generated a seed, and we can use it.
            // steal the results of the available seed.
            seed = item.seed;
            //item.seed = false;
            if (traceOn) {
              callTraceNodeStatus(act.actId, act+'   using seed result: ' + seed.result);
            }
            act.index = seed.index;
            act.phase = seed.phase;
            act.returned++;
            act.outcome = act.deterministic = act.done = true; // force/fake being done/deterministic for now... TODO.
            act.result = seed.result;
            l.idx += (act.advanced = seed.advanced);
            act = (last = act).invoker;
            item.lrPhase = 2;
            if (act) {
              str = act.cachedStr || (act.cachedStr = act.str + '.' + l.idx);
            }
            continue;
          } else //if (!/^resolveByName\(/.test(act.str)
            //&& (!act.invoker || !/^resolveByName\(/.test(act.invoker.str))
            //)
            { // initial left-recursive invocation
            //alert('detected left recursion inside '+item+' at '+l.idx);
            //if (l.noCircularReferences) {
            //  Print('Error: detected formula circularity in '+act.str);
            //  throw 'error';
            //}
            if (traceOn) {
              callTraceNodeStatus(act.actId, act+'   detected left recursion inside actId ' + item.actId + ' with lrPhase '+item.lrPhase);
            }
            inLR++;
            item.lrPhase = 2; // 2 means: grow your seed next time you return.
            act.outcome = false;
            act = (last = act).invoker;
            if (act) {
              str = act.cachedStr || (act.cachedStr = act.str + '.' + l.idx);
            }
            continue;
          } /*else {
            if (traceOn) {
              callTraceNodeStatus(act.actId, act+'   detected RESOLVEBYNAME left recursion inside actId ' + item.actId + ' with lrPhase '+item.lrPhase);
            }
            Print([l.__furthest, act.actId]);
            item.lrPhase = 6; // 6 means "force-grow"
            if (!act.phase && act.func.isSurrogate) {
              act.localContext = l;
            }
            act = (last = act).func();
          }*/
        } else {
          if (traceOn) {
            callTraceNodeStatus(act.actId, act+'   initial invocation at position ' + l.idx);
          }
          if (ConsoleTrace) {
                Print(act.actId+' ' + act + ' initial invocation at position ' + l.idx);
                //if (act.actId==512) Print(act.actId + ': ' +inspectShallow(act))
              }
          if (!act.phase && act.func.isSurrogate) {
            act.localContext = l;
          }
          var thisREholder;
          if (!act.phase && act.invoker && !ignoreDisqualifyingREs && (thisREholder = act.computeRecogRE(l))!==false) {
            act.index = l.idx;
            var nextAct = act;
            if (act = (last = act).checkFollowSet(l, act)) {
               last.outcome = false;
               last.done = true;
               last.deterministic = true;
            } else {
              act = (act = nextAct).func();
            }
          } else {
            if (traceOn && typeof(thisREholder)!='undefined') { // repetitively redundant :)
              callTraceNodeStatus(act.actId, act+'   '+'RElookup succeeded: '+thisREholder);
            }
            act = (last = act).func();
          }
        }
        if (act) {
          if (typeof(m[last.cachedStr])=='undefined') {
            m[last.cachedStr] = last;
          }
          if (last.invoker===act) {
            last.returned++;
            if (last.outcome && last.index + last.advanced >= l.__furthest) {
              //alert('setting furthest to '+(last.index + last.advanced)+ ' from '+last);
              //Print('setting furthest to '+(last.index + last.advanced)+' with '+last+' that ate '+last.advanced);
              l.__furthest = last.index + last.advanced;
              //l.__lastFailed = null;
            } else if (!last.outcome
                && (last.index >= l.__furthest
                  || /^not\(/.test(last.str)
                  || (l.__lastFailed
                    && last.index >= l.__lastFailed.index))
                && (!/^(end\(|opil\( end\()/.test(last.str)
                  || !l.__lastFailed
                  || /^(end\(|opil\( end\()/.test(l.__lastFailed))) {
              //Print('setting lastFailed to '+last+' at position '+last.index);
              l.__lastFailed = last;
            //} else if (!last.outcome) {
              //Print('did not set lastFailed to '+last+' at position '+last.index);
            //} else {
              //Print('did not set furthest to '+last+' at position '+last.index);
            }
            if (last.done && last.returned==1 && !inLR) {
              last.deterministic = true;
            }
            if (traceOn) {
              callTraceNodeStatus(last.actId, last+'   '+ (last.outcome ? 'Succeeded' : 'Failed') + ': at position ' + l.idx + ' returned: ' + (last.result || '').toString().toProgramString() + ' deterministic: ' + last.deterministic + ' advanced: ' + last.advanced + ' done: ' + last.done);
            } else if (ConsoleTrace) {
              if (last.outcome) {
                Print(last.actId + '        ' + last + '\n\tadvanced from ' + last.index + ' to ' + (last.index + 
                  last.advanced) + '\n\t\tyielding ' + last.result.toString().toProgramString() +
                  '\n\t\t\tand returning to ' + act.actId + '     ' + act);
              } else {
                Print(last.actId + '        ' + last + '\n\tfailed at ' + last.index +
                  '\n\t\t\tand returning to ' + act.actId + '     ' + act);
              }
            }
            last.inside && (last.inside = false);
            if (last.lrPhase == 2) {
              if (last.outcome) {
                if (last.advanced <= last.seedAdvanced) { // use the last seed (by definition, the farthest-advanced)
                  if (last.seeds && (last.whichSeed = last.seeds.length)) {
                    seed = last.seeds[last.whichSeed - 1];
                    last.seed = false;
                    if (traceOn) {
                      callTraceNodeStatus(last.actId, last+'   using last seed result: ' + seed.result);
                    }
                    last.index = seed.index;
                    last.phase = seed.phase;
                    last.returned = seed.returned;
                    last.outcome = last.deterministic = last.done = true; // force/fake being done/deterministic for now... TODO.
                    last.result = seed.result;
                    l.idx = last.index + (last.advanced = seed.advanced);
                    m[last.cachedStr].lrPhase = 5; // recursion completed; only available option now is to backtrack through seeds
                  } else { // no suitable seed found; fail the entire recursion...
                    m[last.cachedStr].lrPhase = 4; // 4 means entirely failed...
                    last.outcome = false;
                    last.advanced = 0;
                    last.result = NaN;
                  }
                  inLR--;
                } else {
                  if (traceOn) {
                    callTraceNodeStatus(last.actId, last+'   freezing in a seed; advanced ' + last.advanced + ' from starting position ' + last.index);
                  }
                  last.seedAdvanced = last.advanced;
                  (last.seeds = last.seeds || []).push(last.seed = last.freeze()); // push frozen copy of self onto the seeds stack.
                  last.phase = last.returned = 0; // reset itself
                  act = last; // invoke itself again.
                  act.lrPhase = 3;
                  l.idx = act.index;
                }
              } else {
                if (l.__furthest > last.index) { // try left recursive case, when the left recursive invocation is not on the left
                // edge of the alternatives tree inside itself (when there's something else tried first or with higher priority).
                  last.phase = last.returned = 0; // reset itself
                  act = last; // invoke itself again.
                  l.idx = act.index;
                }
              }
            }
          } else {
            last.inside || (last.inside = true);
          }
          str = act.cachedStr || (act.cachedStr = act.str + '.' + l.idx);
        }
      }
      if (continueCurrentParse) {
        this.doPostDo();
      } else {
        currentParseCancelled = true;
        if (emitCoreDump) {
          document.getElementById('output').innerHTML = '<pre>' + inspectShallow(l) + "\n\n" + inspectShallow(last) + '</pre>';
        }
      }
      parseRunning = false;
      return this.result;
    }
  , reactivate: function reactivate(invoker) {
      var newAct;
      (newAct = new Act(undefined, this.func, this.args, this.str, this.deterministic)).invoker = invoker;
      if (this.postDo) { newAct.postDo = this.postDo.slice(0); }
      newAct.label = this.label;
      return newAct;
    }
  , getVar: function getVar(label) {
      //if (label=='ruleProjectionExpression') throw 'here';
      if (typeof(this.label)!='undefined' && this.label==label) {
        return this.result;
      } else if (typeof(this.vars)!='undefined' && typeof(this.vars[label])!='undefined') {
        return this.vars[label];
      }
      return;
    }
  , getArgList: function getArgList(label) {
      return (typeof(this.func.argNames)!='undefined' && this.func.argNames.has(label))
        ? [this.func.argNames, this.args]
        : this.invoker
          ? this.invoker.getArgList(label)
          : null;
    }
  , resolveVar: function resolveVar(label) {
      var value;
      var al = this.getArgList(label.toString());
      if (al!==null) {
        var index = al[0].indexOf(label.toString());
        return al[1][index];
      } else if (typeof(value = this.getVar(label))!='undefined') {
        return value;
      } else if (this.invoker) {
        return this.invoker.resolveVar(label);
      } else {
        return;
      }
    }
  };
  Func.Act = Act;
  var actsRegistry;
  if (traceActivations) {
    actsRegistry = Func.acts = [];
  }
  
  function applicatorRedirector() {
    return this.first.func();
  }
  
  // generic applicator
  Func.Rule = function Rule(l, applicator, determinismDeterminer, fNameOverride, ruleArgNames, fullNameOverride) {
    var self;
    var fName = typeof(fNameOverride)!='undefined'
      ? fNameOverride
      : applicator.toString().match(functionNameExtractor)[1];
    var fullName = fullNameOverride || ((this.hasOwnProperty('__languagePath')
        ? this.__languagePath
        : this.__languagePath == 'Base.Parser.Combinators'
          ? ''
          : this.__languagePath || '') + fName);
    var theFunc;
    self = function ruleActivator() {
      return new Act(fullName, theFunc, arguments);
    };
    self.arity = (typeof(ruleArgNames)!='undefined' ? ruleArgNames.length : false) || (applicator.toString().match(arityDetector) || {'1':0})[1];
    theFunc = self.func = function applyRule() {
      var first = this.first, returnTo;
      switch (this.phase) {
        
        case 0:
        this.index = this.localContext.idx;
        this.phase = 1;
        first = this.first = applicator.call(this);
        if (typeof(first)!='undefined') {
          //if (typeof(this.label)=='undefined') {
          //  this.label = first.label;
          //}
          first.invoker = this;
        }
        return first;
        
        case 1:
        this.index = first.index;
        this.deterministic = !!(this.deterministic || first.deterministic || !!first.fastRE || (determinismDeterminer && determinismDeterminer.apply(this.args)));
        if (first.outcome) {
          this.done = first.done;
          this.outcome = true;
          this.phase = 2;
          this.result = first.result;
          this.vars = first.vars;
          if (typeof(first.label)!='undefined') {
            if (!this.vars || typeof(this.vars[first.label])=='undefined') {
              (this.vars || (this.vars = {}))[first.label] = first.result;
              //if (typeof(this.label)=='undefined') {
              //  this.label = first.label;
              //}
            }
          }
          this.__nodeLabel = first.__nodeLabel;
          this.localContext.idx = this.index + (this.advanced = first.advanced);
          this.doPostDo();
        } else {
          this.localContext.idx = this.index;
          this.outcome = false;
          this.advanced = 0;
          this.done = true;
        }
        return this.invoker;
        
        case 2:
        this.localContext.idx = this.index;
        this.phase = 1;
        return first;
      }
    };
    self.func.isSurrogate = true;
    self.func.argNames = ruleArgNames;
    self.toString = function() { return fullName };
    var existingRule;
    if (existingRule = l[fName]) {
      if (existingRule.isDispatcher) {
        if (existingRule[self.arity]) {
          throw 'rule '+fName+' with arity '+self.arity+' already installed in '+l;
        }
        return existingRule[self.arity] = self;
      } else {
        var dispatcher;
        dispatcher = function ruleDispatcher() {
          //if (arguments.length == 1 && arguments[0] == 'ArgumentDispatchOverride__') { // treat it as a deferred execution (rule reference)
            //if (traceOn) {
            //  Print(fName+' treated as (deferred) rule reference');
            //}
          //  return l.proxied(dispatcher);
          //}
          if (typeof(dispatcher[arguments.length])!='function') {
            throw 'failed to resolve '+fullName+' with '+(arguments.length)+' arguments';
          }
          //if (traceOn) {
          //  Print(fName+' dispatching with '+(arguments.length)+' arguments');
          //}
          return new Func.Act(fullName, dispatcher[arguments.length].func, arguments);
        };
        dispatcher.toString = existingRule.toString;
        dispatcher.isDispatcher = true;
        dispatcher[existingRule.arity] = existingRule;
        dispatcher[self.arity] = self;
        return l[fName] = dispatcher;
      }
    } else {
      //if (traceOn) {
      //  Print(fName+' installed in context '+l.__contextId+' with arity '+self.arity);
      //}
      return l[fName] = self;
    }
  };
})();

var Rule = Func.Rule;













































