/*
  This software is copyright 2009 Matthew Wilson matthew@wilson.org
  and available under the Artistic License 2 ("AL2") as documented at
  http://jsmeta.googlecode.com
*/

var Module = (function ModuleSystem() {

  var _modules = {};
  
  var contextId = 0;
  
  function Module(moduleImporter, defaultImport, cooptImporter) {
    var fullName = moduleImporter.toString().match(functionNameExtractor)[1].split('__').join('.');
    var module = _modules[fullName] = {
      importer: moduleImporter
    , fullName: fullName
    , namePaths: fullName.split('.')
    , isDefaultImport: defaultImport ? true : false
    , cooptImporter : cooptImporter ? true : false
    };
    if (module.isDefaultImport) {
      defaultBaseModules.push(module.fullName);
    }
    return module;
  }
  
  Module.__modules = _modules;
  
  Module.CreateModule = function(moduleName, declarations) {
    var module = new Module(function PLACEHOLDERZZZZZ(){});
    _modules['PLACEHOLDERZZZZZ'] = undefined;
    module.importer = function(l) {
      var result;
      for (var i = (declarations || []).length - 1; i >= 0; i--) {
        if (typeof(declarations[i])=='function') {
          if (declarations[i].__hasMainSyntaxRule) {
            throw 'here';
          }
          result = declarations[i].call(l, l);
        }
      }
      return result;
    };
    module.fullName = moduleName;
    module.namePaths = moduleName.split('.');
    _modules[moduleName] = module;
    return module;
  };
  
  Module.Exec = function justRunTheFunction(functionAsModule) {
    var moduleContext = new ModuleContext();
    var result = functionAsModule(moduleContext);
    result.__context = moduleContext;
    return result;
  };
  var lastActivations = 0, lastReTests = 0;
  
  Module.Parse = function parseUsingModule(grammarGenerator, inputStr, contextOverride) {
    var startTime = (new Date()).getTime();
    var grammar = Module.Exec(grammarGenerator);
    if (grammar.isArray) {
      contextOverride = grammar[1];
      grammar = grammar[0];
    }
    var l = contextOverride || grammar.__context;
    if (typeof(l.input)=='undefined') {
      l.__Init(inputStr);
    }
    if (inConsole && !TestMode) {
      //Print(grammar.toStringExpanded(null, l));
    }
    if (tempDisableTrace = traceOn && inNestedParse && !tempDisableTrace) {
      traceOn = false;
    }
    var matchAll = l.matchAll(grammar);
    var output;
    if (inConsole && !TestMode) {
      Print('activations: '+Func.activations()+'  re tests: '+Func._reTests());
    }
    var skipRest = false;
    if (debugMode) {
      output = matchAll.run(l);
    } else {
      try {
        output = matchAll.run(l);
      } catch(e) {
        matchAll.outcome = false;
        matchAll.result = '';
        matchAll.errorMsg = 'Parsing problem: '+e;
      }
    }
    var duration = (new Date()).getTime() - startTime;
    var currentActivations = Func.activations() - lastActivations;
    lastActivations = Func.activations();
    var currentReTests = Func._reTests() - lastReTests;
    lastReTests = Func._reTests();
    var errorMsg = '';
    if (!matchAll.outcome) {
      var lastFailed = l.__lastFailed
        //? (l.__lastFailed.actId >= grammar.actId || l.__lastFailed.actId==matchAll.EnDeR.actId)
        ? l.__lastFailed.index
          ? l.__lastFailed
          : grammar
        : grammar;
      var failedIndex = lastFailed.index/* + 1 - 1 */;
      var recogRE;
      if (warnMode) {
        //Print('lastFailed is '+lastFailed);
        /*Print('lastFailed.phase is '+lastFailed.phase);
        Print('lastFailed.first is '+lastFailed.first);
        Print('lastFailed.first.phase is '+lastFailed.first.phase);
        Print('lastFailed.first.outcome is '+lastFailed.first.outcome);
        if (lastFailed.computeRecogRE(l)) {
          Print(lastFailed.computeRecogRE(l));
        }*/
      }
      try {
        //if (lastFailed instanceof Func.Act && lastFailed.invoker && /^token\(/.test(lastFailed.invoker.str)) { // TODO: untested
        //  lastFailed = lastFailed.invoker;
        //}
        while (lastFailed instanceof Func.Act && /^((opil|pil|oiseq|syntax|fil|oistar|oiplus|plus|both|seq|any|token|repeat|regexp|regexpE|repeatBetween|resolveByName)\()|[\w.]+Main\(\)/.test(lastFailed.str)) {
          if (/^(any|regexp|regexpE)\(/.test(lastFailed.str)) { // /^token\(/.test(lastFailed.args[0].str) && 
            lastFailed = typeof(lastFailed.args[0])!='undefined' ? lastFailed.args[0].toProgramString() : 'any';
          } else {
            //alert('checking '+lastFailed.str);
            lastFailed = /^resolveByName\(/.test(lastFailed.str)
              ? lastFailed.first
              : /^repeatBetween\(/.test(lastFailed.str)
                ? lastFailed.args[2]
                : /^repeat\(/.test(lastFailed.str)
                  ? lastFailed.args[1]
                  : /^pil\(/.test(lastFailed.str)
                    ? 'interleave'
                    : (/^token\(/.test(lastFailed.str) && lastFailed.first && /^altRE\(/.test(lastFailed.first.str) /*&& !print(lastFailed.args[0])*/)
                      ? lastFailed.str
                      //lastFailed.first.args[0].replace(/\?\:/g, '').replace(/\(\?!/g, '^(').replace('([\\S\\s])',' any ').replace('[\\S\\s]',' any ').replace(/^\s/, '').replace(/\s$/, '')
                      : /^(both|seq|opil|pil|oistar|oiplus|plus|token|syntax)\(/.test(lastFailed.str)
                        ? lastFailed.args[0]
                        : lastFailed.first.first;
            failedIndex = (typeof(lastFailed)!='undefined' && lastFailed.index) ? lastFailed.index : failedIndex;
          }
        }
      } catch(e) {
        if (debugMode) {
          //Print(e);
        }
        // disregard
      }
      failedIndex = failedIndex || (lastFailed instanceof Func.Act && lastFailed.index ? lastFailed.index : 0) || 0;
      var failedPoint = 'line 1, column 1';
      var foundWhat = typeof(l.input[failedIndex])=='undefined'
        ? 'the end of input'
        : l.input.slice(failedIndex, Math.min(failedIndex + 15, l.input.length)).join('').toProgramString();
      if (/^"/.test(foundWhat) && /^"/.test(lastFailed)/* && lastFailed.length <= foundWhat.length*/) {
        foundWhat = unescapeString(foundWhat.chip());
        lastFailed = unescapeString(lastFailed.chip());
        var len = foundWhat.length;
        for (var i = 0; i <= len; i++) {
          if (lastFailed.charCodeAt(0)==foundWhat.charCodeAt(0)) {
            lastFailed = lastFailed.split('').slice(1).join('');
            foundWhat = foundWhat.split('').slice(1).join('');
            failedIndex++;
          } else {
            break;
          }
        }
        foundWhat = foundWhat.length ? foundWhat.toProgramString() : 'the end of input';
        lastFailed = lastFailed.toProgramString();
      }
      if (failedIndex) {
        var succeededInput = l.input.slice(0, failedIndex).join('');
        var succeededLines = succeededInput.split(/\r\n|\n/);
        var lineNumber = succeededLines.length;
        var lastLine = succeededLines[succeededLines.length - 1];
        var colNumber = lastLine.length + 1;
        failedPoint = 'line ' + lineNumber + ', column ' + colNumber;
      }
      errorMsg = (typeof(matchAll.errorMsg)!='undefined' && matchAll.errorMsg!='')
        ? matchAll.errorMsg+' at '+failedPoint
        : 'Expected '+lastFailed+' at '+failedPoint + '; instead found '+ foundWhat;
    }
    //alert(output);
    var toReturn = {
      input: l.input.str.toProgramString()
    , outcome: matchAll.outcome
    , result: ((typeof(output)!='undefined' && output!==NaN) ? output : '')
    , errorMsg: errorMsg
    , duration: duration+' ms'
    , grammar: grammarGenerator.description || grammar.str
    , activations: currentActivations+' activations'
    , reTests: currentReTests+' re tests'
    };
    //if (debugMode) Print(inspect(toReturn));
    return toReturn;
  };
  
  var defaultBaseModules = [];
  
  var currentlyImporting = {};
  
  var importHasRoot = false;
  
  function declareMember(memberImporter, memberType, memberName, memberMetadata) {
    memberName = memberName || memberImporter.toString().match(functionNameExtractor)[1].split('__').join('.');
    var member = {
      name: memberName
    , importer: memberImporter
    , memberType: memberType
    , exported: false
    , Export: function() {
        this.exported = !this.exported;
      }
    };
    if (memberType == 'language' && typeof(memberMetadata)!='undefined' && typeof(memberMetadata.hasMainSyntaxRule)!='undefined' && memberMetadata.hasMainSyntaxRule) {
      this.__defaultLanguage = memberName;
      this.topLevelLanguages = this.topLevelLanguages || [];
      this.topLevelLanguages.push(memberName);
    }
    this.__members.push(member);
    return member;
  }
  
  function ModuleContext(inheriting) {
    var context = this;
    context.constructor = ModuleContext;
    context.__contextId = contextId++;
    //Print('created moduleContext '+context.__contextId);
    context.__members = [];
    if (!inheriting) {
      if (_modules.parserLoader) {
        context = context.__Import('parserLoader');
      }
      for (var i = 0; i < defaultBaseModules.length; i++) {
        context = context.__Import(defaultBaseModules[i]);
      }
    }
    return context;
  }
  Module.__ModuleContext = ModuleContext;
  
  ModuleContext.prototype = {
    toString: function() {
      return this.hasOwnProperty('__languagePath')
        ? this.__languagePath
        : this.__languagePath == 'Base.Parser.Combinators'
          ? 'anonymousContext'
          : this.__languagePath || 'anonymousContext';
    },
    __Init: function(inputStr) {
      this.__memoTable = { 'REs': {} };
      this._gP = {};
      (this.input = inputStr.split('')).str = inputStr;
      this.idx = 0;
      this.noCircularReferences = false;
      this.input.distinctChars = this.input.distinct();
      this.len = inputStr.length;
      //if (traceOn) {
      //  callTraceNewInput(inputStr);
      //}
    },
    __Import: function moduleImporter(nameOfModuleToImport, importAlias) {
      var isRoot = false;
      if (!importHasRoot) {
        isRoot = importHasRoot = true;
        currentlyImporting = {};
      }
      if (currentlyImporting[nameOfModuleToImport]) {
        // TODO: still recurse the importing path to generate the nested objects if necessary
        this[nameOfModuleToImport] = currentlyImporting[nameOfModuleToImport];
      } else if(typeof(_modules[nameOfModuleToImport])=='undefined') {
        throw 'module '+nameOfModuleToImport+' not found!';
      } else {
        var newContext = spawnOf(this);
        newContext.__members = [];
        currentlyImporting[nameOfModuleToImport] = newContext;
        _modules[nameOfModuleToImport].importer(newContext);
        newContext.__private = spawnOf(newContext);
        //newContext.__private.__contextId = contextId++;
        for (var i = 0; i < newContext.__members.length; i++) {
          var memberContext = spawnOf(newContext);
          memberContext.__languagePath = nameOfModuleToImport + '.' + newContext.__members[i].name;
          //Print('named context '+memberContext.__contextId+' '+memberContext.__languagePath);
          //memberContext.__contextId = contextId++;
          newContext.__members[i].importer(memberContext);
          if (newContext.__members[i].exported) {
            newContext[newContext.__members[i].name] = memberContext;
          }
          newContext.__private[newContext.__members[i].name] = memberContext;
        }
        if (newContext.hasOwnProperty('__defaultLanguage')) {
          (_modules[nameOfModuleToImport].cooptImporter ? newContext : this).__defaultLanguage = newContext.__defaultLanguage;
        }
        if (typeof(importAlias)=='undefined') {
          var i = _modules[nameOfModuleToImport].namePaths.length;
          var j = i, parent, path;
          (parent = this)[_modules[nameOfModuleToImport].namePaths[i-1]] = newContext;
          while (--j+1) {
            parent = parent[path = _modules[nameOfModuleToImport].namePaths[i-j-1]] = j
              ? parent[path] || {}
              : newContext;
          }
        } else {
          this[importAlias] = newContext;
        }
      }
      if (isRoot) {
        importHasRoot = false;
      }
      return _modules[nameOfModuleToImport].cooptImporter
        ? newContext
        : this;
    },
    __Language: function(memberImporter) {
      return declareMember.call(this, memberImporter, 'language');
    },
    __Type: function(memberImporter) {
      return declareMember.call(this, memberImporter, 'type');
    },
    __Extent: function(memberImporter) {
      return declareMember.call(this, memberImporter, 'extent');
    },
    __Value: function(memberImporter) {
      return declareMember.call(this, memberImporter, 'value');
    },
    __Member: function(memberImporter, memberType, memberName, memberMetadata) {
      return declareMember.call(this, memberImporter, memberType, memberName, memberMetadata);
    }
  };
  return Module;
})();

































