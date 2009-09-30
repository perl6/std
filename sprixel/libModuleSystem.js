/*
  This software is copyright 2009 Matthew Wilson matthew@wilson.org
  and available under the Artistic License 2 ("AL2")
*/

function Print(){ for (var i in arguments) say(arguments[i]) }

function ModuleSystem(){

  var _modules = {};
  
  var contextId = 0;
  
  var Module = function(moduleImporter, defaultImport, cooptImporter) {
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
      return this.hasOwnProperty('__fqn') ? this.__fqn : '__ANONYMOUS_MODULE';
    },
    __Init: function(inputStr) {
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

































