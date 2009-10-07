/*
  This software is copyright 2009 Matthew Wilson matthew@wilson.org
  and available under the Artistic License 2 ("AL2") as documented at
  http://jsmeta.googlecode.com
*/

// The "compiler".  Well, it's a compiler that specializes an interpreter
//   as a parser (optionally plus code-generator/[another-]compiler).

function ModuleDeclarationProjection() {
  this.result = '';
  var moduleName = this.getVar("name");
  var declarations = this.getVar("declarations");
  declarations = declarations.isArray ? declarations.flatten() : [declarations];
  var module = Module.CreateModule(moduleName, declarations);
  module.cooptImporter = true;
  
  var grammarGenerator = Module.Exec(lastDynamicParser = function(l){
    l.__Import(moduleName);
    lastParseContext = l;
    return function() {
      if (typeof(l[moduleName].__defaultLanguage)=='undefined' || typeof(l[l.__currentModule = moduleName][l.__currentLanguage = l[moduleName].__defaultLanguage].Main)!='function') {
        return new Func.Act("noEntryPoint", function() {
          this.result = "Main entry point not found";
          l.idx = (this.advanced = l.len) + 1;
          return this.invoker;
        }, []);
      }
      var parser = l[l.__currentModule = moduleName][l.__currentLanguage = l[moduleName].__defaultLanguage].Main();
      if (inConsole && !TestMode) {
        Print('Parsing input with '+l.__currentModule+'.'+l.__currentLanguage+'.Main');
      }
      return parser;
    };
  });
  
  lastParseContext.__Init(JSMetaPadInputString);
  if (tempDisableTrace) {
    traceOn = true;
  }
  var parseResult = Module.Parse(grammarGenerator, JSMetaPadInputString, lastParseContext);
  if (!parseResult.outcome) {
    if (TestMode) {
      return this.result = parseResult.errorMsg;
    } else {
      Print(parseResult.errorMsg);
      return parseResult.result = '';
    }
  }
  if (!inConsole) Print(' ');
  return this.result = parseResult.result;
}

function LanguageDeclarationProjection() {
  var languageName = this.getVar("languageName"),
   languageMembers = this.getVar("languageMembers"),
   hasMainSyntaxRule = false;
  if (languageMembers.isArray) {
    for (var i = 0, len = languageMembers.length; i < len; i++) {
      if (languageMembers[i].isMainSyntaxRule) {
        hasMainSyntaxRule = true;
      }
    }
  } else if (languageMembers.isMainSyntaxRule) {
    hasMainSyntaxRule = true;
  }
  this.result = function(l) {
    var language = l.__Member(function(l) {
      if (languageMembers.isArray) {
        for (var i = 0, len = languageMembers.length; i < len; i++) {
          languageMembers[i](l);
          if (languageMembers[i].isMainSyntaxRule) {
            l.__hasMainSyntaxRule = true;
          }
        }
      } else {
        languageMembers(l);
      }
    }, 'language', languageName, { hasMainSyntaxRule: hasMainSyntaxRule });
    language.Export();
  };
}

function NamedRuleSignatureProjection() {
  if (this.second.getVar("argNames")[this.second.getVar("argNames").length - 1]=='') {
    this.second.getVar("argNames").length--;
  }
  this.result = [this.first.result, this.second.getVar("argNames")];
}

function SyntaxProductionRuleProjection() {
  var ruleSignature = this.getVar("ruleSignature");
  if (!ruleSignature.isArray) ruleSignature = [ruleSignature];
  var importer = this.getVar("importer"),
      ruleName = ruleSignature[0],
      ruleArgNames = ruleSignature[1];
  this.result = function(l) {
    return Func.Rule(l, function TempPlaceHolderERYUWQYU() {
      return importer(l, ruleName);
    }, undefined, ruleName, ruleArgNames);
  };
  if (ruleName=='Main') {
    this.result.isMainSyntaxRule = true;
  }
  this.result.toString = function() { return ruleName };
}

function SyntaxProductionSeriesProjection() {
  var productionTerms = this.getVar("ruleProductionExpression");
  var projectionExpression = this.getVar("ruleProjectionExpression");
  this.vars = {};
  this.result = function(l, nodeLabel) {
    var productionTermActivations = [];
    if (productionTerms.isArray) {
      productionTerms = productionTerms.flatten();
      for (var i = 0, len = productionTerms.length; i < len; i++) {
        productionTermActivations[i] = productionTerms[i](l);
      }
    } else {
      productionTermActivations = [productionTerms(l)];
    }
    return l.project(l.syntax.apply(l, productionTermActivations),
      typeof(projectionExpression)=='function'
      ? projectionExpression
      : ((typeof(projectionExpression)=='string' && projectionExpression!='')
          || (typeof(projectionExpression)=='object' && Object.Length(projectionExpression)!=0))
        ? function() {
            this.result = projectionExpression;
          }
        : function() {
          if (typeof(nodeLabel)!='undefined') {
            var resHolder = {};
            resHolder[nodeLabel] = this.result;
            this.result = resHolder;
          }
        }
    );
  };
}

function SyntaxProductionSetProjection() {
  var alternatives = this.result.isArray ? this.result.flatten() : [this.result];
  this.result = function(l, nodeLabel) {
    return alternatives.length > 2
      ? l.alt.apply(l, alternatives.undefer(l, [l, nodeLabel]))
      : alternatives.length == 2
        ? l.either.apply(l, alternatives.undefer(l, [l, nodeLabel]))
          : alternatives[0].call(l, l, nodeLabel);
  };
}

function SyntaxProductionTermProjection() {
  var termRule = this.getVar("termRule"),
    quantifier = this.getVar("quantifier"),
       varName = this.getVar("varName");
  this.vars = {};
  if (termRule.isArray) {
    if (typeof(termRule[0])=='undefined') {
      throw "general failure";
    }
    termRule = termRule[0];
  }
  this.result = function(l) {
    var rule = termRule(l);
    switch (quantifier) {
      case '*': rule = l.oistar(rule); break;
      case '+': rule = l.oiplus(rule); break;
      case '?': rule = l.opt(rule); break;
      default: if (/^#/.test(quantifier)) {
        var max = NaN;
        if (/\.\.$/.test(quantifier)) {
          max = Infinity;
        }
        if (/\.\.\d+$/.test(quantifier)) {
          max = /\.\.(\d+)$/.exec(quantifier)[1];
        }
        var min = /^#(\d+)/.exec(quantifier)[1];
        if (isNaN(max)) {
          rule = l.repeat(min, l.opil(rule));
        } else if (min > max) {
          throw "range repetition quantifiers out of order";
        } else {
          rule = l.repeatBetween(min, max,  l.opil(rule));
        }
      }
    }
    if (typeof(varName)!='undefined' && varName!==null && varName!='') {
      l.bindLast(varName, rule);
    }
    return rule;
  };
  this.result.toString = function() { return 'syntaxRuleGenerator' };
}

function InterleaveProductionRuleProjection() {
  var ruleName = this.getVar("ruleName"),
      importer = this.getVar("importer");
  this.vars = {};
  this.result = function(l) {
    l.Interleave(function() { return importer(l, ruleName) });
    return Func.Rule(l, function TempPlaceHolderERYUWQYU() {
      return importer(l, ruleName);
    }, undefined, ruleName);
  };
  this.result.toString = function() { return ruleName };
}

function TokenProductionRuleProjection() {
  var ruleSignature = this.getVar("ruleSignature");
  if (!ruleSignature.isArray) ruleSignature = [ruleSignature];
  var importer = this.getVar("importer"),
      ruleName = ruleSignature[0],
      ruleArgNames = ruleSignature[1];
  this.vars = {};
  this.result = function(l) {
    return Func.Rule(l, function TempPlaceHolderERYUWQYU() {
      return importer(l, ruleName);
    }, undefined, ruleName, ruleArgNames);
  };
  this.result.toString = function() { return ruleName };
}

function TokenProductionSeriesProjection() {
  var productionTerms = this.getVar("ruleProductionExpression");
  var projectionExpression = this.getVar("ruleProjectionExpression");
  this.vars = {};
  this.result = function(l) {
    var productionTermActivations = [];
    if (productionTerms.isArray) {
      productionTerms = productionTerms.flatten();
      for (var i = 0, len = productionTerms.length; i < len; i++) {
        productionTermActivations[i] = productionTerms[i](l);
      }
    } else {
      productionTermActivations = [productionTerms(l)];
    }
    return l.project(
      productionTermActivations.length==1 && /^token\( seq\(/.test(productionTermActivations[0].str)
        ? productionTermActivations[0]
        : l.token(l.seq.apply(l, productionTermActivations))
    , typeof(projectionExpression)=='function'
        ? projectionExpression
        : ((typeof(projectionExpression)=='string' && projectionExpression!='')
            || (typeof(projectionExpression)=='object' && Object.Length(projectionExpression)!=0))
          ? function() {
              this.result = projectionExpression;
            }
          : function() {}
    );
  };
}

function TokenProductionTermProjection() {
  var termRule = this.getVar("termRule"),
    quantifier = this.getVar("quantifier"),
     inverseOp = this.getVar("inverseOp"),
       varName = this.getVar("varName");
  //Print([this.actId, this.index, this.advanced, varName, termRule, quantifier, inverseOp]);
  this.vars = {};
  this.result = function(l) {
    var rule = termRule(l);
    switch (quantifier) {
      case '*': rule = l.star(rule); break;
      case '+': rule = l.plus(rule); break;
      case '?': rule = l.opt(rule); break;
      default: if (/^#/.test(quantifier)) {
        var max = NaN;
        if (/\.\.$/.test(quantifier)) {
          max = Infinity;
        }
        if (/\.\.\d+$/.test(quantifier)) {
          max = /\.\.(\d+)$/.exec(quantifier)[1];
        }
        var min = /^#(\d+)/.exec(quantifier)[1];
        if (isNaN(max)) {
          rule = l.repeat(min, rule);
        } else if (min > max) {
          throw "range repetition quantifiers out of order";
        } else {
          rule = l.repeatBetween(min, max, rule);
        }
      }
    }
    if (typeof(inverseOp)!='undefined' && inverseOp!==null && inverseOp!='') {
      rule = l.useSecond(l.both(l.not(rule),l.any()));
    }
    if (typeof(varName)!='undefined' && varName!==null && varName!='') {
      l.bindLast(varName, rule);
      //alert('labeled "'+varName+'" to '+rule.str);
    }// else {
    //  alert("didn't label "+rule.str);
    //}
    return l.tokenizeDefault(rule);
  };
  this.result.toString = function() { return 'tokenRuleGenerator' };
}

function TokenProductionSetProjection() {
  var alternatives = this.result.isArray ? this.result.flatten() : [this.result];
  this.result = function(l, nodeLabel) {
    return l.tokenizeDefault(alternatives.length > 2
      ? l.alt.apply(l, alternatives.undefer(l, [l, nodeLabel]))
      : alternatives.length == 2
        ? l.either.apply(l, alternatives.undefer(l, [l, nodeLabel]))
        : alternatives[0].call(l, l, nodeLabel));
  };
}

function LiteralProjection() {
  var valueLiteral;
  eval('valueLiteral = '+this.result);
  this.result = function(l) {
    this.result = valueLiteral;
  };
}

function ReferenceProjection() {
  var identifier = this.result;
  this.result = function() {
    this.result = this.getVar(identifier);
    if (typeof(this.result)=='undefined') {
      throw "failed to resolve "+identifier;
    }
  };
}

function TopLevelAtomProjection() {
  var referencedValue = this.getVar("referencedValue");
  this.result = function() {
    if (typeof(referencedValue)=='function') {
      referencedValue.call(this);
      referencedValue = this.result;
    }
    var labelOfResult = '';
    if (typeof(referencedValue)=='object' && !referencedValue.isArray) {
      for (var i in referencedValue) {
        labelOfResult = i;
      }
    }
    this.result = labelOfResult;
  };
}

function TextLiteralProjection() {
  var textLiteral;
  eval('textLiteral = '+this.result);
  this.result = function(l) {
    return l.any(textLiteral);
  };
}

function CharacterRangeProductionProjection() {
  var charMin, charMax;
  eval('charMin = '+this.getVar("charMin")
    +';charMax = '+this.getVar("charMax"));
  this.vars = {};
  this.result = function(l) {
    return l.charRange(charMin, charMax);
  };
}

function ParameterizedNamedRuleReferenceProjection() {
  var ruleName = this.getVar("invokedRuleName");
  var ruleRefArgs = this.getVar("argActivations") || [];
  if (!ruleRefArgs.isArray) {
    ruleRefArgs = [ruleRefArgs];
  }
  this.vars = {};
  this.result = function(l) {
    return l.resolveByName(ruleName.withScope(l), ruleRefArgs);
  };
}

function NamedRuleReferenceProjection() {
  var ruleName = this.result;
  this.result = function(l) {
    return l.resolveByName(ruleName.withScope(l), []);
  };
}

function OrderedTermProjection() {
  var termLabel = this.getVar("termLabel");
  var termNodes = this.getVar("termNodes");
  this.vars = {};
  this.result = function() {
    var nodes, result = {}, label = undefer(termLabel || "", this).toString();
    result[label] = (nodes = undefer(termNodes, this)).isArray
      ? nodes
      : [ nodes ];
    this.result = result;
  };
}

function UnorderedTermProjection() {
  var termLabel = this.getVar("termLabel");
  var termNodes = this.getVar("termNodes");
  this.vars = {};
  this.result = function() {
    var nodes, result = {}, label = undefer(termLabel || "", this).toString();
    result[label] = (nodes = undefer(termNodes, this)).isArray
      ? '{' + (  nodes  ).toString().chip() + '}'
      : '{' + ([ nodes ]).toString().chip() + '}';
    this.result = result;
  };
}

function EntityTermProjection() {
  var termLabel = this.getVar("termLabel") || "";
  var termAtoms = this.getVar("termAtoms");
  this.vars = {};
  this.result = function() {
    var atoms, result = {};
    result[undefer(termLabel, this).toString()]
      = (atoms = undefer(termAtoms, this)).isArray
        ? atoms
        : [ atoms ];
    this.result = result;
  };
}

function AnyKeywordProjection() {
  this.result = function(l) {
    return l.any();
  }
}

function EmptyKeywordProjection() {
  this.result = function(l) {
    return l.empty();
  }
}







