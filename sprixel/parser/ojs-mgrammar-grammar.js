/*
  This software is copyright 2009 Matthew Wilson matthew@wilson.org
  and available under the Artistic License 2 ("AL2") as documented at
  http://jsmeta.googlecode.com
*/

function MGrammar(l) { with (l) {
  
  Func.Rule(l, function Main() { return ModuleDeclaration() });
  
  Interleave(function Ignored() { return l.regexp(
    //cheating on "precedence \d+:" for now.... :)
    '(?:(?:[\\u0009\\u000B\\u000C\\u0020\\u000A\\u000D\\u0085\\u2028\\u2029]|\\r\\n)|(?:\/\\*(?:[^*]|\\*[^\/])*\\*?\\*\/)|(?:\/\/([^\\n])*(\\u000D\\u000A|[\\u000A\\u000D\\u0085\\u2028\\u2029]|$)))'//|(?:precedence(?:(?:[\\u0009\\u000B\\u000C\\u0020\\u000A\\u000D\\u0085\\u2028\\u2029]|\\r\\n)|(?:\/\\*(?:[^*]|\\*[^\/])*\\*?\\*\/)|(?:\/\/([^\\n])*(\\u000D\\u000A|[\\u000A\\u000D\\u0085\\u2028\\u2029]|$)))+\\d+(?:(?:[\\u0009\\u000B\\u000C\\u0020\\u000A\\u000D\\u0085\\u2028\\u2029]|\\r\\n)|(?:\/\\*(?:[^*]|\\*[^\/])*\\*?\\*\/)|(?:\/\/([^\\n])*(\\u000D\\u000A|[\\u000A\\u000D\\u0085\\u2028\\u2029]|$)))*:)'
  )});
  
  Func.Rule(l, function ModuleDeclaration() { return project(
    syntax(
      Attributes()
    , l.fil(any("module"))
    , bind("name", l.myIdentifier())
    , any("{")
    , bind("declarations", ModuleBody())
    , any("}")
    , opt(any(";")))
  , ModuleDeclarationProjection
  )});
  
  Func.Rule(l, function ModuleBody() {
    return l.both(l.ImportExportStatements(), l.oiplus(LanguageDeclaration()));
  });
  
  Func.Rule(l, function ImportExportStatements() {
    return l.oistar(l.project(l.either(l.syntax(
        l.fil(l.any("import")),
        l.DottedIdentifier(),
        l.any(";"))
    , l.syntax(
        l.fil(l.any("export")),
        l.myIdentifier(),
        l.any(";"))
    ), function() { this.result = undefined }));
  });
  
  Func.Rule(l, function LanguageDeclaration() { return project(
    syntax(
      Attributes()
    , fil(any("language"))
    , bind("languageName", myIdentifier())
    , any("{")
    , bind("languageMembers", LanguageBody())
    , any("}"))
  , LanguageDeclarationProjection
  )});
  
  Func.Rule(l, function LanguageBody() { return flatten(plus(alt(
      SyntaxProductionRule()
    , TokenProductionRule()
    , InterleaveProductionRule()
  )))});
  
  Func.Rule(l, function myIdentifier() { return regexp(
    '[A-Za-z_][A-Za-z0-9_]*'
  )});
  
  Func.Rule(l, function DottedIdentifier() { return regexp(
    '[A-Za-z][A-Za-z0-9]*(\\.[A-Za-z][A-Za-z0-9]*)*'
  )});
  
  Func.Rule(l, function NamedRuleSignature() { return either(
    project(
      both(
        myIdentifier()
      , seq(
          any('(')
        , bind("argNames", oiplusSep(any(','), myIdentifier()))
        , any(')')))
    , NamedRuleSignatureProjection)
  , flatten(myIdentifier())
  )});
  
  Func.Rule(l, function SyntaxProductionRule() { return finalize(project(
    both(syntax(
        Attributes()
      , fil(any("syntax")))
    , error(syntax(
        bind("ruleSignature", NamedRuleSignature(), "rule name (with optional parameter names)")
      , any('=')
      , bind("importer", SyntaxProductionSet())
      , any(';'))))
  , SyntaxProductionRuleProjection
  ))});
  
  Func.Rule(l, function SyntaxProductionSequence() { return project(
    both(
      notAfter('\\w')
    , syntax(
        any('(')
      , bind("productionSet", clearLexicalScopeOfResultingRule(SyntaxProductionSet()))
      , any(')')))
  , function() {
      this.result = this.getVar("productionSet");
    }
  )});
  
  Func.Rule(l, function SyntaxProductionSeries() { return project(
    both(
      bind("ruleProductionExpression", oiplus(SyntaxProductionTerm()))
    , bind("ruleProjectionExpression", opil(optR(RuleProjection()))))
  , SyntaxProductionSeriesProjection
  )});
  
  Func.Rule(l, function SyntaxProductionSet() { return project(
    oiplusSep(any('|'), SyntaxProductionSeries())
  , SyntaxProductionSetProjection
  )});
  
  Func.Rule(l, function SyntaxProductionTerm() { return either(
    project(
      syntax(
        bind("varName", opt(useFirst(both(myIdentifier(), opil(any(':'))))))
      , bind("termRule", alt(  // or a zillion others. :)
          SyntaxProductionSequence()
        , TextLiteralProduction()
        , BuiltInRuleKeyword()
        , NamedRuleReference()
        , CharacterRangeProduction()))
      , bind("quantifier", regexpE('([*?+]|#\\d+(\\.\\.\\d*)?)?')))
    , SyntaxProductionTermProjection)
  , TokenProductionTerm() // fallback to token production term.
  )});
  
  Func.Rule(l, function InterleaveProductionRule() { return finalize(project(
    both(syntax(
      Attributes()
    , fil(any("interleave")))
  , error(syntax(
      bind("ruleName", myIdentifier())
    , any('=')
    , bind("importer", TokenProductionSet())
    , any(';'))))
  , InterleaveProductionRuleProjection
  ))});
  
  Func.Rule(l, function TokenProductionRule() { return finalize(project(
    both(syntax(
      Attributes()
    , fil(any("token")))
  , error(syntax(
      bind("ruleSignature", NamedRuleSignature())
    , any('=')
    , bind("importer", TokenProductionSet())
    , any(';'))))
  , TokenProductionRuleProjection
  ))});
  
  Func.Rule(l, function TokenProductionSequence() { return project(
    both(
      notAfter('\\w')
    , syntax(
        any('('),
        bind("productionSet", clearLexicalScopeOfResultingRule(TokenProductionSet()))
      , any(')')))
  , function() {
      this.result = this.getVar("productionSet");
      //if (this.result) Print(['productionSet',this.result]);
    }
  )});
  
  Func.Rule(l, function TokenProductionSeries() { return project(
    both(
      bind("ruleProductionExpression", oiplus(TokenProductionTerm()))
    , bind("ruleProjectionExpression", opil(optR(RuleProjection()))))
  , TokenProductionSeriesProjection
  )});
  
  Func.Rule(l, function TokenProductionSet() { return project(
    oiplusSep(any('|'), TokenProductionSeries())
  , TokenProductionSetProjection
  )});
  
  Func.Rule(l, function TokenProductionTerm() { return project(
/*    syntax(
      bind("varName", opt(useFirst(both(myIdentifier(), opil(any(':'))))))
    , bind("inverseOp", opt(any("^")))
    , bind("termRule", alt(  // or a zillion others. :)
        TokenProductionSequence()
      , TextLiteralProduction()
      , BuiltInRuleKeyword()
      , NamedRuleReference()
      , CharacterRangeProduction()))
    , bind("quantifier", regexpE('([*?+]|#\\d+(\\.\\.\\d*)?)?')))*/
    seq(
      opil(bind("varName", opt(useFirst(both(myIdentifier(), opil(any(':')))))))
    , opil(bind("inverseOp", opt(any("^"))))
    , opil(bind("termRule", alt(  // or a zillion others. :)
        TokenProductionSequence()
      , TextLiteralProduction()
      , BuiltInRuleKeyword()
      , NamedRuleReference()
      , CharacterRangeProduction())))
    , opil(bind("quantifier", regexpE('([*?+]|#\\d+(\\.\\.\\d*)?)?'))))
  , TokenProductionTermProjection
  )});
  
  Func.Rule(l, function Attributes() { // ignored... :)
    return oistar(syntax(
      any("@{")
    , opt(Nodes())
    , any("}")));
  });
  
  
  // ----------------------------------------------------------
  //                 MGraph; projection stuff
  // ----------------------------------------------------------
  
  Func.Rule(l, function RuleProjection() {
    return useSecond(both(both(any('='), error(any('>'))), opil(TopLevelNode())));
  });
  
  Func.Rule(l, function Literal() { return project(
    alt(
      BooleanLiteral()
    , RealLiteral()
    , IntegerLiteral()
    , Language.Grammar.TextLiteral()
    , NullLiteral())
  , LiteralProjection
  )});
  
  Func.Rule(l, function BooleanLiteral() {
    return any('true','false');
  });
  
  Func.Rule(l, function NullLiteral() {
    return any('null');
  });
  
  Func.Rule(l, function RealLiteral() {
    return regexp('(?:0|[1-9]\\d*)(?:\\.\\d+)?')
  });
  
  Func.Rule(l, function IntegerLiteral() {
    return regexp('0|[1-9]\\d*')
  });
  
  Func.Rule(l, function Reference() { return project(
    myIdentifier()
  , ReferenceProjection
  )});
  
  Func.Rule(l, function TopLevelAtom() { return alt(
    Literal()
  , project(
      seq(
        any("labelof")
      , any("(")
      , opil(bind("referencedValue", Reference()))
      , opil(any(")")))
    , TopLevelAtomProjection)
  , Reference());
  });
  
  Func.Rule(l, function OrderedTerm() { return project(
    syntax(
      bind("termLabel", opt(Label()))
    , any("[")
    , bind("termNodes", opt(either(Nodes(),LabeledAtoms())))
    , any("]"))
  , OrderedTermProjection
  )});
  
  Func.Rule(l, function UnorderedTerm() { return project(
    syntax(
      bind("termLabel", opt(Label()))
    , any("{")
    , bind("termNodes", opt(either(Nodes(),LabeledAtoms())))
    , any("}"))
  , UnorderedTermProjection
  )});
  
  Func.Rule(l, function EntityTerm() { return project(
    syntax(
      bind("termLabel", opt(Label()))
    , any("[")
    , bind("termAtoms", opt(LabeledAtoms()))
    , any("]"))
  , EntityTermProjection
  )});
  
  Func.Rule(l, function StringValue() { return project(either(
      Language.Grammar.TextLiteral()
    , Reference())
  , function() {
      var referencedValue = this.result;
      this.result = function() {
        undefer(referencedValue, this);
      };
    });
  });
  
  Func.Rule(l, function Label() { return either(
    useFirst(both(
      useSecond(both(
        opil(both(
          any("id")
        , any("(")))
      , opil(StringValue())))
    , opil(any(")"))))
  , myIdentifier());
  });
  
  Func.Rule(l, function Nodes() { return oiplusSep(
    any(",")
  , Node());
  });
  
  Func.Rule(l, function TopLevelNode() { return l.alt(
    l.EntityTerm()
  , l.OrderedTerm()
  , l.UnorderedTerm()
  , l.TopLevelAtom());
  });
  
  Func.Rule(l, function Node() { return l.alt(
    l.EntityTerm()
  , l.OrderedTerm()
  , l.UnorderedTerm()
  , l.Atom());
  });
  
  Func.Rule(l, function Atom() { return l.either(
    l.TopLevelAtom()
  , l.seq(
      l.any("valuesof")
    , l.any("(")
    , l.opil(l.Reference())
    , l.opil(l.any(")"))))
  });
  
  Func.Rule(l, function LabeledAtoms() { return l.oiplusSep(
    l.any(",")
  , l.LabeledAtom());
  });
  
  Func.Rule(l, function LabeledAtom() { return l.syntax(
    l.Label()
  , l.any("=>")
  , l.Node());
  });
  
  // ----------------------------------------------------------
  //                 TERMINALS
  // ----------------------------------------------------------
  
  
  Func.Rule(l, function TextLiteralProduction() { return project(
    Language.Grammar.TextLiteral()
  , TextLiteralProjection
  )});
  
  Func.Rule(l, function SingleCharacterLiteral() { return regexp(
     '(?:(?:\"(?:[^\\u0022\\u005C\\u000A\\u000D\\u0085\\u2028\\u2029]|\\u005C[\'"0abfnrtv\\u005c]'
     + '|[\\]u[0-9a-fA-F]{4}|[\\]U[0-9a-fA-F]{8})\")'
     + '|(?:\'(?:[^\\u0027\\u005C\\u000A\\u000D\\u0085\\u2028\\u2029]|\\u005C[\'"0abfnrtv\\u005c]'
     + '|[\\]u[0-9a-fA-F]{4}|[\\]U[0-9a-fA-F]{8})\'))'
  )});
  
  Func.Rule(l, function CharacterRangeProduction() { return project(
    seq(
      bind("charMin", SingleCharacterLiteral())
    , any('..')
    , bind("charMax" ,SingleCharacterLiteral()))
  , CharacterRangeProductionProjection
  )});
  
  Func.Rule(l, function BuiltInRuleKeyword() { return either(
    project(keyword('any'), AnyKeywordProjection)
  , project(keyword('empty'), EmptyKeywordProjection)
  )});
  
  Func.Rule(l, function NamedRuleReference() { return either(
/*    NamedSyntaxRuleReference()
  , NamedTokenRuleReference()
  )});
  
  Func.Rule(l, function NamedSyntaxRuleReference() { return either(
    project(
      both(
        bind("invokedRuleName", DottedIdentifier())
      , seq(
          any('(')
        , bind("argActivations", oiplusSep(any(','), SyntaxProductionSet()))
        , any(')')))
      , ParameterizedNamedRuleReferenceProjection)
  , project(
      DottedIdentifier()
    , NamedRuleReferenceProjection
  ))});
  
  Func.Rule(l, function NamedTokenRuleReference() { return either(
*/
    project(
      both(
        bind("invokedRuleName", DottedIdentifier())
      , seq(
          any('(')
        , bind("argActivations", clearLexicalScope(oiplusSep(any(','), TokenProductionSet())))
        , any(')')))
      , ParameterizedNamedRuleReferenceProjection)
  , project(
      DottedIdentifier()
    , NamedRuleReferenceProjection
  ))});
  
}}




















