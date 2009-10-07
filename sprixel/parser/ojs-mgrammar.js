/*
  This software is copyright 2009 Matthew Wilson matthew@wilson.org
  and available under the Artistic License 2 ("AL2") as documented at
  http://jsmeta.googlecode.com
*/
// much of this is translated nearly verbatim from some release of Microsoft's "Oslo" CTP's included .mg files, at the time of this writing, the "May 09 CTP" release.

Module(function Language(l) {

  l.__Language(function Base(l) {
    
    Func.Rule(l, function NewLine() { return l.token(l.any(
      "\u000A"
    , "\u000D\u000A" // swapped the order of this and \u000D
    , "\u000D"
    , "\u0085"
    , "\u2028"
    , "\u2029"
    ))});
    /*
    Func.Rule(l, function Whitespace() { return l.token(l.plus(l.WhitespaceCharacter())) });
    
    Func.Rule(l, function WhitespaceCharacter() { return l.any(
      "\u0009"
    , "\u000B"
    , "\u000C"
    , "\u0020" // inlines NewLine here for vast performance increase...
    , "\u000A"
    , "\u000D\u000A" // swapped the order of this and \u000D
    , "\u000D"
    , "\u0085"
    , "\u2028"
    , "\u2029"
    )});
    */
    Func.Rule(l, function Whitespace() { return l.regexp(
      '(?:' + l.WhitespaceCharacter().computeRecogRE(l) + '+)'); });
    
    Func.Rule(l, function WhitespaceCharacter() { return l.regexp(
      "(?:[\\u0009\\u000B\\u000C\\u0020\\u000A\\u000D\\u0085\\u2028\\u2029]|\\u000D\\u000A)"
    )});
    
    Func.Rule(l, function UpperCase() { return l.charRange('A', 'Z') });
    
    Func.Rule(l, function LowerCase() { return l.charRange('a', 'z') });
    
    Func.Rule(l, function Letter() { return l.alt(
      l.LowerCase()
    , l.UpperCase()
    , l.any('_')
    )});
    
    Func.Rule(l, function Digits() { return l.token((l.plus(l.Digit()))) });
    
    Func.Rule(l, function Digit() { return l.charRange('0', '9') });
    
    Func.Rule(l, function HexDigit() { return l.alt(
      l.Digit()
    , l.charRange('a', 'f')
    , l.charRange('A', 'F')
    // '[0-9a-fA-F]'
    )});
    
    Func.Rule(l, function HexDigits() { return l.token(l.plus(l.HexDigit())) });
    
    Func.Rule(l, function NewLineCharacter() { return l.any(
      "\u000A"
    , "\u000D"
    , "\u0085"
    , "\u2028"
    , "\u2029"
    )});
  }).Export();
  
  l.__Language(function Grammar(l) {
    
    
    
    Func.Rule(l, function Comment() { return l.regexp(
      '(?:\/\\*(?:[^*]|\\*[^\/])*\\*?\\*\/)|(?:\/\/([^\\n])*(\\u000D\\u000A|[\\u000A\\u000D\\u0085\\u2028\\u2029]|$))'
    /*either(
      l.CommentDelimited()
    , l.CommentLine()*/
    )});
    
    Func.Rule(l, function Binary() { return l.seq(
      l.any("0")
    , l.any('x', 'X')
    , l.token(l.star(l.Language.Base.HexDigit()))
    )});
    
    Func.Rule(l, function Date() { return l.seq(
      l.opt(l.Sign())
    , l.DateYear()
    , l.any('-')
    , l.DateMonth()
    , l.any('-')
    , l.DateDay()
    )});
    
    Func.Rule(l, function DateTime() { return l.seq(
      l.Date()
    , l.any('T')
    , l.Time()
    )});
    
    Func.Rule(l, function DateTimeOffset() { return l.seq(
      l.Date()
    , l.any('T')
    , l.Time()
    , l.TimeZone()
    )});
    
    Func.Rule(l, function Decimal() { return l.seq(
      l.token(l.plus(l.Language.Base.Digit()))
    , l.any('.')
    , l.token(l.plus(l.Language.Base.Digit()))
    )});
    
    Func.Rule(l, function Guid() { return l.seq(
      l.any('#[')
    , l.repeat(8, l.Language.Base.HexDigit())
    , l.any('-')
    , l.repeat(4, l.Language.Base.HexDigit())
    , l.any('-')
    , l.repeat(4, l.Language.Base.HexDigit())
    , l.any('-')
    , l.repeat(4, l.Language.Base.HexDigit())
    , l.any('-')
    , l.repeat(12, l.Language.Base.HexDigit())
    , l.any('[')
    )});
    
    Func.Rule(l, function Integer() { return l.Language.Base.Digits() });
    
    Func.Rule(l, function Scientific() { return l.seq(
      l.Decimal()
    , l.any('e', 'E')
    , l.token(l.opt(l.Sign()))
    , l.token(l.plus(l.Language.Base.Digit()))
    )});
    
    Func.Rule(l, function TextLiteral() { return l.regexp(
      '(?:(?:"(?:[^\\u0022\\\\\\u000A\\u000D\\u0085\\u2028\\u2029]|\\\\[\'"0abfnrtv\\\\]|\\\\u[0-9a-fA-F]{4}|\\U[0-9a-fA-F]{8})+")|(?:\'(?:[^\\u0027\\\\\\u000A\\u000D\\u0085\\u2028\\u2029]|\\\\[\'"0abfnrtv\\\\]|\\\\u[0-9a-fA-F]{4}|\\\\U[0-9a-fA-F]{8})+\'))'
      // l.token(l.either(l.RegularStringLiteral(), l.VerbatimStringLiteral()))
    )});
    
    Func.Rule(l, function RegularStringLiteral() { return l.either(
      l.seq(l.any('"')
      , l.plus(l.DoubleQuoteTextCharacter())
      , l.any('"')
      )
    , l.seq(l.any("'")
      , l.plus(l.SingleQuoteTextCharacter())
      , l.any("'")
      )
    )});
    
    Func.Rule(l, function VerbatimStringLiteral() { return l.either(
      l.seq(l.any('@"')
      , l.plus(l.DoubleQuoteTextVerbatimCharacter())
      , l.any('"')
      )
    , l.seq(l.any("@'")
      , l.plus(l.SingleQuoteTextVerbatimCharacter())
      , l.any("'")
      )
    )});
    
    Func.Rule(l, function SingleQuoteTextCharacter() { return l.alt(
      l.SingleQuoteTextSimple()
    , l.CharacterEscapeSimple()
    , l.CharacterEscapeUnicode()
    )});
    
    Func.Rule(l, function SingleQuoteTextSimple() { return l.none(
      '\u0027'
    , '\u005C'
    , '\u000A'
    , '\u000D'
    , '\u0085'
    , '\u2028'
    , '\u2029'
    )});
    
    Func.Rule(l, function SingleQuoteTextVerbatimCharacter() { return l.either(
      l.none('\u0027')
    , l.SingleQuoteTextVerbatimCharacterEscape()
    )});
    
    Func.Rule(l, function SingleQuoteTextVerbatimCharacterEscape() { return l.any('\u0027\u0027') });
    
    Func.Rule(l, function SingleQuoteTextVerbatimCharacters() { return l.token(l.plus(
      l.SingleQuoteTextVerbatimCharacter()
    ))});
    
    Func.Rule(l, function DoubleQuoteTextCharacter() { return l.alt(
      l.DoubleQuoteTextSimple()
    , l.CharacterEscapeSimple()
    , l.CharacterEscapeUnicode()
    )});
    
    Func.Rule(l, function DoubleQuoteTextSimple() { return l.none(
      '\u0022', '\u005C', '\u000A', '\u000D', '\u0085', '\u2028', '\u2029'
    )});
    
    Func.Rule(l, function DoubleQuoteTextVerbatimCharacter() { return l.either(
      l.none('\u0022')
    , l.DoubleQuoteTextVerbatimCharacterEscape()
    )});
    
    Func.Rule(l, function DoubleQuoteTextVerbatimCharacterEscape() { return l.any('\u0022\u0022') });
    
    Func.Rule(l, function DoubleQuoteTextVerbatimCharacters() { return l.token(l.plus(
      l.DoubleQuoteTextVerbatimCharacter()
    ))});
    
    Func.Rule(l, function CharacterEscapeSimple() { return l.both(
      l.any('\u005C')
    , l.CharacterEscapeSimpleCharacter()
    )});
    
    Func.Rule(l, function CharacterEscapeSimpleCharacter() { return l.any(
      "'", '"', '\u005C', '0', 'a', 'b', 'f', 'n', 'r', 't', 'v'
      // '[\'"0abfnrtv\\u005c]'
    )});
    
    Func.Rule(l, function CharacterEscapeUnicode() { return l.either(
      l.both(
        l.any("\\u")
      , l.repeat(4, l.Language.Base.HexDigit())
      )
    , l.both(
        l.any("\\U")
      , l.repeat(8, l.Language.Base.HexDigit())
      )
      // '[\\]u[0-9a-fA-F]{4}|[\\]U[0-9a-fA-F]{8}'
    )});
    
    Func.Rule(l, function Time() { return l.seq(
      l.TimeHourMinute()
    , l.any(":")
    , l.TimeSecond()
    )});
    
    Func.Rule(l, function CommentDelimited() { return l.regexp(
    //'/*' ('*' any - '/' | any - '*')* '*'? '*/'
    //'\/\*
    '\/\\*([^*]|\\*[^\/])*\\*?\\*\/'
    /*  l.any("/*")
    , l.token(l.star(l.CommentDelimitedContent()))
    , l.any("")*/
    )});
    
    Func.Rule(l, function CommentDelimitedContent() { return l.either(
      l.none('*')
    , l.both(
        l.any('*')
      , l.none('/')
      )
    )});
    
    Func.Rule(l, function CommentLine() { return l.regexp(
      '\/\/([^\\n])*(\\u000D\\u000A|[\\u000A\\u000D\\u0085\\u2028\\u2029]|$)'
    )});
    
    Func.Rule(l, function CommentLineContent() { return l.none(
      '\u000A', '\u000D', '\u0085', '\u2028', '\u2029'
    )});
    
    Func.Rule(l, function DateDay() { return l.any(
      "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"
    )});
    
    Func.Rule(l, function DateMonth() { return l.any(
      "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"
    )});
    
    Func.Rule(l, function DateYear() { return l.repeat(
      4, l.Language.Base.Digit()
    )});
    
    Func.Rule(l, function Sign() { return l.any(
      "+", "-"
    )});
    
    Func.Rule(l, function TimeHour() { return l.any(
      "00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"
    )});
    
    Func.Rule(l, function TimeMinute() { return l.both(
      l.any('0', '1', '2', '3', '4', '5')
    , l.Language.Base.Digit()
    )});
    
    Func.Rule(l, function TimeSecond() { return l.seq(
      l.any('0', '1', '2', '3', '4', '5', "60")
    , l.Language.Base.Digit()
    , l.token(l.opt(TimeSecondDecimalPart()))
    )});
    
    Func.Rule(l, function TimeSecondDecimalPart() { return l.both(
      l.any('.')
    , l.Language.Base.Digits()
    )});
    
    Func.Rule(l, function TimeZone() { return l.either(
      l.both(
        l.Sign()
      , l.TimeHourMinute()
      )
    , l.any('Z')
    )});
    
    Func.Rule(l, function Identifier() { return l.token(l.seq(
      l.opt(l.any('@'))
    , l.Language.Base.Letter()
    , l.star(
        l.either(
          l.Language.Base.Letter()
        , l.Language.Base.Digit()
        )
      )
    ))});
  }).Export();
}, true);

Module(function Microsoft__M__Grammar(l) {
  
  l.__Language(function MGrammar(l) { with (l) {
    
    Func.Rule(l, function Main() { with (l) { return project(
      bind("cu", CompilationUnit()), function() {
        this.result = this.getVar("cu") })}});
    
    Interleave(function Skippable() { with (l) {
      return either( Base.Whitespace(), Language.Grammar.Comment());
    }});
    
    Func.Rule(l, function AttributeSections() { with (l) { return alt(
      project( bind("item", AttributeSection()), arrayWrapRaw)
    , project( bind("list", AttributeSections()), function() {
        this.result = array(valuesof(this.getVar("list")), this.getVar(item));}));
    }});
    
    Func.Rule(l, function AttributeSection() { with (l) {
      return project(
        oiseq( any('@{'), bind('list', GraphNodes()), any('}'))
      , function() { this.result = { AttributeSection: { Nodes: this.getVar("list")}}});
    }});
    
    Func.Rule(l, function GraphNodes() { with (l) { return either(
      project(oiseq(bind("item", GraphNode()), opt(any(','))), function() {
        this.result = [this.getVar("item")]})
    , project(oiseq(
        bind('list', GraphNodes())
      , bind("item", GraphNode()), opt(any(','))), function() {
          this.result = array(valuesof(this.getVar("list")), this.getVar(item))}));
    }});
    
    Func.Rule(l, function GraphNode() { with (l) { return alt(
      project(GraphLabel(), function() { this.result =
        { GraphNode: { GraphLabel: this.rawResult}}})
    , project(Literal(), function() { this.result =
        { GraphNode: { GraphLiteral: this.rawResult}}})
    , resultRaw(GraphOrderedNode())
    , resultRaw(GraphUnorderedNode())
    , resultRaw(GraphUnorderedNode()));
    }});
    
    Func.Rule(l, function GraphOrderedNode() { with (l) { return project(oiseq(
      bind("label", opt(GraphLabel()))
    , any('[')
    , bind("list", opt(GraphNodes()))
    , any(']')), function() { this.result = { GraphNode: { Label: this.getVar("label"),
        OrderedNodes: this.getVar("list") }}});
    }});
    
    Func.Rule(l, function GraphUnorderedNode() { with (l) { return project(oiseq(
      bind("label", opt(GraphLabel()))
    , any('{')
    , bind("list", opt(GraphNodes()))
    , any('}')), function() { this.result = { GraphNode: { Label: this.getVar("label"),
        UnorderedNodes: this.getVar("list") }}});
    }});
    
    Func.Rule(l, function GraphEntityNode() { with (l) { return project(oiseq(
      bind("label", opt(GraphLabel()))
    , any('{')
    , bind("list", opt(GraphLabeledAtoms()))
    , any('}')), function() { this.result = { GraphNode: { Label: this.getVar("label"),
        UnorderedNodes: this.getVar("list") }}});
    }});
    
    Func.Rule(l, function GraphLabeledAtoms() { with (l) { return either(
      resultRawArray(GraphLabeledAtom)
    , project(oiseq(bind("list", GraphLabeledAtoms())
      , any(",")
      , bind("item", GraphLabeledAtom())), function() { this.result =
          array(valuesof(this.getVar("list")), this.getVar("item"))}))
    }});
    
    Func.Rule(l, function GraphLabeledAtom() { with (l) { return project(
      oiseq(bind("label", GraphLabel()), any('='), bind("node", GraphNode()))
    , function() { this.result = { GraphNode: { Label: this.getVar("label"),
        Node: this.getVar("node")}}})
    }});
    
    Func.Rule(l, function GraphLabel() { with (l) { return project(
      Grammar.Identifier()
    , function() { this.result = { Label: { Text: this.result }}})
    }});
    
    Func.Rule(l, function Literal() { with (l) { return alt(
      resultRaw(BooleanLiteral())
    , resultRaw(IntegerLiteral())
    , resultRaw(RealLiteral())
    , resultRaw(StringLiteral())
    , resultRaw(NullLiteral()))
    }});
    
    Func.Rule(l, function GrammarReference() { with (l) {
      var TypeArgument = this.args[0];
    // snore
    }});
    
    
  }});
  
  l.__Language(function MGrammar(l) {
    
    Func.Rule(l, function Main() { return l.project(
      l.bind("c", l.CompilationUnit()), function() {
        this.result = this.getVar("c") })});
    
    Func.Rule(l, function Skippable() { return l.either(
      l.Whitespace(), l.Comment()) });
    
    Func.Rule(l, function Comment() { return l.CommentToken });
    
    Func.Rule(l, function CompilationUnit() { return l.project(
      l.bind("decls", l.star(l.ModuleDeclaration())),
      function() { this.result = 
        { CompilationUnit: { Modules : this.getVar('decls') }}})});
    
    Func.Rule(l, function ExportDirective() { return l.project(
      l.iseq( l.any('export'), l.bind("members", SimpleNames()),
        l.any(';')), function() { this.result = {
          ExportDirective: { Names: this.getVar("members")}}})});
    
    Func.Rule(l, function ImportAlias() {  });

  }).Export();
});

























