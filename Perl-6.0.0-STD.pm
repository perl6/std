grammar Perl-6.0.0-STD;          # (XXX maybe should be -PROTO or some such)

=begin things todo

    sublanguages
    harvest symbol bits from regex, install endsyms (or does engine do this?)
    add more suppositions and figure out exact error continuation semantics
    finish out all the {*} #= hookage
    think about longest-token-defeating {*} that maybe should be <?{ {*}; 1}>
    add parsing this file to sanity tests :)

=end things todo

# This file is designed to be either preprocessed into a grammar with
# action statements or used as-is without any preprocessing.  The {*}
# notation is a no-op action block, but can be identified uniquely via a
# combination of the preceding token or rule name plus any additional text
# following a #= comment.  We put this into a comment rather than using
# a macro so that bootstrap compilers don't have to worry about macros
# yet, and to keep the main grammar relatively uncluttered by action
# statements.  Note that the preprocessor can certainly generate accesses
# to $/ within the action block, so we need not mention it explicitly.
# Also, some rules are named by syntactic category plus the symbols
# parsed by the rule.  These extra parts of the operator name are mentioned
# in comments that begin #+.

# Note that rules with only one action need no #= comment, so the identifier
# of the following stub is just "TOP".

token TOP { <UNIT( $+unitstop or /$/ )> {*} }

# This grammar also assumes transitive longest-token semantics, though
# we make a feeble attempt to order rules so a procedural | can usually
# produce a correct parse.

=begin comment linkage

  XXX random inconsistent ideas

  From the viewpoint of the user:
    complete replacement of grammar till end of file:
            use only MyGrammar;
        turns into something like:
            use MyGrammar :my<$?PARSER>;    # XXX how to export this?
        which ends up looking like something below...
            use MyGrammar;
            BEGIN {
                temp COMPILING::<$?PARSER> := MyGrammar;
                my $more = m:p/ <$?PARSER::UNIT(/$/)>/;
                # XXX how to we attach $more to the prior tree?
            }

    complete replacement of grammar till end of scope:
            use MyGrammar;
            BEGIN {
                temp COMPILING::<$?PARSER> := MyGrammar;
                my $more = m:p/ <$?PARSER::UNIT(/<null>/)>/;
                # XXX how to we attach $more to the prior tree?
            }

    mutating existing grammar by derivation:
        # (Presumably normal subroutine defs don't need to mutate the grammar)
        BEGIN {
            temp COMPILING::<$?PARSER> := grammar is OUTER::<$?PARSER> {
                token infix returns Additive { :<plus> }
            }
            my $more = m:p/ <$?PARSER::UNIT(/<null>/)>/;   # XXX or some such
            ...
        }

    Note that these BEGIN blocks parse the rest of a scope as kind of
        a "compilation continuation".  The temp restores the old parser
        at the end of the begin block, which is presumably coincident
        with the end of the user's current scope if the rule ended up
        in the right spot.  Note also that, within the BEGIN block,
        $_ might be the current program being parsed!

    These also assume a cooperative subgrammar that knows how to quit
        on other stoppers than just /$/.  In the absence of that we
        might need to snip out a substring to feed the subgrammar.
        Of course, this means finding a delimiter that can't occur
        in the substring, or preparsing the subgrammar somehow to
        find the right closer, neither of which is exactly optimal.

=end comment linkage

# The internal precedence levels are *not* part of the public interface.
# The current values are mere implmentation; they may change at any time.
# Users should specify precedence only in relation to existing levels.

constant %term              ::= { :prec<z=>                           };
constant %methodcall        ::= { :prec<w=>                           };
constant %autoincrement     ::= { :prec<v=>, :lvalue                  };
constant %exponentiation    ::= { :prec<u=>, :assoc<right>            };
constant %symbolic_unary    ::= { :prec<t=>                           };
constant %multiplicative    ::= { :prec<s=>, :assoc<left>             };
constant %additive          ::= { :prec<r=>, :assoc<left>             };
constant %junctive_and      ::= { :prec<q=>, :assoc<list>             };
constant %junctive_or       ::= { :prec<p=>, :assoc<list>             };
constant %named_unary       ::= { :prec<o=>, :assoc<left>             };
constant %nonchaining       ::= { :prec<n=>, :assoc<non>              };
constant %chaining          ::= { :prec<m=>, :assoc<chain>, :bool     };
constant %tight_and         ::= { :prec<l=>, :assoc<left>             };
constant %tight_or          ::= { :prec<k=>, :assoc<left>             };
constant %conditional       ::= { :prec<j=>, :assoc<right>, :!assign  };
constant %item_assignment   ::= { :prec<i=>, :assoc<right>, :lvalue   };
constant %loose_unary       ::= { :prec<h=>,                          };
constant %comma             ::= { :prec<g=>, :assoc<list>, :!assign   };
constant %list_infix        ::= { :prec<f=>, :assoc<list>, :!assign   };
constant %list_prefix       ::= { :prec<e=>,                          };
constant %loose_and         ::= { :prec<d=>, :assoc<left>, :!assign   };
constant %loose_or          ::= { :prec<c=>, :assoc<left>, :!assign   };
constant %LOOSEST           ::= { :prec<a=!>,                         };
constant %terminator        ::= { :prec<a=>, :assoc<list>             };

# "epsilon" tighter than terminator
my $LOOSEST = %LOOSEST<prec>;

role PrecOp[*%defaults] {
    has Str $.prec = "zzz";
    has Str $.assoc = "";
    has Str $.symbol = "?unk?";
    has Bool $.lvalue = False;
    has Bool $.bool = False;
    has Bool $.assign = True;

    has Match $.match handles *;  # in particular, handles .<foo> and .[0]

    # This is hopefully called to coerce a match into an operator object.
    method &.(Match $m) {
        # Note, binding $<prec> or $<assoc> in match overrides default here!
        my $self = $.bless(|%$m, |%defaults, :match($m));
        $self.symbol = join ' ', gather { ... };  # XXX don't know how yet
        if $self.assoc {
            $+thisop = $self;
        }
        return $self;
    }
}

class Term            does PrecOp[|%term]                   {}
class Methodcall      does PrecOp[|%methodcall]             {}
class Autoincrement   does PrecOp[|%autoincrement]          {}
class Exponentiation  does PrecOp[|%exponentiation]         {}
class Symbolic_unary  does PrecOp[|%symbolic_unary]         {}
class Multiplicative  does PrecOp[|%multiplicative]         {}
class Additive        does PrecOp[|%additive]               {}
class Junctive_and    does PrecOp[|%junctive_and]           {}
class Junctive_or     does PrecOp[|%junctive_or]            {}
class Named_unary     does PrecOp[|%named_unary]            {}
class Nonchaining     does PrecOp[|%nonchaining]            {}
class Chaining        does PrecOp[|%chaining]               {}
class Tight_and       does PrecOp[|%tight_and]              {}
class Tight_or        does PrecOp[|%tight_or]               {}
class Conditional     does PrecOp[|%conditional]            {}
class Item_assignment does PrecOp[|%item_assignment]        {}
class Loose_unary     does PrecOp[|%loose_unary]            {}
class Comma           does PrecOp[|%comma]                  {}
class List_infix      does PrecOp[|%list_infix]             {}
class List_prefix     does PrecOp[|%list_prefix]            {}
class Loose_and       does PrecOp[|%loose_and]              {}
class Loose_or        does PrecOp[|%loose_or]               {}
class Terminator      does PrecOp[|%terminator]             {}

# Categories are designed to be easily extensible in derived grammars
# by merely adding more rules in the same category.

# The endsym trait, if specified, says what to implicitly check for in each
# rule right after the initial :<symbol>.  Normally this is used to make sure
# there's appropriate whitespace, though Perl 6 also uses it to rule out
# the => (fatarrow) construct.

token category
        { :<category> }
proto token category
        { }

token category
        { :<sigil> }
proto token sigil
        { }

token category
        { :<twigil> }
proto token twigil
        { }

token category
        { :<special_variable> }
proto token special_variable
        { }

token category
        { :<nameroot> }
proto token nameroot
        { }

token category
        { :<version> }
proto token version
        { }

token category
        { :<term> }
proto token term
        { }

token category
        { :<quote> }
proto token quote
        { }

token category
        { :<prefix> }
proto token prefix
            is defequiv(%symbolic_unary)        # XXX not sure how used yet
        { }

token category
        { :<infix> }
proto token infix
            is defequiv(%additive)
        { }

token category

        { :<postfix> }
proto token postfix
            is defequiv(%autoincrement)
        { }

token category
        { :<dotty> }
proto token dotty
            is endsym(/ <?unsp>? /
        { }

token category
        { :<circumfix> }
proto token circumfix
        { }

token category
        { :<postcircumfix> }
proto token postcircumfix
        { }

token category
        { :<regex_metachar> }
proto token regex_metachar
        { }

token category
        { :<regex_backslash> }
proto token regex_backslash
        { }

token category
        { :<regex_assertion> }
proto token regex_assertion
        { }

token category
        { :<regex_mod_internal> }
proto token regex_mod_internal
        { }

token category
        { :<regex_mod_external> }
proto token regex_mod_external
            is endsym(/ <?before \(> <postcircumfix> /)
        { }

token category
        { :<quote_mod> }
proto token quote_mod
        { }

token category
        { :<q_backslash> }
proto token q_backslash
        { }

token category
        { :<qq_backslash> }
proto token qq_backslash
        { }

token category
        { :<trait_verb> }
proto token trait_verb
            is endsym(/ \s+ <nofat> /)
        { }

token category
        { :<trait_auxiliary> }
proto token trait_auxiliary
            is endsym(/ \s+ <nofat> /)
        { }

token category
        { :<type_declarator> }
proto token type_declarator
            is endsym(/ >> <nofat> /)
        { }

token category
        { :<scope_declarator> }
proto token scope_declarator
            is endsym(/ >> <nofat> /)
        { }

token category
        { :<package_declarator> }
proto token package_declarator
            is endsym(/ >> <nofat> /)
        { }

token category
        { :<routine_declarator> }
proto token routine_declarator
            is endsym(/ >> <nofat> /)
        { }

token category
        { :<statement_prefix> }
proto rule  statement_prefix
            is endsym(/ >> <nofat> /)
        { }

token category
        { :<statement_control> }
proto rule  statement_control
            is endsym(/ \s <nofat> /)
        { }

token category
        { :<statement_mod_cond> }
proto rule  statement_mod_cond
            is endsym(/ >> <nofat> /)
        { }

token category
        { :<statement_mod_loop> }
proto rule  statement_mod_loop
            is endsym(/ >> <nofat> /)
        { }

token category
        { :<infix_prefix_meta_operator> }
proto token infix_prefix_meta_operator
        { }

token category
        { :<infix_postfix_meta_operator> }
proto token infix_postfix_meta_operator
        { }

token category
        { :<postfix_prefix_meta_operator> }
proto token postfix_prefix_meta_operator
        { }

token category
        { :<prefix_postfix_meta_operator> }
proto token prefix_postfix_meta_operator
        { }

token category
        { :<prefix_circumfix_meta_operator> }
proto token prefix_circumfix_meta_operator
        { }

# Lexical routines

# make sure we're not an autoquoted identifier
regex nofat { <!before \h* <?unsp>? =\> > }

token ws {
    || <?after \w> <?before \w> ::: <fail>        # must \s+ between words
    || [
       | <unsp>              {*}                                #= unsp
       | \v                  {*} <heredoc>                      #= vwhite
       | <unv>               {*}                                #= unv
       ]*  {*}                                                  #= all
}

token unsp {
    \\ <?before [\s|\#]>
    [
    | \v                     {*}                                #= vwhite
    | <unv>                  {*}                                #= unv
    ]*  {*}                                                     #= all
}

token unv {
       | \h+                 {*}                                #= hwhite
       | ^^ [
            | \# \N*         {*}                                #= line
            | <?pod_comment> {*}                                #= pod
            ]
       | <'#'> [
            # assuming <bracketed> defaults to standard set
            | <?bracketed>   {*}                                #= inline
            | \N*            {*}                                #= end
            ]
}

# XXX We need to parse the pod eventually to support $= variables.

token pod_comment {
    ^^ =
    [
    | begin <?ws> <ident> .*? \n
      =end <?ws> $<ident> \N* \n?                   {*}         #= block
    | \N* \n?                                       {*}         #= misc
    ]
    {*}
}

# Top-level rules

method UNIT ($unitstop is context = /$/) {
    UNIT: do {
        m:p/ <comp_unit> /;
    }
}

# Note: we only check for the unitstopper.  We don't check for ^ because
# we might be embedded in something else.
rule comp_unit (:$begin_compunit is context = 1)
    <package_declarator>?
    { $begin_compunit = 0 }
    <statement_list>
    [ <$+unitstop> || <panic: Can't understand next input--giving up> ]
    {*}
}

token block {
    \{
    <statement_list>
    [ \} || <panic: Missing right brace> ]
    [
    | <?unsp>? <?before <[,:]>> {*}                             #= normal 
    | <?before <?unv>? \n > {*} { let $<endline> := 1; }        #= endline
    | {*} { let $<endlist> := 1; }                              #= endlist
    ]
    {*}
}

token regex_block {  # perhaps parameterize and combine with block someday
    \{
    <regex \}>
    [ \} || <panic: Missing right brace> ]
    [
    | <?unsp>? <?before <[,:]>> {*}                             #= normal
    | <?before <?unv>? \n > {*} { let $<endline> := 1; }        #= endline
    | {*} { let $<endlist> := 1; }                              #= endlist
    ]
    {*}
}

rule statement_list {
    <statement>*
    {*}
}

token label {
    <ident> \: \s <?ws>

    [ <?{ is_type($<ident>) }>
      <suppose: You tried to use an existing name $<ident> as a label>
    ]?

    # add label as a pseudo type
    { COMPILING::{"::$<ident>"} = Label.new($<ident>) }  # XXX need statement ref too?

    {*}
}

rule statement {
    <label>*                                     {*}            #= label
    [
    | <statement_control>                        {*}            #= control
    | <block>                                    {*}            #= block
    | <EXPR>                                     {*}            #= expr
        [<statement_mod_cond> <EXPR> {*} ]?                     #= mod cond
        [<statement_mod_loop> <EXPR> {*} ]?                     #= mod loop
    ]
    {*}
}

rule statement_control {
    :<use>
    <module_name_wild> <EXPR>? ;?                       {*} #=  #? use
}

rule statement_control {
    :<if>
    <EXPR>                           {*}                        #= if expr
    <block>                          {*}                        #= if block
    @<elsif> := [ elsif <EXPR>       {*}                        #= elsif expr
                        <block>      {*} ]*                     #= elsif block
    @<else> := [ else <block>        {*} ]?                     #= else
    {*}
}

rule statement_control {
    :<unless>
    <EXPR>                           {*}                        #= unless expr
    <block>                          {*}                        #= unless block
    {*}
}

rule statement_control {
    :<while>
    <EXPR>                             {*}                      #= while expr
    <block>                            {*}                      #= while block
    {*}
}

rule statement_control {
    :<until>
    <EXPR>                             {*}                      #= until expr
    <block>                            {*}                      #= until block
    {*}
}
rule statement_control {
    :<repeat>
    [
        | (while|until) <EXPR>         {*}                      #= rep wu expr
          <block>                      {*}                      #= rep wu block
        | <block>                      {*}                      #= rep block wu
          (while|until) <EXPR>         {*}                      #= rep expr wu
    ]
    {*}
}
rule statement_control {
    :<loop>
    $<eee> := [
        \(
            $<e1> := <EXPR> ;   {*}                             #= loop e1
            $<e2> := <EXPR> ;   {*}                             #= loop e2
            $<e3> := <EXPR>     {*}                             #= loop e3
        \)                      {*}                             #= loop eee
    ]?
    <block>                     {*}                             #= loop block
    {*}
}

rule statement_control { :<for>     <block> {*} }               #= for
rule statement_control { :<when>    <block> {*} }               #= when
rule statement_control { :<BEGIN>   <block> {*} }               #= BEGIN
rule statement_control { :<CHECK>   <block> {*} }               #= CHECK
rule statement_control { :<INIT>    <block> {*} }               #= INIT
rule statement_control { :<END>     <block> {*} }               #= END
rule statement_control { :<START>   <block> {*} }               #= START
rule statement_control { :<ENTER>   <block> {*} }               #= ENTER
rule statement_control { :<LEAVE>   <block> {*} }               #= LEAVE
rule statement_control { :<KEEP>    <block> {*} }               #= KEEP
rule statement_control { :<UNDO>    <block> {*} }               #= UNDO
rule statement_control { :<FIRST>   <block> {*} }               #= FIRST
rule statement_control { :<NEXT>    <block> {*} }               #= NEXT
rule statement_control { :<LAST>    <block> {*} }               #= LAST
rule statement_control { :<PRE>     <block> {*} }               #= PRE
rule statement_control { :<POST>    <block> {*} }               #= POST
rule statement_control { :<CATCH>   <block> {*} }               #= CATCH
rule statement_control { :<CONTROL> <block> {*} }               #= CONTROL

token statement_control { %statement_control }

rule modifier_expr { <EXPR> ;? {*} }

rule statement_mod_cond { :<if>     <modifier_expr> {*} };      #= if
rule statement_mod_cond { :<unless> <modifier_expr> {*} };      #= unless
rule statement_mod_cond { :<when>   <modifier_expr> {*} };      #= for

rule statement_mod_loop { :<for>    <modifier_expr> {*} };      #= for
rule statement_mod_loop { :<given>  <modifier_expr> {*} };      #= for
rule statement_mod_loop { :<while>  <modifier_expr> {*} };      #= while
rule statement_mod_loop { :<until>  <modifier_expr> {*} };      #= until

token nameroot { <'perl6'> }
token nameroot { <'perl5'> }
token nameroot { <'parrot'> }
token nameroot { <'ruby'> }
token nameroot { <'python'> }
token nameroot { <'tcl'> }
token nameroot { <'js'> }
token nameroot { <'scheme'> }
token nameroot { <'lisp'> }
token nameroot { <'haskell'> }
token nameroot { <'java'> }
token nameroot { <'c'> }
token nameroot { <'cplusplus'> }
token nameroot { <'csharp'> }
token nameroot { <'ada'> }
token nameroot { <'lua'> }
token nameroot { <'php'> }

token module_name {
    <name>                                          {*}         #= name
    [- <version>                                    {*}         #= version
        [-
            <authority>                             {*}         #= auth
        ]?
    ]?
    {*}
}

token authority { <-[ \s ; \{ ]>+ }

token module_name_wild {
    [ <nameroot> \: {*} ]?                                      #= root
    <name>                                          {*}         #= name
    [- <version_wild>                               {*}         #= version
        [-
            <authority_wild>                        {*}         #= auth
        ]?
    ]?
    {*}
}

token version_wild   { <block> | <whatever> | <version> }
token authority_wild { <block> | <whatever> | <authority> }

token whatever { \* {*} }

token version {
    v \d+ [ \. \d+ ]*                 {*}                       #= vstyle
}
token version {
    \d+ \. \d+ \. \d+ [ \. \d+]*      {*}                       #= dotted
}

###################################################

token expect_term {
    <?ws>

    [
        [
        | <prefix>                                      {*}     #= prefix
        | <prefix_circumfix_meta_operator>              {*}     #= precircum
        ]
        <prefix_postfix_meta_operator>*                 {*}     #= prepost
    ]*

    <noun>                                              {*}     #= noun
    <expect_postfix>*                                   {*}     #= postfix
    <?ws>
    <adverbs>?
    {*}
}

token adverbs {
    [ <colonpair> <?ws> ]+
    {
        my $prop = $+prevop err
            panic('No previous operator visible to adverbial pair (' ~
                $<colonpair> ~ ')');
        $prop.adverb($<colonpair>)
    }
    {*}
}

token noun {
    [
    | <pair>
    | <package_declarator>
    | <scope_declarator>
    | <plurality_declarator>
    | <routine_declarator>
    | <regex_declarator>
    | <type_declarator>
    | <circumfix>
    | <variable>
    | <value>
    | <subcall>
    | <capterm>
    | <sigterm>
    | <quote>
    | <term>
    | <statement_prefix>
    ]
    {*}
}

token pair {
    [
    | $<key>:=<ident> \h* =\> $<val>:=<EXPR(%assignment)>
                                                        {*}     #= fat
    | [ <colonpair> <?ws> ]+
                                                        {*}     #= colon
    ]
    {*}
}

token colonpair {
    [
    | \: !? <ident>                                     {*}     #= bool
    | \: <ident>? <unsp>? <postcircumfix>               {*}     #= value
    ]
    {*}
}

regex infix_nospace {
    <expect_infix>
    <!{ $<expect_infix> ~~ /\s/ }>
}

token expect_tight_infix ($loosest) {
    <!before \{ | -\> >                # presumably a statement control block
    <expect_infix>
    ::: <?{ $+thisop.prec ge $loosest }>
}

token expect_infix {
    <infix>
    <infix_postfix_meta_operator>*
    {*}
}

token dotty { :<.+> <methodop>                      {*}         #= plus
token dotty { :<.*> <methodop>                      {*}         #= star
token dotty { :<.?> <methodop>                      {*}         #= query
token dotty { :<.=> <methodop>                      {*}         #= equals
token dotty { :<.^> <methodop>                      {*}         #= caret
token dotty { :<.:> <methodop>                      {*}         #= colon
token dotty { :<.>  <dottyop>                       {*}         #= plain

token dottyop {
    [
    | methodop
    | postop
    ]
}

# Note, this rule mustn't do anything irreversible because it's used
# as a lookahead by the quote interpolator.

token expect_postfix {
    [
    | \\ <?before \.>
    | <?unsp>?
    ]

    [ [\. <?unsp>?]? <postfix_prefix_meta_operator> <?unsp>? ]*

    [
    | <dotty>
    | <postop>
    ]
    {*}
}

# Note: backtracks, or we'd never get to parse [LIST] on seeing [+ and such.
regex prefix_circumfix_meta_operator (:$thisop? is context ) {     #+ [ ]
    :<[> \\?? <infix_nospace> :<]>   # prefer no meta \ if op has \

    [ <!{ $+thisop<assoc> eq 'non' }>
        || <panic: Can't reduce a non-associative operator> ]

    [ <!{ $+thisop.prec eq %conditional<prec> }>
        || <panic: Can't reduce a conditional operator> ]


    {*}                                                         #= [ ]
}

token prefix_postfix_meta_operator { :<«>     {*} }             #= hyper
token prefix_postfix_meta_operator { :['<<'] {*} }              #= HYPER

token postfix_prefix_meta_operator { :<»>     {*} }             #= hyper
token postfix_prefix_meta_operator { :['>>'] {*} }              #= HYPER

token infix { <infix_prefix_meta_operator> }
token infix { <infix_circumfix_meta_operator> }

token infix_prefix_meta_operator {                            #+ !
    :<!> <!before !> <infix_nospace>

    <?nonest: negation>

    [
    || <?{ $+thisop<assoc> eq 'chain'}>
    || <?{ $+thisop<assoc> and $+thisop<boolean> }>
    || <panic: Only boolean infix operators may be negated>
    ]

    { $+thisop<hyper> and panic("Negation of hyper operator not allowed") }

    {*}                                                         #= !
}

regex nonest (Str $s) {
    <!{ $+thisop{$s}++ }> || <panic: Nested $s metaoperators not allowed>
}

token infix_circumfix_meta_operator {                         #+ X X
    :<X> <infix_nospace> :<X>
    <nonest: cross>
    {*}                                                         #= X X
}

token infix_circumfix_meta_operator {                         #+ « »
    :<«> <infix_nospace> :<»>
    <nonest: hyper>
    {*}                                                         #= « »
}

token infix_circumfix_meta_operator {                         #+ « «
    :<«> <infix_nospace> :<«>
    <nonest: hyper>
    {*}                                                         #= « «
}

token infix_circumfix_meta_operator {                         #+ » »
    :<»> <infix_nospace> :<»>
    <nonest: hyper>
    {*}                                                         #= » » 
}

token infix_circumfix_meta_operator {                         #+ » «
    :<»> <infix_nospace> :<«>
    <nonest: hyper>
    {*}                                                         #= » «
}

token infix_circumfix_meta_operator {                         #+ << >>
    :['<<'] <infix_nospace> :['>>']
    <nonest: hyper>
    {*}                                                         #= << >>
}

token infix_circumfix_meta_operator {                         #+ << <<
    :['<<'] <infix_nospace> :['<<']
    <nonest: hyper>
    {*}                                                         #= << <<
}

token infix_circumfix_meta_operator {                         #+ >> >>
    :['>>'] <infix_nospace> :['>>']
    <nonest: hyper>
    {*}                                                         #= >> >>
}

token infix_circumfix_meta_operator {                         #+ >> <<
    :['>>'] <infix_nospace> :['<<']
    <nonest: hyper>
    {*}                                                         #= >> <<
}

token infix_postfix_meta_operator {                           #+ =
    :<=>
    <nonest: assignment>

    [
    || <?{ $+thisop.prec gt %item_assignment<prec> }
    || <panic: Can't make assignment op of operator looser than assignment>
    ]

    [
    || <!{ $+thisop<assoc> eq 'chain' }
    || <panic: Can't make assignment op of boolean operator>
    ]
    
    [
    || <!{ $+thisop<assoc> eq 'non' }
    || <panic: Can't make assignment op of non-associative operator>
    ]
    
    { $+thisop = %item_assignment }  # force assignment precedence
    {*}                                                         #= =
}

token postfix { :<i> {*} }                                      #= i
token postfix { :<++> {*} }                                     #= incr
token postfix { :<--> {*} }                                     #= decr

token postcircumfix { :<(> <EXPR> :<)> {*} }                    #= ( )

token postcircumfix { :<[> <EXPR> :<]> {*} }                    #= [ ]

token postcircumfix { :<{> <EXPR> :<}> {*} }                    #= { }

token postcircumfix { :['<'] <anglewords> :['>'] {*}            #= < > }

token postcircumfix { :['<<'] <shellwords> :['>>'] {*}          #= << >>
}

token postcircumfix { :<«> <shellwords> :<»> {*} }              #= « »

token postop { <postfix> | <postcircumfix> }

token methodop {
    [
    | <ident>
    | <?before \$> <variable>
    | <?before <[ ' " ]>> <quote>
    ]

    [
    | \.? \( <EXPR> \)
    | \: <?before \s> <!{ $+inquote }> <listop_expr>
    | <null>
    ]
    {*}
}

token circumfix { :<(> <EXPR> :<)> {*} }                        #= ( )
token circumfix { :<[> <EXPR> :<]> {*} }                        #= [ ]

token circumfix { :['<']  <anglewords>  :['>'] {*} }            #= < >
token circumfix { :['<<'] <shellwords> :['>>'] {*} }            #= << >>
token circumfix { :<«>    <shellwords> :<»>    {*} }            #= « »

token circumfix ( --> Circumfix)                                #+ { }
    { <?before :<{> <block> <?after :<}>  {*} }                 #= { }

rule scoped {
    <type>?
    [
    | <variable>
    | \( <signature> \)
    | <package_declarator>
    | <plurality_declarator>
    | <routine_declarator>
    | <regex_declarator>
    | <type_declarator>
    ]
    <trait>*
    {*}
}

token scope_declarator { :<my>       <scoped> {*} }             #= my
token scope_declarator { :<our>      <scoped> {*} }             #= our
token scope_declarator { :<state>    <scoped> {*} }             #= state
token scope_declarator { :<constant> <scoped> {*} }             #= constant
token scope_declarator { :<has>      <scoped> {*} }             #= has

token package_declarator { :<class>   <package_def> {*} }       #= class
token package_declarator { :<grammar> <package_def> {*} }       #= grammar
token package_declarator { :<module>  <package_def> {*} }       #= module
token package_declarator { :<role>    <package_def> {*} }       #= role
token package_declarator { :<package> <package_def> {*} }       #= package

token package_def {
    <module_name>?
    <trait>* {*}                                                #= traits
    [
    || <?{ $+begin_compunit } :: \;
        { defined $<module_name> or
            panic("Compilation unit cannot be anonymous"
        }
        {*}                                                     #= semi
    || <block>
        {*}                                                     #= block
    ]
    {*}
}

rule pluralized {
    [
    | <variable>
    | \( <signature> \)
    | <package_declarator>
    | <routine_declarator>
    | <regex_declarator>
    | <type_declarator>
    ]
    {*}
}

token plurality_declarator { :<multi> <pluralized> {*} }        #= multi
token plurality_declarator { :<proto> <pluralized> {*} }        #= proto
token plurality_declarator { :<only>  <pluralized> {*} }        #= only

token routine_declarator { :<sub>       <routine_def> {*} }     #= sub
token routine_declarator { :<method>    <method_def> {*} }      #= method
token routine_declarator { :<submethod> <method_def> {*} }      #= submethod
token routine_declarator { :<macro>     <macro_def> {*} }       #= macro

token regex_declarator { :<regex>       <regex_def> {*} }       #= regex
token regex_declarator { :<token>       <regex_def> {*} }       #= token
token regex_declarator { :<rule>        <regex_def> {*} }       #= rule

token special_variable { :<$!>  {*} }                           #= $!
token special_variable { :<$/>  {*} }                           #= $/

token variable {
    [
    | <special_variable> {*}                                    #= special
    | <sigiltwigil>
        [
        || <?{ $<sigiltwigil><sigil> eq '&' }> :: <sublongname>
        || <name>
        ]
        [
        | <?{ $<sigiltwigil><twigil> eq '.' }>
            <?unsp>? <?before \(> <postcircumfix> {*}           #= methcall
        | <null> {*}                                            #= $?foo
        ]
    | <sigil> \d+ {*}                                           #= $0
    | <sigil> <?before \< | \(> <postcircumfix> {*}             #= $()
    | <name> <'::'> <hashpostfix> {*}                           #= FOO::<$x>
    ]
    {*}
}

token sigiltwigil {
    <sigil> <twigil>?
    <?{ {*}; 1 }>               # XXX right way to allow use in longer token?
}

# Note, don't reduce on a bare sigil unless you don't want a twigil or
# you otherwise don't care what the longest token is.

token sigil { :<$>  }
token sigil { :<@@> }
token sigil { :<@>  }
token sigil { :<%>  }
token sigil { :<&>  }
token sigil { :<::> }

token twigil { :<.> }
token twigil { :<!> }
token twigil { :<^> }
token twigil { :<*> }
token twigil { :<+> }
token twigil { :<?> }
token twigil { :<=> }

token name {
    [
    | <ident> <nofat> [ <'::'> <ident> ]*
    | [ <'::'> <ident> ]+
    ]
    {*}
}

token subshortname {
    [
    | <name>
    | <category> \: <?before \< | \{ > <postcircumfix>
    ]
    {*}
}

token sublongname {
    <subshortname>
    [
    | <capterm>
    | <sigterm>
    | <null>
    ]
    {*}
}

token subcall {
    <subshortname> <?unsp>? \.? \( <EXPR> \)
    {*}
}

token value {
    [
    | <string>
    | <number>
    | <version>
    | <fulltypename>
    ]
    {*}
}

token typename {
    <name>
    <?{
        is_type($<name>)
    }>
    {*}
}

regex fulltypename {
    <typename> <?unsp>?
    [
    | <?before \[> <postcircumfix>
    | <?ws> of <?ws> <fulltypename>
    | <null>
    ]
    {*}
}

token number {
    [
    | <integer>
    | <dec_number>
    | <radix_number>
    ]
    {*}
}

token integer {
    [
    | 0 [ b <[01]>+           [ _ <[01]>+ ]*
        | o <[0..7]>+         [ _ <[0..7]>+ ]*
        | x <[0..9a..fA..F]>+ [ _ <[0..9a..fA..F]>+ ]*
        | d \d+               [ _ \d+]*
        ]
    | \d+[_\d+]*
    ]
    {*}
}

token radint {
    [
    | <integer>
    | <rad_number> <?{
                        defined $<rad_number><radint>
                        and
                        not defined $<rad_number><radfrac>
                   }>
    ]
    {*}
}

token dec_number {
    \d+[_\d+]* [ \. \d+[_\d+]* [ <[Ee]> <[+\-]>? \d+ ]? ]
    {*}
}

token rad_number {
    \: $<radix> := [\d+] 
    [
    || \<
            $<radint> := [<[ 0..9 a..z A..Z ]>+
            $<radfrac> := [ \. <[ 0..9 a..z A..Z ]>+ ]? ]
            [ \* $<base> := <radint> \*\* $<exp> := <radint> ]?
       \>
      { return radcalc($<radix>, $<radnum>, $<base>, $<exp>) }
    || <?before \[> <postcircumfix>
    || <?before \(> <postcircumfix>
    ]
    {*}
}

our @herestub_queue;

# XXX maybe shouldn't be hash, but for now...

our %quote_adverb := {
    Q => {                          # base form of all quotes
        adverbs => &Q_adverb,
        parser => &q_pickdelim,
        escset => < >,
        escrule => &quote_escapes,
    },
    q  => {
        adverbs => &q_quote_adverb,
        parser => &q_pickdelim,
        escset => < \\ >,
        escrule => &quote_escapes,
    },
    qq => {
        adverbs => &q_quote_adverb,
        parser => &q_pickdelim,
        escset => < \\ $ @ % & { >,
        escrule => &quote_escapes,
    },
    b => {
        escadd => < \\ >,
    },
    s => {
        escadd => < $ >,
    },
    a => {
        escadd => < @ >,
    },
    h => {
        escadd => < % >,
    },
    f => {
        escadd => < & >,
    },
    c => {
        escadd => < { >,
    },
    to => {
        adverbs => &q_quote_adverb,
        parser => &q_herestub,
        escadd => < ^^ >,          #  grabs leading whitespace
    },
    rx => {
        adverbs => &q_regex_adverb,
        parser => &q_regex,
        escset => < >,             # let regex parser handle everything
    },
    m => {
        adverbs => &q_regex_adverb,
        parser => &q_regex,
        escset => < >,             # let regex parser handle everything
    },
    s => {
        adverbs => &q_regex_adverb,
        parser => &q_regex,
        escset => < >,             # let regex parser handle everything
    },
    tr => {
        adverbs => &q_trans_adverb,
        parser => &q_trans,
        escset => < >,             # let trans parser handle everything
    },
};

our %regex_adverb := {
    g => {
        ...
    },
    # XXX --more--
};

our %trans_adverb := {
    d => {
        ...
    },
    # XXX --more--
};

token Q_adverb {}
token q_quote_adverb {}
token q_quote {}
token q_regex_adverb {}
token q_regex {}
token q_trans_adverb {}
token q_trans {}

token q_herestub ($lang) {
    $<delimstr> := <quotesnabber('Q')>  # force raw semantics on /END/ marker
    {
        push @herestub_queue:
            new Herestub:
                delim => $<delimstr><delimited><q><text>, # XXX or some such
                orignode => $/,
                lang => $lang;
    }
    {*}
}

class Herestub {
    has Str $.delim;
    has $.orignode;
    has $.lang;
}

# XXX be sure to temporize @herestub_queue on reentry to new line of heredocs

method heredoc {
    while my $herestub = shift @herestub_queue {
        my $delim = $herestub.delim;
        my $lang = $herestub.lang;
        my $doc;
        my $ws = "";
        my $stoppat = $delim eq "" ?? rx[^^ \h* $$]
                                   !! rx[^^ $ws:=(\h*?) $delim \h* $$ \n?];
        my @heredoc_initial_ws is context is rw;
        if m:p/$doc:=<q_unbalanced($lang, :stop($stoppat))>/ {
            if $ws and @heredoc_initial_ws {
                my $wsequiv = $ws;
                $wsequiv ~~ s/^ (\t+) /{ ' ' x ($0 * 8) }/; # per spec
                for @heredoc_initial_strings {
                    next if s/^ $ws //;   # reward consistent tabbing
                    s/^^ (\t+) /{ ' ' x ($0 * 8) }/;
                    s/^ $wsequiv // or s/^ \h+ //;
                }
            }
            $herestub.orignode<doc> = $doc;
        }
        else {
            fail("Ending delimiter $delim not found");
        }
    }
}

token quote { <before :<'>>    <quotesnabber("q")> }
token quote { <before :<">>    <quotesnabber("qq")> }
token quote { <before :['«']> <quotesnabber("q",":ww")> }
token quote { <before :['<<']> <quotesnabber("q",":ww")> }
token quote { <before :['<']>  <quotesnabber("q",":w")> }

token quote { <before :['/']>  <quotesnabber("rx")> }

# handle composite forms like qww
token quote { :<qq> <regex_mod_external>
    <quotesnabber('qq', $<regex_mod_external>)> }
token quote { :<q>  <regex_mod_external>
    <quotesnabber('q', $<regex_mod_external>)> }

token regex_mod_external { :<:w> }
token regex_mod_external { :<:ww> }
token regex_mod_external { :<:x> }
token regex_mod_external { :<:t> }
token regex_mod_external { :<:n> }
token regex_mod_external { :<:s> }
token regex_mod_external { :<:a> }
token regex_mod_external { :<:h> }
token regex_mod_external { :<:f> }
token regex_mod_external { :<:c> }
token regex_mod_external { :<:b> }

token quote { :<rx> <quotesnabber('rx')> }

token quote { :<m>  <quotesnabber('m')> }
token quote { :<mm> <quotesnabber('m', ':s')> }
token quote { :<s>  $<pat> := <quotesnabber('s')> <finish_subst($<pat>)> }
token quote { :<ss> $<pat> := <quotesnabber('s', ':s')> <finish_subst($<pat>)> }

token quote { :<tr> $<pat> := <quotesnabber('tr')> <finish_trans(<$pat>)> }

token finish_subst ($pat, :$thisop is context is rw) {
    [
    # bracketed form
    | <?{ $pat<delim> == 2 }> ::
          <?ws>
          <infix>            # looking for pseudoassign here
          { $thisop.prec == %item_assignment<prec> or
              panic("Bracketed subst must use some form of assignment" }
          $<repl> := <EXPR(%item_assignment)>
    # unbracketed form
    | $<repl> := <q_unbalanced(qlang('qq'), $pat<delim>[0])>
    ]
}

token finish_trans ($pat) {
    [
    # bracketed form
    | <?{ $pat<delim> == 2 }> ::
          <?ws>
          $<repl> := <q_pickdelim(qlang('tr'))>
    # unbracketed form
    | $<repl> := <q_unbalanced(qlang('tr'), $pat<delim>[0])>
    ]
}

# The key observation here is that the inside of quoted constructs may
# be any of a lot of different sublanguages, and we have to parameterize
# which parse rule to use as well as what options to feed that parse rule.

class QLang {
    has %.escset;
    has $.adverbs;
    has $.parser;

    submethod new ($pedigree) {
        my ($starthere, @adv) = $pedigree.split(':');
        my %start := %quote_adverbs{$starthere};
        my $self = .bless(|%start);
        for @adv {
            $self.tweak($_);
        }
    }

    method tweak ($p) {
        # XXX more bogus hashiness, we need another polymorphic dispatch here
        given $p.key {
            when 'escset' {
                ...
            }
            when 'escadd' {
                ...
            }
        }
    }
}

sub qlang ($pedigree) {
    $pedigree ~~ s:g/ \s+ //;
    (state %qlang){$pedigree} //= new QLang($pedigree);
}

token quotesnabber ($q is copy, :$delim is context is rw = '') {
    <!before \w> <nofat> ::
    <?ws>

    [ (<colonpair>) { $q ~= $0 } <?ws> ]*

    { my $lang = qlang($q) }

    # Dispatch to current lang's subparser.
    $<delimited> := <$($<lang>.parser)($<lang>)>
    { let $<delim> = $delim }
    {*}
}

# XXX should eventually be derived from current Unicode tables.
constant %open2close ::= {
    "\x0028" => "\x0029", "\x003C" => "\x003E", "\x005B" => "\x005D",
    "\x007B" => "\x007D", "\x00AB" => "\x00BB", "\x0F3A" => "\x0F3B",
    "\x0F3C" => "\x0F3D", "\x169B" => "\x169C", "\x2039" => "\x203A",
    "\x2045" => "\x2046", "\x207D" => "\x207E", "\x208D" => "\x208E",
    "\x2208" => "\x220B", "\x2209" => "\x220C", "\x220A" => "\x220D",
    "\x2215" => "\x29F5", "\x223C" => "\x223D", "\x2243" => "\x22CD",
    "\x2252" => "\x2253", "\x2254" => "\x2255", "\x2264" => "\x2265",
    "\x2266" => "\x2267", "\x2268" => "\x2269", "\x226A" => "\x226B",
    "\x226E" => "\x226F", "\x2270" => "\x2271", "\x2272" => "\x2273",
    "\x2274" => "\x2275", "\x2276" => "\x2277", "\x2278" => "\x2279",
    "\x227A" => "\x227B", "\x227C" => "\x227D", "\x227E" => "\x227F",
    "\x2280" => "\x2281", "\x2282" => "\x2283", "\x2284" => "\x2285",
    "\x2286" => "\x2287", "\x2288" => "\x2289", "\x228A" => "\x228B",
    "\x228F" => "\x2290", "\x2291" => "\x2292", "\x2298" => "\x29B8",
    "\x22A2" => "\x22A3", "\x22A6" => "\x2ADE", "\x22A8" => "\x2AE4",
    "\x22A9" => "\x2AE3", "\x22AB" => "\x2AE5", "\x22B0" => "\x22B1",
    "\x22B2" => "\x22B3", "\x22B4" => "\x22B5", "\x22B6" => "\x22B7",
    "\x22C9" => "\x22CA", "\x22CB" => "\x22CC", "\x22D0" => "\x22D1",
    "\x22D6" => "\x22D7", "\x22D8" => "\x22D9", "\x22DA" => "\x22DB",
    "\x22DC" => "\x22DD", "\x22DE" => "\x22DF", "\x22E0" => "\x22E1",
    "\x22E2" => "\x22E3", "\x22E4" => "\x22E5", "\x22E6" => "\x22E7",
    "\x22E8" => "\x22E9", "\x22EA" => "\x22EB", "\x22EC" => "\x22ED",
    "\x22F0" => "\x22F1", "\x22F2" => "\x22FA", "\x22F3" => "\x22FB",
    "\x22F4" => "\x22FC", "\x22F6" => "\x22FD", "\x22F7" => "\x22FE",
    "\x2308" => "\x2309", "\x230A" => "\x230B", "\x2329" => "\x232A",
    "\x23B4" => "\x23B5", "\x2768" => "\x2769", "\x276A" => "\x276B",
    "\x276C" => "\x276D", "\x276E" => "\x276F", "\x2770" => "\x2771",
    "\x2772" => "\x2773", "\x2774" => "\x2775", "\x27C3" => "\x27C4",
    "\x27C5" => "\x27C6", "\x27D5" => "\x27D6", "\x27DD" => "\x27DE",
    "\x27E2" => "\x27E3", "\x27E4" => "\x27E5", "\x27E6" => "\x27E7",
    "\x27E8" => "\x27E9", "\x27EA" => "\x27EB", "\x2983" => "\x2984",
    "\x2985" => "\x2986", "\x2987" => "\x2988", "\x2989" => "\x298A",
    "\x298B" => "\x298C", "\x298D" => "\x298E", "\x298F" => "\x2990",
    "\x2991" => "\x2992", "\x2993" => "\x2994", "\x2995" => "\x2996",
    "\x2997" => "\x2998", "\x29C0" => "\x29C1", "\x29C4" => "\x29C5",
    "\x29CF" => "\x29D0", "\x29D1" => "\x29D2", "\x29D4" => "\x29D5",
    "\x29D8" => "\x29D9", "\x29DA" => "\x29DB", "\x29F8" => "\x29F9",
    "\x29FC" => "\x29FD", "\x2A2B" => "\x2A2C", "\x2A2D" => "\x2A2E",
    "\x2A34" => "\x2A35", "\x2A3C" => "\x2A3D", "\x2A64" => "\x2A65",
    "\x2A79" => "\x2A7A", "\x2A7D" => "\x2A7E", "\x2A7F" => "\x2A80",
    "\x2A81" => "\x2A82", "\x2A83" => "\x2A84", "\x2A8B" => "\x2A8C",
    "\x2A91" => "\x2A92", "\x2A93" => "\x2A94", "\x2A95" => "\x2A96",
    "\x2A97" => "\x2A98", "\x2A99" => "\x2A9A", "\x2A9B" => "\x2A9C",
    "\x2AA1" => "\x2AA2", "\x2AA6" => "\x2AA7", "\x2AA8" => "\x2AA9",
    "\x2AAA" => "\x2AAB", "\x2AAC" => "\x2AAD", "\x2AAF" => "\x2AB0",
    "\x2AB3" => "\x2AB4", "\x2ABB" => "\x2ABC", "\x2ABD" => "\x2ABE",
    "\x2ABF" => "\x2AC0", "\x2AC1" => "\x2AC2", "\x2AC3" => "\x2AC4",
    "\x2AC5" => "\x2AC6", "\x2ACD" => "\x2ACE", "\x2ACF" => "\x2AD0",
    "\x2AD1" => "\x2AD2", "\x2AD3" => "\x2AD4", "\x2AD5" => "\x2AD6",
    "\x2AEC" => "\x2AED", "\x2AF7" => "\x2AF8", "\x2AF9" => "\x2AFA",
    "\x2E02" => "\x2E03", "\x2E04" => "\x2E05", "\x2E09" => "\x2E0A",
    "\x2E0C" => "\x2E0D", "\x2E1C" => "\x2E1D", "\x3008" => "\x3009",
    "\x300A" => "\x300B", "\x300C" => "\x300D", "\x300E" => "\x300F",
    "\x3010" => "\x3011", "\x3014" => "\x3015", "\x3016" => "\x3017",
    "\x3018" => "\x3019", "\x301A" => "\x301B", "\x301D" => "\x301E",
    "\xFD3E" => "\xFD3F", "\xFE17" => "\xFE18", "\xFE35" => "\xFE36",
    "\xFE37" => "\xFE38", "\xFE39" => "\xFE3A", "\xFE3B" => "\xFE3C",
    "\xFE3D" => "\xFE3E", "\xFE3F" => "\xFE40", "\xFE41" => "\xFE42",
    "\xFE43" => "\xFE44", "\xFE47" => "\xFE48", "\xFE59" => "\xFE5A",
    "\xFE5B" => "\xFE5C", "\xFE5D" => "\xFE5E", "\xFF08" => "\xFF09",
    "\xFF1C" => "\xFF1E", "\xFF3B" => "\xFF3D", "\xFF5B" => "\xFF5D",
    "\xFF5F" => "\xFF60", "\xFF62" => "\xFF63",
};

# assumes whitespace is eaten already

method findbrack {
    my $start;
    my $stop;
    if m:p/ <?before <isPe>> / {
        panic("Use a closing delimiter for an opener is reserved");
    }
    elsif m:p/ <?before $start := [ (<isPs>|<isMirrored>) $0* ] > / {
        $stop = %open2close{$0} err
            panic("Don't know how to flip $start bracket");
        $stop x= $start.chars;
        $+delim = [$start,$stop];
        return $start, $stop;
    }
    else {
        $+delim = substr($_,0,1);
        return ();
    }
}

regex bracketed ($lang = qlang("Q")) {
     <?{ ($<start>,$<stop>) = $.findbrack() }>
     $<q> := <q_balanced($lang, $<start>, $<stop>)>
    {*}
}

regex q_pickdelim ($lang) {
    [
    || <?{ ($<start>,$<stop>) = $.findbrack() }>
       $<q> := <q_balanced($lang, $<start>, $<stop>)>
    || [ $<stop> := [\S] || <panic: Quote delimiter must not be whitespace> ]
       $<q> := <q_unbalanced($lang, $<stop>)>
    ]
    {*}
}

regex rx_pickdelim ($lang) {
    [
    | <?{ ($<start>,$<stop>) = $.findbrack() }>
      $<start>
      $<r> := <regex($<stop>)>        # counts its own brackets, we hope
    | [ $<stop> := [\S] || <panic: Quote delimiter must not be whitespace> ]
      $<r> := <regex($<stop>)>
    ]
    {*}
}

regex q_balanced ($lang, $start, $stop, :@esc = $lang<escset>) {
    $<start> := <$start>
    $<text> := [.*?]
    @<more> := [
        <!before <$stop>>
        [ # XXX triple rule should just be in escapes to be customizable
        | <?before <$start>**{3}>
            $<dequote> := <q_dequote($lang, $start, $stop, :@esc)>
        | <?before <$start>>
            $<subtext> := <q_balanced($lang, $start, $stop, :@esc)>
        | <?before @esc>
            $<escape> := [ <q_escape($lang)> ]
        ]
        $<text> := [.*?]
    ]*
    $<stop> := <$stop>
    {*}
}

regex q_unbalanced ($lang, $stop, :@esc = $lang<escset>) {
    $<text> := [.*?]
    @<more> := [
      <!before <$stop>>
      [ <?before @esc> $<escape> := [ <q_escape($lang)> ]
      $<text> := [.*?]
    ]*
    $<stop> := <$stop>
    {*}
}

# We get here only for escapes in escape set, even though more are defined.
regex q_escape ($lang) {
    <$($lang<escrule>)>
    {*}
}

token quote_escapes {
    [
    || \\ <qq_backslash>
    || <?before \{> <block>
    || <?before \$> <variable> <extrapost>?
    || <variable> <extrapost>
    || .
    ]
    {*}
}

# Note, backtracks!  So expect_postfix mustn't commit to anything permanent.
regex extrapost ($inquote is context = 1) {
    <expect_postfix>*
    <?after <[ \] \} \> \) ]> > 
    {*}
}

rule multisig {
    :?\( <signature> \) [ \| :?\( <signature> \) ]*
}

rule routine_def {
    | <ident>?  <multisig>?
    <trait>*
    <block>
    {*}
}

rule method_def {
    [
    | <ident>  <multisig>?
    | <?before <sigil> \. <[ [ { ( ]> > <sigiltwigil> <postcircumfix>
    ]
    <trait>*
    <block>
    {*}
}

rule regex_def {
    <ident>?
    <trait>*
    [ :?\( <signature> \)]?
    <regex_block>
    {*}
}

rule trait { <trait_verb> | <trait_auxiliary> }

rule trait_auxiliary { :<is>   <ident><postcircumfix>?
rule trait_auxiliary { :<will> <ident> <block> }

rule trait_verb { :<of>        <type> }
rule trait_verb { :<returns>   <type> }
rule trait_verb { :<handles>   <EXPR> }

token capterm {
    \\ \( <capture> \)
    {*}
}

rule capture {
    <EXPR>
    {*}
}

token sigterm {
    \: \( <signature> \)
    {*}
}

rule signature (:$zone is context is rw = 'posreq') {
    [<parameter> [ [ \, | \: | ; | ;; ] <parameter> ]* ]?
    [ --\> <type> ]?
    {*}
}

rule type_declarator {
    :<subset>
    <name>
    [ of <type_name> ]?
    where <subset>
    {*}
}

rule type_constraint {
    [
    | <value>
    | <type_name>
    | where <subset>
    ]
    {*}
}

token parameter {

    <type_constraint>*
    [
    | $<slurp> := [ $<quantchar>:=[ \* ] <sigiltwigil> <ident>?  ]
        { let $<quant> := '*' }
    |   [ $<named> :=
            [ $<quantchar>:=[ \: ]
                [
                | $<name>:=<?ident> \( <sigiltwigil> <ident>?  \)
                |             <sigiltwigil> $<name>:=<ident>
                ]
                { let $<quant> := '*' }
            ]
        | <sigiltwigil>  <ident>?
            { let $<quant> := '!'; }
        ]
        [ $<quantchar> := <[ ? ! ]> { let $<quant> := $<quantchar> } ]?
    ]

    <trait>*

    [
        <default_value> {
            given $<quantchar> {
              when '!' { panic("Can't put a default on a required parameter") }
              when '*' { panic("Can't put a default on a slurpy parameter") }
            }
            let $<quant> := '?';
        }
    ]?

    # enforce zone constraints
    {
        given $<quant> {
            when '!' {
                given $+zone {
                    when 'posopt' {
panic("Can't use required parameter in optional zone");
                    }
                    when 'var' {
panic("Can't use required parameter in variadic zone");
                    }
                }
            }
            when '?' {
                given $+zone {
                    when 'posreq' { $+zone = 'posopt' }
                    when 'var' {
panic("Can't use optional positional parameter in variadic zone");
                    }
                }
            }
            when '*' {
                $+zone = 'var';
            }
        }
    }
    {*}
}

rule default_value {
    \= <EXPR(%item_assignment)>
}

rule statement_prefix { :<do>      <statement> {*} }            #= do
rule statement_prefix { :<try>     <statement> {*} }            #= try
rule statement_prefix { :<gather>  <statement> {*} }            #= gather
rule statement_prefix { :<contend> <statement> {*} }            #= contend
rule statement_prefix { :<async>   <statement> {*} }            #= async
rule statement_prefix { :<lazy>    <statement> {*} }            #= lazy

## term
token term ( --> Term)                                          #+ *
    { :<*> {*} }                                                #= *

token circumfix ( --> Term)                                     #+ $( )
    { <symbol <sigil>> :<(> <EXPR> :<)> {*} }       #= $( ) 

token circumfix ( --> Term)                                     #+ Type( )
    { <symbol <typename> > :<(> <EXPR> :<)> {*} }               #= Type( ) 

token circumfix ( --> Term)                                     #+ ( )
    { :<(> <EXPR> :<)> {*} }                                    #= ( )

token postcircumfix ( --> Term)                                 #+ ( )
    is abstract('call')
    { :<(> <EXPR> :<)> {*} }                                    #= ( )

## autoincrement
token postfix ( --> Autoincrement)                              #+ ++
    { :<++> {*} }                                               #= ++

token postfix ( --> Autoincrement)                              #+ --
    { :<--> {*} }                                               #= --

token prefix ( --> Autoincrement)                               #+ ++
    { :<++> {*} }                                               #= ++

token prefix ( --> Autoincrement)                               #+ --
    { :<--> {*} }                                               #= --


## exponentiation
token infix ( --> Exponentiate)                                 #+ **
    { :<**> {*} }                                               #= **

## symbolic unary
token prefix ( --> Symbolic_unary)                              #+ !
    { :<!> {*} }                                                #= !

token prefix ( --> Symbolic_unary)                              #+ +
    { :<+> {*} }                                                #= +

token prefix ( --> Symbolic_unary)                              #+ -
    { :<-> {*} }                                                #= -

token prefix ( --> Symbolic_unary)                              #+ ~
    { :<~> {*} }                                                #= ~

token prefix ( --> Symbolic_unary)                              #+ ?
    { :<?> {*} }                                                #= ?

token prefix ( --> Symbolic_unary)                              #+ =
    { :<=> {*} }                                                #= =

token prefix ( --> Symbolic_unary)                              #+ *
    { :<*> {*} }                                                #= *

token prefix ( --> Symbolic_unary)                              #+ **
    { :<**> {*} }                                               #= **

token prefix ( --> Symbolic_unary)                              #+ ~^
    { :<~^> {*} }                                               #= ~^

token prefix ( --> Symbolic_unary)                              #+ +^
    { :<+^> {*} }                                               #= +^

token prefix ( --> Symbolic_unary)                              #+ ?^
    { :<?^> {*} }                                               #= ?^

token prefix ( --> Symbolic_unary)                              #+ ^
    { :<^> {*} }                                                #= ^

token prefix ( --> Symbolic_unary)                              #+ |
    { :<|> {*} }                                                #= |


## multiplicative
token infix ( --> Multiplicative)                               #+ *
    { :<*> {*} }                                                #= *

token infix ( --> Multiplicative)                               #+ /
    { :</> {*} }                                                #= /

token infix ( --> Multiplicative)                               #+ %
    { :<%> {*} }                                                #= %

token infix ( --> Multiplicative)                               #+ x
    { :<x> {*} }                                                #= x

token infix ( --> Multiplicative)                               #+ xx
    { :<xx> {*} }                                               #= xx

token infix ( --> Multiplicative)                               #+ +&
    { :<+&> {*} }                                               #= +&

token infix ( --> Multiplicative)                               #+ +<
    { :['+<'] {*} }                                             #= +<

token infix ( --> Multiplicative)                               #+ +>
    { :['+>'] {*} }                                             #= +>

token infix ( --> Multiplicative)                               #+ ~&
    { :<~&> {*} }                                               #= ~&

token infix ( --> Multiplicative)                               #+ ~<
    { :['~<'] {*} }                                             #= ~<

token infix ( --> Multiplicative)                               #+ ~>
    { :['~>'] {*} }                                             #= ~>


## additive
token infix ( --> Additive)                                     #+ +
    { :<+> {*} }                                                #= +

token infix ( --> Additive)                                     #+ -
    { :<-> {*} }                                                #= -

token infix ( --> Additive)                                     #+ ~
    { :<~> {*} }                                                #= ~

token infix ( --> Additive)                                     #+ +|
    { :<+|> {*} }                                               #= +|

token infix ( --> Additive)                                     #+ +^
    { :<+^> {*} }                                               #= +^

token infix ( --> Additive)                                     #+ ~|
    { :<~|> {*} }                                               #= ~|

token infix ( --> Additive)                                     #+ ~^
    { :<~^> {*} }                                               #= ~^

token infix ( --> Additive)                                     #+ ?|
    { :<?|> {*} }                                               #= ?|

token infix ( --> Additive)                                     #+ ?^
    { :<?^> {*} }                                               #= ?^


## junctive and (all)
token infix ( --> Junctive_and)                                 #+ &
    { :<&> {*} }                                                #= &


## junctive or (any)
token infix ( --> Junctive_or)                                  #+ |
    { :<|> {*} }                                                #= |

token infix ( --> Junctive_or)                                  #+ ^
    { :<^> {*} }                                                #= ^


## named unary examples
token prefix ( --> Named_unary)                                 #+ rand
    { :<rand> {*} }                                             #= rand

token prefix ( --> Named_unary)                                 #+ sleep
    { :<sleep> {*} }                                            #= sleep

token prefix ( --> Named_unary)                                 #+ abs
    { :<abs> {*} }                                              #= abs

## nonchaining binary
token infix ( --> Nonchaining)                                  #+ <=>
    { :['<=>'] {*} }                                            #= <=>

token infix ( --> Nonchaining)                                  #+ cmp
    { :<cmp> {*} }                                              #= cmp

token infix ( --> Nonchaining)                                  #+ is
    { :<is> {*} }                                               #= is

token infix ( --> Nonchaining)                                  #+ but
    { :<but> {*} }                                              #= but

token infix ( --> Nonchaining)                                  #+ does
    { :<does> {*} }                                             #= does

token infix ( --> Nonchaining)                                  #+ ..
    { :<..> {*} }                                               #= ..

token infix ( --> Nonchaining)                                  #+ ^..
    { :<^..> {*} }                                              #= ^..

token infix ( --> Nonchaining)                                  #+ ..^
    { :<..^> {*} }                                              #= ..^

token infix ( --> Nonchaining)                                  #+ ^..^
    { :<^..^> {*} }                                             #= ^..^

token infix ( --> Nonchaining)                                  #+ ff
    { :<ff> {*} }                                               #= ff

token infix ( --> Nonchaining)                                  #+ ^ff
    { :<^ff> {*} }                                              #= ^ff

token infix ( --> Nonchaining)                                  #+ ff^
    { :<ff^> {*} }                                              #= ff^

token infix ( --> Nonchaining)                                  #+ ^ff^
    { :<^ff^> {*} }                                             #= ^ff^

token infix ( --> Nonchaining)                                  #+ fff
    { :<fff> {*} }                                              #= fff

token infix ( --> Nonchaining)                                  #+ ^fff
    { :<^fff> {*} }                                             #= ^fff

token infix ( --> Nonchaining)                                  #+ fff^
    { :<fff^> {*} }                                             #= fff^

token infix ( --> Nonchaining)                                  #+ ^fff^
    { :<^fff^> {*} }                                            #= ^fff^


## chaining binary
token infix ( --> Chaining)                                     #+ ==
    { :<==> {*} }                                               #= ==

token infix ( --> Chaining)                                     #+ !=
    { :<!=> {*} }                                               #= !=

token infix ( --> Chaining)                                     #+ <
    { :['<'] {*} }                                              #= <

token infix ( --> Chaining)                                     #+ <=
    { :['<='] {*} }                                             #= <=

token infix ( --> Chaining)                                     #+ >
    { :['>'] {*} }                                              #= >

token infix ( --> Chaining)                                     #+ >=
    { :['>='] {*} }                                             #= >=

token infix ( --> Chaining)                                     #+ ~~
    { :<~~> {*} }                                               #= ~~

token infix ( --> Chaining)                                     #+ !~
    { :<!~> {*} }                                               #= !~

token infix ( --> Chaining)                                     #+ =~
    { :<=~> {*} }                                               #= =~

token infix ( --> Chaining)                                     #+ eq
    { :<eq> {*} }                                               #= eq

token infix ( --> Chaining)                                     #+ ne
    { :<ne> {*} }                                               #= ne

token infix ( --> Chaining)                                     #+ lt
    { :<lt> {*} }                                               #= lt

token infix ( --> Chaining)                                     #+ le
    { :<le> {*} }                                               #= le

token infix ( --> Chaining)                                     #+ gt
    { :<gt> {*} }                                               #= gt

token infix ( --> Chaining)                                     #+ ge
    { :<ge> {*} }                                               #= ge

token infix ( --> Chaining)                                     #+ =:=
    { :<=:=> {*} }                                              #= =:=

token infix ( --> Chaining)                                     #+ ===
    { :<===> {*} }                                              #= ===


## tight and
token infix ( --> Tight_and)                                    #+ &&
    { :<&&> {*} }                                               #= &&


## tight or
token infix ( --> Tight_or)                                     #+ ||
    { :<||> {*} }                                               #= ||

token infix ( --> Tight_or)  {                                  #+ ^^
    :<^^>
    { $<assoc> := 'list' }  # override Tight_or's 'left' associativity
    {*}                                                         #= ^^
}

token infix ( --> Tight_or)                                     #+ //
    { :<//> {*} }                                               #= //


## conditional
token infix ( --> Conditional)                                  #+ ?? !!
    { :<??> <EXPR(%conditional)> :<!!> {*} }                    #= ?? !!


## assignment
token infix ( --> Assignment)                                   #+ =
    is abstract('assign')
    is lvalue(1)
    { :<=> {*} }                                                #= =

token infix ( --> Assignment)                                   #+ :=
    is abstract('bind')
    { :<:=> {*} }                                               #= :=

token infix ( --> Assignment)                                   #+ ::=
    { :<::=> {*} }                                              #= ::=

# XXX need to do something to turn subcall into method call here...
token infix ( --> Assignment)                                   #+ .=
    { :<.=> {*} }                                               #= .=

# Note, other assignment ops generated by infix_postfix_meta_operator rule

## list item separator
token infix ( --> Comma)                                        #+ ,
    { :<,> {*} }                                                #= ,

## loose unary
token prefix ( --> Loose_unary)                                 #+ true
    { :<true> {*} }                                             #= true

token prefix ( --> Loose_unary)                                 #+ not
    { :<not> {*} }                                              #= not

## list prefix (really sub calls mostly defined in Prelude)
token prefix ( --> List_prefix)                                 #+ print
    { <symbol <listop> > \s <nofat>
        <EXPR(%list_prefix)>              {*}                   #= print
    }

# unrecognized identifiers are assumed to be post-declared subs.
token prefix ( --> List_prefix)                                 #+ print
    { <symbol <ident>> \s <nofat>
        <EXPR(%list_prefix)>              {*}                   #= print
    }

token prefix ( --> List_prefix)                                 #+ $:
    { <symbol <sigil> \: > \s
        <EXPR(%list_prefix)>              {*}                   #= $:
    }

token prefix ( --> List_prefix)                                 #+ Type:
    { <symbol <typename> \: > \s
        <EXPR(%list_prefix)>              {*}                   #= Type: 
    }

## loose and
token infix ( --> Loose_and)                                    #+ and
    is abstract('if')
    { :<and> {*} }                                              #= and

## loose or
token infix ( --> Loose_or)                                     #+ or
    is abstract('unless')
    { :<or> {*} }                                               #= or

token infix ( --> Loose_or)                                     #+ xor
    is abstract('xor')
    { :<xor> {*} }                                              #= xor

token infix ( --> Loose_or)                                     #+ err
    { :<err> {*} }                                              #= err

## expression terminator

# XXX correct to eat semicolon here?
token terminator ( --> Terminator)                              #+ ;
    { :<;> {*} }                                                #= ;

token terminator ( --> Terminator)                              #+ <==
    { <?before :['<=='] > {*} }                                 #= <==

token terminator ( --> Terminator)                              #+ ==>
    { <?before :['==>'] > {*} }              #'                 #= ==>

token terminator ( --> Terminator)                              #+ -->
    { <?before :['-->'] > {*} }              #'                 #= -->

token terminator ( --> Terminator)                              #+ )
    { <?before :<)> > {*} }                                     #= )

token terminator ( --> Terminator)                              #+ ]
    { <?before :<]> > {*} }                                     #= ]

token terminator ( --> Terminator)                              #+ }
    {[ <?before :<}> > {*} ]}                                   #= }

token terminator ( --> Terminator)                              #+ !!
    { <?before :<!!> > {*} }                                    #= !!

regex stdstopper {
    | <terminator>
    | <statement_mod_cond>
    | <statement_mod_loop>
    | <$+unitstopper>
    | $
}

token assertstopper { <stdstopper> | \> }

# A fairly complete (but almost certainly buggy) operator precedence parser

method EXPR (%preclim = %LOOSEST, :$stop = &stdstopper) {
    my $preclim = %preclim<prec>;
    my $inquote is context = 0;
    if m:p/ <?before <$stop>> / {
        return;
    }
    my $prevop is context is rw;
    my @termstack;
    my @opstack;
    push @opstack, %terminator;         # (just a sentinel value)
    push @termstack, $.expect_term();

    my sub reduce () {
        my $op = pop @opstack;
        given $op<assoc> {
            when 'chain' {
                my @chain;
                push @chain, pop(@termstack);
                push @chain, $op;
                while @opstack {
                    last if $op.prec ne @opstack[-1].prec;
                    push @chain, pop(@termstack);
                    push @chain, pop(@opstack)<infix>;
                }
                push @chain, pop(@termstack);
                $op<infix><chain> = reverse @chain;
                push @termstack, $op<infix>;
            }
            when 'list' {
                my @list;
                push @list, pop(@termstack);
                while @opstack {
                    last if $op<symbol> ne @opstack[-1]<symbol>;
                    push @list, pop(@termstack);
                    pop(@opstack);
                }
                push @list, pop(@termstack);
                $op<infix><list> = reverse @list;
                push @termstack, $op<infix>;
            }
            default {
                $op<infix><right> = pop @termstack;
                $op<infix><left> = pop @termstack;
                push @termstack, $op<infix>;
            }
        }
    }

    while not m:p/ <?before <$stop> > / {
        my $thisop is context is rw;
        my $infix := $.expect_tight_infix($preclim);
        my Str $newprec = $thisop.prec;
        $thisop //= %terminator;
        $thisop<infix> = $infix;

        # Does new infix (or terminator) force any reductions?
        while @opstack[-1].prec lt $newprec {
            reduce();
        }

        # Not much point in reducing the sentinels...
        last if $newprec lt $LOOSEST;

        # Equal precedence, so use associativity to decide.
        if @opstack[-1].prec eq $newprec {
            given $thisop<assoc> {
                when 'non'   { panic(qq["$infix" is not associative]) }
                when 'left'  { reduce() }   # reduce immediately
                when 'right' | 'chain' { }  # just shift
                when 'list'  {              # if op differs reduce else shift
                    reduce() if $thisop<symbol> !eqv @opstack[-1]<symbol>;
                }
                default { panic(qq[Unknown associativity "$_" for "$infix"]) }
            }
        }
        push @opstack, $thisop;
        if m:p/ <?before <$stop>> / {
            fail("$infix.perl() is missing right term");
        }
        push @termstack, $.expect_term();
    }
    reduce() if @termstack > 1;
    @termstack == 1 or panic("Internal operator parser error");
    return @termstack[0];
}

#############################################3333
## Regex
#############################################3333

rule regex ($stop is context) {
    <regex_ordered_disjunction>
    {*}
}

rule regex_ordered_disjunction {
    <'||'>?
    <regex_ordered_conjunction>
    [ :<||> <regex_ordered_conjunction> ]*
    {*}
}

rule regex_ordered_conjunction {
    <regex_unordered_disjunction>
    [ :<&&> <regex_unordered_disjunction> ]*
    {*}
}

rule regex_unordered_disjunction {
    <'|'>?
    <regex_unordered_conjunction>
    [ :<|> <regex_unordered_conjunction> ]*
    {*}
}

rule regex_unordered_conjunction {
    <regex_sequence>
    [ :<&> <regex_sequence> ]*
    {*}
}

rule regex_sequence {
    <regex_quantified_atom>+
    # Could combine unquantified atoms into one here...
    {*}
}

rule regex_quantified_atom {
    <regex_atom>
    [ <regex_quantifier>
        <?{ $<regex_atom>.max_width }>
            || <panic: "Can't quantify zero-width atom")
    ]?
    {*}
}

rule regex_atom {
    [
    || <$+stop> :: <fail>
    || <regex_metachar>
    || (.)
    ]
    {*}
}

# sequence stoppers
token regex_metachar { :['>'] :: <fail> }
token regex_metachar { :<&&>  :: <fail> }
token regex_metachar { :<&>   :: <fail> }
token regex_metachar { :<||>  :: <fail> }
token regex_metachar { :<|>   :: <fail> }
token regex_metachar { :<]>   :: <fail> }
token regex_metachar { :<)>   :: <fail> }
token regex_metachar is symbol<}>
                     { \}     :: <fail> }

# "normal" metachars
token regex_metachar { <block> }
token regex_metachar { <quantifier> <panic: quantifier quantifies nothing> }
token regex_metachar { <regex_mod_internal> }
token regex_metachar { :<[> <regex \]> :<]> }
token regex_metachar { :<(> <regex \)> :<)> }
token regex_metachar { :<\>> <regex_rightangle> }
token regex_metachar { :['<('] }
token regex_metachar { :[')>'] }
token regex_metachar { :['<<'] }
token regex_metachar { :['>>'] }
token regex_metachar { :['<'] <regex_assertion> :['>'] }
token regex_metachar { :<\\> <regex_backslash> }
token regex_metachar { :<.> }
token regex_metachar { :<^^> }
token regex_metachar { :<^> }
token regex_metachar { :<$$> }
token regex_metachar {
    | :<$> <before $
                 | \s
                 | \|
                 | \)
                 | \]
                 | \>
           >
    | <variable>
}

token codepoint {
    \[ (.*?) \]
}

token q_backslash { :<qq> <qq_bracketed> }
token q_backslash { :<\> }
token q_backslash { (.) }

token qq_backslash { :<a> }
token qq_backslash { :<b> }
token qq_backslash { :<c> <codepoint> }
token qq_backslash { :<e> }
token qq_backslash { :<f> }
token qq_backslash { :<n> }
token qq_backslash { :<o> [ <octnum> | \[<octnum>[,<octnum>]*\] ] }
token qq_backslash { :<r> }
token qq_backslash { :<t> }
token qq_backslash { :<x> [ <hexnum> | \[<hexnum>[,<hexnum>]*\] ] }
token qq_backslash { :: \W || <panic: unrecognized backslash sequence> }

token regex_backslash { :i :<a> }
token regex_backslash { :i :<b> }
token regex_backslash { :i :<c> <codepoint> }
token regex_backslash { :i :<d> }
token regex_backslash { :i :<e> }
token regex_backslash { :i :<f> }
token regex_backslash { :i :<h> }
token regex_backslash { :i :<n> }
token regex_backslash { :i :<o> [ <octnum> | \[<octnum>[,<octnum>]*\] ] }
token regex_backslash { :i :<r> }
token regex_backslash { :i :<t> }
token regex_backslash { :i :<v> }
token regex_backslash { :i :<w> }
token regex_backslash { :i :<x> [ <hexnum> | \[<hexnum>[,<hexnum>]*\] ] }
token regex_backslash { :: <panic: unrecognized regex backslash sequence> }

token regex_assertion { :<?> <regex_assertion> }
token regex_assertion { :<!> <regex_assertion> }

token regex_assertion { <block> }
token regex_assertion { <variable> }
token regex_assertion { <ident> [               # is qq right here?
                                | \: <?ws>
                                    <q_unbalanced(%sublang<qq><esc>, :stop«>»))>
                                | \( <EXPR> \)
                                | <?ws> <EXPR>
                                ]?
}

token regex_assertion { <before :<[>> <cclass_elem>+ }
token regex_assertion { <before :<+>> <cclass_elem>+ }
token regex_assertion { <before :<->> <cclass_elem>+ }

token regex_assertion { <panic: unrecognized regex assertion> }

token cclass_elem {
    [ \+ | - | <null> ]
    [
    | <name>
    | <before \[> <bracketed(QLang('cclass'))>
    ]
}

token regex_mod_internal { <colonpair> }
token regex_mod_internal { :<:i> <regex_mod_arg>? }
token regex_mod_internal { :<:!i> }
token regex_mod_internal { :<:[> <EXPR> :<]> }
token regex_mod_internal { :<:\<> <EXPR> :<\>> }
token regex_mod_internal { <panic: unrecognized regex modifier> }

token regex_mod_external { <colonpair> }
token regex_mod_external { :<:g> <regex_mod_arg> }
token regex_mod_external { :<:global> <regex_mod_arg> }
token regex_mod_external { :<:s> <regex_mod_arg> }
token regex_mod_external { :<:sigspace> <regex_mod_arg> }
token regex_mod_external { :<:nth> <regex_mod_arg> }
token regex_mod_external { \:\d+ ( x | st | nd | rd | th ) }
token regex_mod_external { <panic: unrecognized regex modifier> }

token regex_quantifier { :<**> <?ws> <block> <quantmod> }
token regex_quantifier { :<*> <quantmod> }
token regex_quantifier { :<+> <quantmod> }

token quantmod { [ \? | \! | \: | \+ ]? }

# The <panic: message> rule is called for syntax errors.
# If there are any <suppose> points, backtrack and retry parse
# with a different supposition.  If it gets farther than the
# panic point, print out the supposition ("Looks like you
# used a Perl5-style shift operator (<<) at line 42.  Maybe
# you wanted +< or |< instead.")  Or some such...
# In any event, this is only for better diagnostics, and
# further compilation is suppressed by the <commit><fail>.

rule panic (Str $s) { <commit> <fail($s)> }

## vim: expandtab sw=4
