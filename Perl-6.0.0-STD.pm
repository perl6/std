grammar Perl:ver<6.0.0.alpha>:auth<http://perl.org>;

has StrPos $.ws_from;
has StrPos $.ws_to;

=begin things todo

    add more suppositions and figure out exact error continuation semantics
    finish out all the {*} #= hookage
    think about longest-token-defeating {*} that maybe should be <?{ {*}; 1}>
    add parsing this file to sanity tests :)

=end things todo

=begin comment overview

This file is designed to be either preprocessed into a grammar with
action statements or used as-is without any preprocessing.  The {*}
notation is a no-op action block, but can be identified uniquely via a
combination of the preceding token or rule name plus any additional text
following a #= comment.  We put this into a comment rather than using
a macro so that bootstrap compilers don't have to worry about macros
yet, and to keep the main grammar relatively uncluttered by action
statements.  Note that the preprocessor can certainly generate accesses
to $/ within the action block, so we need not mention it explicitly.
Also, some rules are named by syntactic category plus an additonal symbol
specified in adverbial form, either in bare :name form or in :sym<name>
form.  (It does not matter which form you use for identifier symbols,
except that to specify a symbol "sym" you must use the :sym<sym> form
of adverb.)  If you use the <sym> rule within the rule, it will parse the
symbol at that point.  At the final reduction point of a rule, if $<sym>
has been set, that is used as the final symbol name for the rule.  This
need not match the symbol specified as part the rule name; that is just
for disambiguating the name.  However, if no $<sym> is set, the original
symbol will be used by default.

Note that rules with only one action need no #= comment, so the identifier
of the following stub is just "TOP".

Another nod toward preprocessing is that blocks that contain nested braces
are delimited by double braces so that the preprocessor does not need to
understand Perl 6 code.

=end comment overview

token TOP { <UNIT( $+unitstopper or "_EOS" )> {*} }

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
                token infix:plus returns Additive { <sym> }
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
# The current values are mere implementation; they may change at any time.
# Users should specify precedence only in relation to existing levels.

constant %term              = { :prec<z=>                           };
constant %methodcall        = { :prec<w=>                           };
constant %autoincrement     = { :prec<v=>, :lvalue                  };
constant %exponentiation    = { :prec<u=>, :assoc<right>, :assign   };
constant %symbolic_unary    = { :prec<t=>                           };
constant %multiplicative    = { :prec<s=>, :assoc<left>,  :assign   };
constant %additive          = { :prec<r=>, :assoc<left>,  :assign   };
constant %junctive_and      = { :prec<q=>, :assoc<list>,  :assign   };
constant %junctive_or       = { :prec<p=>, :assoc<list>,  :assign   };
constant %named_unary       = { :prec<o=>,                          };
constant %nonchaining       = { :prec<n=>, :assoc<non>              };
constant %chaining          = { :prec<m=>, :assoc<chain>, :bool     };
constant %tight_and         = { :prec<l=>, :assoc<left>,  :assign   };
constant %tight_or          = { :prec<k=>, :assoc<left>,  :assign   };
constant %conditional       = { :prec<j=>, :assoc<right>,           };
constant %item_assignment   = { :prec<i=>, :assoc<right>, :lvalue   };
constant %loose_unary       = { :prec<h=>,                          };
constant %comma             = { :prec<g=>, :assoc<list>,            };
constant %list_infix        = { :prec<f=>, :assoc<list>,            };
constant %list_prefix       = { :prec<e=>,                          };
constant %loose_and         = { :prec<d=>, :assoc<left>,            };
constant %loose_or          = { :prec<c=>, :assoc<left>,            };
constant %LOOSEST           = { :prec<a=!>,                         };
constant %terminator        = { :prec<a=>, :assoc<list>             };

# "epsilon" tighter than terminator
#constant $LOOSEST = %LOOSEST<prec>;
constant $LOOSEST = "a=!"; # XXX preceding line is busted

role PrecOp[*%defaults] {

    # This is hopefully called on a match to mix in operator info by type.
    method &.(Match $m) {
#        $m but= ::?CLASS;
        for %defaults.kv -> $k, $v { $m{$k} //= $v };
        %+thisop<top> = $m;
        if not $m<transparent> {
            %+thisop<prec> = $m<prec>;
            %+thisop<assoc> = $m<assoc>;
        }
        return $m;
    }
}

class Hyper           does PrecOp[:transparent]             {}

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
# by merely adding more rules in the same category.  The rules within
# a given category start with the category name followed by a differentiating
# adverbial qualifier to serve (along with the category) as the longer name.

# The endsym context, if specified, says what to implicitly check for in each
# rule right after the initial <sym>.  Normally this is used to make sure
# there's appropriate whitespace, though Perl 6 also uses it to rule out
# the => (fatarrow) construct.  Note that endsym isn't called if <sym>
# isn't called.

my $endsym is context = "null";
my $unitstopper is context = "_EOS";
my $endstmt is context = -1;
my $endargs is context = -1;

# XXX the only magic we're assuming here is that someone set up $+sym for us.
#  (well, and endsym, but that's set explicitly in the proto sigs below,
#  (which theoretically propagate to the sigs of the multis they control...))
token sym (Str $pat = $+sym) {
    $pat <$+endsym>
}

#multi method sym ($¢, $pat = $+sym) {
#    m:p/ <$pat> <$+endsym> /;
#}

proto token category { }

token category:category { <sym> }

token category:sigil { <sym> }
proto token sigil { }

token category:twigil { <sym> }
proto token twigil { }

token category:special_variable { <sym> }
proto token special_variable { }

token category:version { <sym> }
proto token version { }

token category:term { <sym> }
proto token term { }

token category:quote { <sym> }
proto token quote { }

token category:prefix { <sym> }
proto token prefix is defequiv(%symbolic_unary) { }

token category:infix { <sym> }
proto token infix is defequiv(%additive) { }

token category:postfix { <sym> }
proto token postfix is defequiv(%autoincrement) { }

token category:dotty { <sym> }
proto token dotty (:$endsym is context = / <?unsp>? /) { }

token category:circumfix { <sym> }
proto token circumfix { }

token category:postcircumfix { <sym> }
proto token postcircumfix { }

token category:regex_metachar { <sym> }
proto token regex_metachar { }

token category:regex_backslash { <sym> }
proto token regex_backslash { }

token category:regex_assertion { <sym> }
proto token regex_assertion { }

token category:regex_mod_internal { <sym> }
proto token regex_mod_internal { }

#token category:regex_mod_external { <sym> }
#proto token regex_mod_external
#    (:$endsym is context = / <?before '('> <postcircumfix> /) { }

token category:quote_mod { <sym> }
proto token quote_mod { }

token category:q_backslash { <sym> }
proto token q_backslash { }

token category:qq_backslash { <sym> }
proto token qq_backslash { }

token category:trait_verb { <sym> }
proto token trait_verb (:$endsym is context = / \s+ <nofat> /) { }

token category:trait_auxiliary { <sym> }
proto token trait_auxiliary (:$endsym is context = / \s+ <nofat> /) { }

token category:type_declarator { <sym> }
proto token type_declarator (:$endsym is context = / >> <nofat> /) { }

token category:scope_declarator { <sym> }
proto token scope_declarator (:$endsym is context = / >> <nofat> /) { }

token category:package_declarator { <sym> }
proto token package_declarator (:$endsym is context = / >> <nofat> /) { }

token category:routine_declarator { <sym> }
proto token routine_declarator (:$endsym is context = / >> <nofat> /) { }

token category:statement_prefix { <sym> }
proto rule  statement_prefix (:$endsym is context = / >> <nofat> /) { }

token category:statement_control { <sym> }
proto rule  statement_control (:$endsym is context = / \s <nofat> /) { }

token category:statement_mod_cond { <sym> }
proto rule  statement_mod_cond (:$endsym is context = / >> <nofat> /) { }

token category:statement_mod_loop { <sym> }
proto rule  statement_mod_loop (:$endsym is context = / >> <nofat> /) { }

token category:infix_prefix_meta_operator { <sym> }
proto token infix_prefix_meta_operator { }

token category:infix_postfix_meta_operator { <sym> }
proto token infix_postfix_meta_operator { }

token category:infix_circumfixfix_meta_operator { <sym> }
proto token infix_circumfixfix_meta_operator { }

token category:postfix_prefix_meta_operator { <sym> }
proto token postfix_prefix_meta_operator { }

token category:prefix_postfix_meta_operator { <sym> }
proto token prefix_postfix_meta_operator { }

token category:prefix_circumfix_meta_operator { <sym> }
proto token prefix_circumfix_meta_operator { }

# Lexical routines

# make sure we're not an autoquoted identifier
regex nofat { <!before \h* <?unsp>? '=>' > }

token ws {
    || <?{ $¢ === $!ws_to }> <null>
    || <?after \w> <?before \w> ::: <fail>        # must \s+ between words
    || { $!ws_from = $¢.to }
       [
       | <unsp>              {*}                                #= unsp
       | \v                  {*} <heredoc>                      #= vwhite
       | <unv>               {*}                                #= unv
       ]*  {*}                                                  #= all
       { $!ws_to = $¢.to }
}

token unsp {
    \\ <?before [\s|'#']>
    [
    | \v                     {*}                                #= vwhite
    | <unv>                  {*}                                #= unv
    ]*  {*}                                                     #= all
}

token unv {
   | \h+                 {*}                                    #= hwhite
   | ^^ [ <?block_comment> | <?pod_comment> ]  {*}              #= multiline
   | '#' [
        # assuming <bracketed> defaults to standard set
        | <?bracketed>   {*}                                    #= embedded
        | \N*            {*}                                    #= end
        ]
}

=begin perlhints #{ #}
token:  block_comment
syn:    #{ <arbitrary text> }
name:   block comment
desc:   #{ starts a comment that is terminated by #}. Inside the comment \
        brackets may be nested.
ex:     say 
        #{ 
        this is a comment
        #} 
        "something"; 
=end perlhints

token block_comment {
    ^^ '#' <?{ ($<start>,$<stop>) = self.peek_brackets($¢) }>
    $<start> \N* \n                        # eat the #{ line
    [
    ||  [
        || [ <?block_comment> | <?pod_comment> ] \n # inner blocks must match too
        || \N* \n
        ]*?                                # 0 or more inner lines
        ^^ '#' $<stop> <differ> \N*        # the #} line (leave final \n there)
    || <panic: Unterminated block comment> # (hopefully adds current position)
    ]
    {*}                                                         #= block
}

regex same   { <?after (.)> <?before $0> }
regex differ { <?after (.)> <!before $0> }

token ident {
    <alpha> \w*
}

# XXX We need to parse the pod eventually to support $= variables.

token pod_comment {
    ^^ '=' <?unsp>?
    [
    | begin <?ws> <ident> .*? \n
      '=' <?unsp>? 'end' <?ws> $<ident> \N*         {*}         #= block
    | \N*                                           {*}         #= misc
    ]
    {*}
}

# Top-level rules

method UNIT ($¢, $unitstopper is context = "_EOS") {
    UNIT: do {
        self.compunit($¢);
    }
}

# Note: we only check for the unitstopper.  We don't check for ^ because
# we might be embedded in something else.
rule comp_unit (:$begin_compunit is context = 1,
                :$endstmt        is context<rw> = -1,
                :$endargs        is context<rw> = -1)
{
    <statementlist>
    [ <$+unitstopper> || <panic: Can't understand next input--giving up> ]
    {*}
}

# Note: because of the possibility of placeholders we can't determine arity of
# the block syntactically, so this must be determined via semantic analysis.
# Also, pblocks used in an if/unless statement do not treat $_ as a placeholder,
# while most other blocks treat $_ as equivalent to $^x.  Therefore the first
# possible place to check arity is not here but in the rule that calls this
# rule.  (Could also be done in a later pass.)

token pblock {
    [ <lambda> <signature> ]? <block>
}

=begin perlhints ->
token:  lambda
syn:    -> <signature> { <statements> }
name:   lambda
desc:   -> introduces a (possibly empty) signature to a block
ex:     for @list -> $a { say $a; }
        my &function := -> { say 42; };
=end perlhints

=begin perlhints <->
token:  lambda
syn:    <-> <signature> { <statements> }
name:   lambda rw
desc:   <-> introduces a (possibly empty) signature to a block, applying the \
        'is rw' trait on all arguments
ex:     for @list <-> $a { $a++ }
=end perlhints

token lambda { '->' | '<->' }

=begin perlhints { }
token:  block
syn:    { <statemts> }
name:   block
desc:   { ... } groups statements and introduces a new scope
ex:     for @list -> $a { say $a }
=end perlhints

token block {
    '{'
    <statementlist>
    [ '}' || <panic: Missing right brace> ]
    [
    | \h* <?unsp>? <?before <[,:]>> {*}                         #= normal 
    | <?unv>? <?before \n > <?ws>
        { let $+endstmt = $!ws_from; } {*}                      #= endstmt
    | {*} { let $+endargs = $¢.to; }                               #= endargs
    ]
    {*}
}

=begin perlhints { }
token:  regex_block
syn:    regex { <regex> }
name:   regex block
desc:   delimits a regex, rule or token
ex:     regex word { <alpha>+ }
=end perlhints

token regex_block {  # perhaps parameterize and combine with block someday
    '{'
    <regex '}'>
    [ '}' || <panic: Missing right brace> ]
    [
    | <?unsp>? <?before <[,:]>> {*}                             #= normal
    | <?unv>? <?before \n > <?ws>
        { let $+endstmt = $!ws_from; } {*}                      #= endstmt
    | {*} { let $+endargs = $¢.to; }                               #= endargs
    ]
    {*}
}

# statement semantics
rule statementlist {
    <statement>*
    {*}
}

# embedded semis, context-dependent semantics
rule semilist {
    <statement>*
    {*}
}

=begin perlhints :
token:  label
syn:    <identifier>:
name:   label
desc:   <identifier>: assigns a name to a block or statement
ex:     INNER: 
        for @list { 
            if m/something/ { 
                last INNER; 
            } 
        }
=end perlhints

token label {
    <ident> ':' \s <?ws>

    [ <?{ is_type($<ident>) }>
      <suppose("You tried to use an existing name $/{'ident'} as a label")>
    ]?

    # add label as a pseudo type
    {{ eval 'COMPILING::{"::$<ident>"} = Label.new($<ident>)' }}  # XXX need statement ref too?

    {*}
}

rule statement (StrPos :$endstmt is context<rw> = -1) {
    <label>*                                     {*}            #= label
    [
    | <statement_control>                        {*}            #= control
    | $0:=<expect_term> $<expr>:=<EXPR(:seen($0))>  {*}         #= expr
        [
        || <?before <stdstopper>>
        || <statement_mod_loop> $<loopx>:=<EXPR> {*}            #= mod loop
        || <statement_mod_cond> $<condx>:=<EXPR>
            [
            || <?before <stdstopper>> {*}                       #= mod cond
            || <statement_mod_loop> $<loopx>:=<EXPR> {*}        #= mod condloop
            ]
        ]
        {*}                                                     #= modexpr
    | <?before ';'> {*}                                         #= null
    ]
    <eat_terminator>
    {*}
}

token eat_terminator {
    [
    || ';'
    || <?{ $+endstmt === $!ws_from }>
    || <?before <terminator>>
    || {{ if $¢ === $!ws_to { $¢ = $!ws_from } }}   # undo any line transition
        <panic: Statement not terminated properly>  # "can't happen" anyway :)
    ]
}

rule statement_control:use {
    <sym>
    <module_name> <EXPR>? <eat_terminator>           {*}        #= use
}

rule statement_control:no {
    <sym>
    <module_name> <EXPR>? <eat_terminator>           {*}        #= no
}

rule statement_control:if {
    <sym>
    <EXPR>                           {*}                        #= if expr
    <pblock>                         {*}                        #= if block
    @<elsif> := ( elsif <EXPR>       {*}                        #= elsif expr
                        <pblock>     {*} )*                     #= elsif block
    @<else> := ( else <pblock>       {*} )?                     #= else
    {*}
}

rule statement_control:unless {
    <sym> 
    <EXPR>                           {*}                        #= unless expr
    <pblock>                         {*}                        #= unless block
    {*}
}

rule statement_control:while {
    <sym>
    [ <?before '(' [[my]? '$'\w+ '=']? '<' '$'?\w+ '>' ')'>   #'
        <panic: This appears to be Perl 5 code> ]?
    <EXPR>                             {*}                      #= while expr
    <pblock>                           {*}                      #= while block
    {*}
}

rule statement_control:until {
    <sym>
    <EXPR>                             {*}                      #= until expr
    <pblock>                           {*}                      #= until block
    {*}
}
rule statement_control:repeat {
    <sym>
    [
        | (while|until) <EXPR>         {*}                      #= rep wu expr
          <block>                      {*}                      #= rep wu block
        | <block>                      {*}                      #= rep block wu
          (while|until) <EXPR>         {*}                      #= rep expr wu
    ]
    {*}
}
rule statement_control:loop {
    <sym>
    $<eee> := (
        '('
            $<e1> := <EXPR> ';'   {*}                           #= loop e1
            $<e2> := <EXPR> ';'   {*}                           #= loop e2
            $<e3> := <EXPR>     {*}                             #= loop e3
        ')'                      {*}                            #= loop eee
    )?
    <block>                     {*}                             #= loop block
    {*}
}

rule statement_control:for {
    <sym>
    [ <?before [my]? '$'\w+ '(' >
        <panic: This appears to be Perl 5 code> ]?
    <EXPR>                             {*}                      #= for expr
    <pblock>                           {*}                      #= for block
    {*}
}

rule statement_control:given {
    <sym>
    <EXPR>                             {*}                      #= given expr
    <pblock>                           {*}                      #= given block
    {*}
}
rule statement_control:when {
    <sym>
    <EXPR>                             {*}                      #= when expr
    <pblock>                           {*}                      #= when block
    {*}
}
rule statement_control:default   { <sym> <block> {*} }          #= default

rule statement_control:BEGIN   { <sym> <block> {*} }            #= BEGIN
rule statement_control:CHECK   { <sym> <block> {*} }            #= CHECK
rule statement_control:INIT    { <sym> <block> {*} }            #= INIT
rule statement_control:END     { <sym> <block> {*} }            #= END
rule statement_control:START   { <sym> <block> {*} }            #= START
rule statement_control:ENTER   { <sym> <block> {*} }            #= ENTER
rule statement_control:LEAVE   { <sym> <block> {*} }            #= LEAVE
rule statement_control:KEEP    { <sym> <block> {*} }            #= KEEP
rule statement_control:UNDO    { <sym> <block> {*} }            #= UNDO
rule statement_control:FIRST   { <sym> <block> {*} }            #= FIRST
rule statement_control:NEXT    { <sym> <block> {*} }            #= NEXT
rule statement_control:LAST    { <sym> <block> {*} }            #= LAST
rule statement_control:PRE     { <sym> <block> {*} }            #= PRE
rule statement_control:POST    { <sym> <block> {*} }            #= POST
rule statement_control:CATCH   { <sym> <block> {*} }            #= CATCH
rule statement_control:CONTROL { <sym> <block> {*} }            #= CONTROL

rule modifier_expr { <EXPR> {*} }

rule statement_mod_cond:if     { <sym> <modifier_expr> {*} }    #= mod if
rule statement_mod_cond:unless { <sym> <modifier_expr> {*} }    #= mod unless
rule statement_mod_cond:when   { <sym> <modifier_expr> {*} }    #= mod for

rule statement_mod_loop:while { <sym> <modifier_expr> {*} }     #= mod while
rule statement_mod_loop:until { <sym> <modifier_expr> {*} }     #= mod until

rule statement_mod_loop:for   { <sym> <modifier_expr> {*} }     #= mod for
rule statement_mod_loop:given { <sym> <modifier_expr> {*} }     #= mod given
rule statement_mod_loop:when  { <sym> <modifier_expr> {*} }     #= mod when

token module_name {
    <name>                                          {*}         #= name
    <colonpair>*
    {*}
}

=begin perlhints *
token:  whatever
syn:    @list[*]
name:   whatever star
desc:   * in context of list index means "all the indices"
ex:     @list.pick(*)  # randomly pick all elements of @list
=end perlhints

token whatever { '*' {*} }

token version:sym<v> {
    v \d+ [ '.' (\d+ | <whatever>) ]* '+'?            {*}       #= vstyle
}

###################################################

token expect_term {
    <?ws>

    # queue up the prefixes to interleave with postfixes
    @<pre> := (
        [
        | <prefix>
            { $<prec> := $<prefix><prec> }
                                                        {*}     #= prefix
        | <prefix_circumfix_meta_operator>
            { $<prec> := $<prefix_circumfix_meta_operator><prec> }
                                                        {*}     #= precircum
        ]
        # XXX assuming no precedence change
        <prefix_postfix_meta_operator>*                 {*}     #= prepost
    )*

    <noun>                                              {*}     #= noun

    # also queue up any postfixes, since adverbs could change things
    @<post> := <expect_postfix>*                        {*}     #= postfix
    <?ws>
    <adverbs>?

    # now push ops over the noun according to precedence.
    { return self.nounphrase($¢, |$/) }
}

method nounphrase ($¢, :$noun, :@pre is rw, :@post is rw, *%_) {
    my $nounphrase = $noun;
    my $pre = pop @pre;
    my $post = shift @post;
    while $pre or $post {
        my $oldterm = $nounphrase;
        if $pre {
            if $post and $post<prec> gt $pre<prec> {
                $nounphrase = $post;
                $post = shift @post;
            }
            else {
                $nounphrase = $pre;
                $pre = pop @pre;
            }
        }
        else {
            $nounphrase = $post;
            $post = shift @post;
        }
        $nounphrase<term> = $oldterm;
    }
#    return $nounphrase;
    return $¢;  # XXX need to attach noun too!
}

token adverbs {
    [ <colonpair> <?ws> ]+
    {
        my $prop = $+prevop err
            self.panic($¢, 'No previous operator visible to adverbial pair (' ~
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
    | <term>
    | <statement_prefix>
    ]
    {*}
}

=begin perlhints =>
token:  pair
syn:    <key> => <value>
name:   pair
desc:   <key> => <value> constructs a pair, usually building a hash
ex:     my %continents = (
            England => 'Europe',
            Brazil  => 'South America',
            India   => 'Asia'
        );
# XXX perhaps a non-hash example?
=end perlhints

token pair {
    [
    | $<key>:=<ident> \h* '=>' $<val>:=<EXPR(%item_assignment)>
                                                        {*}     #= fat
    | [ <colonpair> <?ws> ]+
                                                        {*}     #= colon
    ]
    {*}
}

token colonpair {
    ':'
    [
    | '!' <ident>                                       {*}    #= false
    | <ident> [ <?unsp>? <postcircumfix> ]?              {*}    #= value
    | <postcircumfix>                                   {*}    #= structural
    | <sigiltwigil> <desigilname>                       {*}    #= varname
    ]
    {*}
}

token quotepair {
    ':'
    [
    | '!' <ident>                                        {*}    #= false
    | <ident> [ <unsp>? <?before '('> <postcircumfix> ]? {*}    #= value
    | \d+ <[a..z]>+                                             #= nth
    ]
    {*}
}

regex infix_nospace {
    <expect_infix>
    <!{ $<expect_infix> ~~ /\s/ }>
}

token expect_tight_infix ($loosest) {
    <!before '{' | <lambda> >     #'  # presumably a statement control block
    <expect_infix>
    ::: <?{ %+thisop<prec> ge $loosest }>
}

token expect_infix {
    <infix>
    <infix_postfix_meta_operator>*
    {*}
}

token dotty:sym<.+> { <sym> <methodop> {*} }                    #= plus
token dotty:sym<.*> { <sym> <methodop> {*} }                    #= star
token dotty:sym<.?> { <sym> <methodop> {*} }                    #= query
token dotty:sym<.=> { <sym> <methodop> {*} }                    #= equals
token dotty:sym<.^> { <sym> <methodop> {*} }                    #= caret
token dotty:sym<.:> { <sym> <methodop> {*} }                    #= colon
token dotty:sym<.>  { <sym> <dottyop>  {*} }                    #= plain

token dottyop {
    [
    | <methodop>
    | <postop>
    ]
    {*}
}

# Note, this rule mustn't do anything irreversible because it's used
# as a lookahead by the quote interpolator.

token expect_postfix {
    [
    | \\ <?before '.'>
    | <?unsp>?
    ]

    [ ['.' <?unsp>?]? <postfix_prefix_meta_operator> <?unsp>? ]*

    [
    | <dotty>
    | <postop>
    ]
    { $<prec> = $<postop><prec> }
    {*}
}

# Note: backtracks, or we'd never get to parse [LIST] on seeing [+ and such.
# (Also backtracks if on \op when no \op infix exists.)
regex prefix_circumfix_meta_operator:reduce (:%thisop? is context ) {
    @<sym> := [ '[' \\?? ]   # prefer no meta \ if op has \
    <infix_nospace>
    @<sym> := [ ']' ]

    [ <!{ %+thisop<assoc> eq 'non' }>
        || <panic: Can't reduce a non-associative operator> ]

    [ <!{ %+thisop<prec> eq %conditional<prec> }>
        || <panic: Can't reduce a conditional operator> ]

    { $<prec> := %+thisop<prec> }

    {*}                                                         #= [ ]
}

token prefix_postfix_meta_operator:sym< « >    { <sym> {*} }    #= hyper
token prefix_postfix_meta_operator:sym« << » { <sym> {*} }      #= HYPER

token postfix_prefix_meta_operator:sym< » >    { <sym> {*} }    #= hyper
token postfix_prefix_meta_operator:sym« >> » { <sym> {*} }      #= HYPER

token infix:pre { <infix_prefix_meta_operator> }
token infix:circ { <infix_circumfix_meta_operator> }

token infix_prefix_meta_operator:sym<!> ( --> Chaining) {
    <sym> <!before '!'> <infix_nospace>

    <?nonest: negation>

    [
    || <?{ %+thisop<assoc> eq 'chain'}>
    || <?{ %+thisop<assoc> and %+thisop<bool> }>
    || <panic: Only boolean infix operators may be negated>
    ]

    { %+thisop<hyper> and self.panic($¢, "Negation of hyper operator not allowed") }

    {*}                                                         #= !
}

regex nonest (Str $s) {
    <!{{ %+thisop{$s}++ }}> || <panic: Nested $s metaoperators not allowed>
}

token infix_circumfix_meta_operator:sym<X X> ( --> List_infix) {
    X <infix_nospace> X
    <nonest: cross>
    {*}                                                         #= X X
}

token infix_circumfix_meta_operator:sym<« »> ( --> Hyper) {
    '«' <infix_nospace> '»'
    <nonest: hyper>
    {*}                                                         #= « »
}

token infix_circumfix_meta_operator:sym<« «> ( --> Hyper) {
    '«' <infix_nospace> '«'
    <nonest: hyper>
    {*}                                                         #= « «
}

token infix_circumfix_meta_operator:sym<» »> ( --> Hyper) {
    '»' <infix_nospace> '»'
    <nonest: hyper>
    {*}                                                         #= » » 
}

token infix_circumfix_meta_operator:sym<» «> ( --> Hyper) {
    '»' <infix_nospace> '«'
    <nonest: hyper>
    {*}                                                         #= » «
}

token infix_circumfix_meta_operator:sym« << >> » ( --> Hyper) {
    '<<' <infix_nospace> '>>'
    <nonest: hyper>
    {*}                                                         #= << >>
}

token infix_circumfix_meta_operator:sym« << << » ( --> Hyper) {
    '<<' <infix_nospace> '<<'
    <nonest: hyper>
    {*}                                                         #= << <<
}

token infix_circumfix_meta_operator:sym« >> >> » ( --> Hyper) {
    '>>' <infix_nospace> '>>'
    <nonest: hyper>
    {*}                                                         #= >> >>
}

token infix_circumfix_meta_operator:sym« >> << » ( --> Hyper) {
    '>>' <infix_nospace> '<<'
    <nonest: hyper>
    {*}                                                         #= >> <<
}

token infix_postfix_meta_operator:sym<=> ( --> Item_assignment) {
    '='
    <nonest: assignment>

    [
    || <?{ %+thisop<prec> gt %item_assignment<prec> }>
    || <panic: Can't make assignment op of operator looser than assignment>
    ]

    [
    || <!{ %+thisop<assoc> eq 'chain' }>
    || <panic: Can't make assignment op of boolean operator>
    ]
    
    [
    || <!{ %+thisop<assoc> eq 'non' }>
    || <panic: Can't make assignment op of non-associative operator>
    ]
    
    {*}                                                         #= =
}

token postfix:i       ( --> Autoincrement) { <sym> {*} }         #= i
token postfix:sym<++> ( --> Autoincrement) { <sym> {*} }         #= incr
token postfix:sym<--> ( --> Autoincrement) { <sym> {*} }         #= decr

token postcircumfix:sym<( )> ( --> Methodcall)
    { '(' <semilist> ')' {*} }                                  #= ( )

token postcircumfix:sym<[ ]> ( --> Methodcall)
    { '[' <semilist> ']' {*} }                                  #= [ ]

token postcircumfix:sym<{ }> ( --> Methodcall)
    { '{' <semilist> '}' {*} }                                  #= { }

token postcircumfix:sym«< >» ( --> Methodcall)
    { '<' <anglewords> '>' {*} }                                #= < >

token postcircumfix:sym«<< >>» ( --> Methodcall)
    { '<<' <shellwords> '>>' {*}}                               #= << >>

token postcircumfix:sym<« »> ( --> Methodcall)
    { '«' <shellwords> '»' {*} }                                #= « »

token postop {
    | <postfix>         { $<prec> := $<postfix><prec> }
    | <postcircumfix>   { $<prec> := $<postcircumfix><prec> }
}

token methodop {
    [
    | <ident>
    | <?before '$' | '@' > <variable>
    | <?before <[ ' " ]>> <quote>
        { $<quote> ~~ /\W/ or self.panic($¢, "Useless use of quotes") }
    ] <?unsp>? 

    [
    | '.'? <?unsp>? '(' <semilist> ')'
    | ':' <?before \s> <!{ $+inquote }> <arglist>
    | <null>
    ]
    {*}
}

token arglist (StrPos :$endargs is context<rw> = 0) { <EXPR(%list_prefix)> }

token anglewords($stop) {
    <?ws> [ <!before $stop> .]*  # XXX need to split
}

token shellwords($stop) {
    <?ws> [ <!before $stop> .]*  # XXX need to split
}

token circumfix:sym<{ }> ( --> Circumfix) {
    <?before '{'> <block>
    {*}                                                         #= { }
}

token variable_decl {
    <variable>
    [   # Is it a shaped array or hash declaration?
        <?{ $<variable><sigiltwigil><sigil> eq '@' | '%' }>
        <?ws>
        <?before [ '<' | '(' |  '[' | '{' ] >
        <postcircumfix>
    ]?

    <trait>*
    <?ws>
    [
    | '=' <?ws> <EXPR(%item_assignment)>
    | '.=' <?ws> <EXPR(%item_assignment)>
    ]
}

rule scoped {
    [
    | <variable_decl>
    | <typename>? '(' <signature> ')' <trait>*
    | <package_declarator>
    | <typename>? <plurality_declarator>
    | <typename>? <routine_declarator>
    | <regex_declarator>
    | <typename>? <type_declarator>
    ]
    
    {*}
}

token scope_declarator:my       { <sym> <scoped> {*} }          #= my
token scope_declarator:our      { <sym> <scoped> {*} }          #= our
token scope_declarator:state    { <sym> <scoped> {*} }          #= state
token scope_declarator:constant { <sym> <scoped> {*} }          #= constant
token scope_declarator:has      { <sym> <scoped> {*} }          #= has

token package_declarator:class   { <sym> <package_def> {*} }    #= class
token package_declarator:grammar { <sym> <package_def> {*} }    #= grammar
token package_declarator:module  { <sym> <package_def> {*} }    #= module
token package_declarator:role    { <sym> <package_def> {*} }    #= role
token package_declarator:package { <sym> <package_def> {*} }    #= package

token package_declarator:require {   # here because of declarational aspects
    <sym>
    <module_name> <EXPR>?
    {*}
}

token package_declarator:trusts {
    <sym>
    <module_name>
    {*}
}

token package_def {
    <module_name>?
    <trait>* {*}                                                #= traits
    [
    || <?{ $+begin_compunit }> :: ';'
        {
            $<module_name> err self.panic($¢, "Compilation unit cannot be anonymous");
            $begin_compunit = 0;
        }
        {*}                                                     #= semi
    || <block>
        {*}                                                     #= block
    ]
    {*}
}

rule pluralized {
    [
    | <variable_decl>
    | '(' <signature> ')' <trait>*
    | <package_declarator>
    | <routine_declarator>
    | <regex_declarator>
    | <type_declarator>
    ]
    {*}
}

token plurality_declarator:multi { <sym> <pluralized> {*} }     #= multi
token plurality_declarator:proto { <sym> <pluralized> {*} }     #= proto
token plurality_declarator:only  { <sym> <pluralized> {*} }     #= only

token routine_declarator:sub       { <sym> <routine_def> {*} }  #= sub
token routine_declarator:method    { <sym> <method_def> {*} }   #= method
token routine_declarator:submethod { <sym> <method_def> {*} }   #= submethod
token routine_declarator:macro     { <sym> <macro_def> {*} }    #= macro

token regex_declarator:regex { <sym>       <regex_def> {*} }    #= regex
token regex_declarator:token { <sym>       <regex_def> {*} }    #= token
token regex_declarator:rule  { <sym>       <regex_def> {*} }    #= rule

# Most of these special variable rules are there simply to catch old p5 brainos

token special_variable:sym<$¢> { <sym> {*} }                    #= $¢

token special_variable:sym<$!> { <sym> <!before \w> {*} }       #= $!

token special_variable:sym<$!{ }> {
    # XXX the backslashes are necessary here for bootstrapping, not for P6...
    ( '$!\{' (.*?) '\}' )
    <obs("$0 variable", 'smart match against $!')>
}

token special_variable:sym<$/> {
    <sym>
    # XXX assuming nobody ever wants to assign $/ directly anymore...
    [ <?before \h* '=' <![=]> >
        <obs('$/ variable as input record separator',
             "filehandle's :irs attribute")>
    ]?
    {*}                                                         #= $/
}

token special_variable:sym<$~> {
    <sym> <?before \s | ',' | '=' | <terminator> >
    <obs('$~ variable', 'Form module')>
}

token special_variable:sym<$`> {
    <sym> <?before \s | ',' | <terminator> >
    <obs('$` variable', 'explicit pattern before <(')>
}

token special_variable:sym<$@> {
    <sym>
    <obs('$@ variable as eval error', '$!')>
}

token special_variable:sym<$#> {
    <sym>
    [
    || (\w+) <obs("\$#$0 variable", "@{$0}.end")>
    || <obs('$# variable', '.fmt')>
    ]
}
token special_variable:sym<$$> {
    <sym> <?before \s | ',' | <terminator> >
    <obs('$$ variable', '$*PID')>
}
token special_variable:sym<$%> {
    <sym>
    <obs('$% variable', 'Form module')>
}

# Note: this works because placeholders are restricted to lowercase
token special_variable:sym<$^X> {
    ( <sigil> '^' (<[A..Z]>) \W )
    <obscaret($0, $<sigil>, $1)>
}

token special_variable:sym<$^> {
    <sym> <?before \s | ',' | '=' | <terminator> >
    <obs('$^ variable', 'Form module')>
}

token special_variable:sym<$&> {
    <sym> <?before \s | ',' | <terminator> >
    <obs('$& variable', '$/ or $()')>
}

token special_variable:sym<$*> {
    <sym> <?before \s | ',' | '=' | <terminator> >
    <obs('$* variable', '^^ and $$')>
}

token special_variable:sym<$)> {
    <sym> <?before \s | ',' | <terminator> >
    <obs('$) variable', "$*EGID")>
}

token special_variable:sym<$-> {
    <sym> <?before \s | ',' | '=' | <terminator> >
    <obs('$- variable', 'Form module')>
}

token special_variable:sym<$=> {
    <sym> <?before \s | ',' | '=' | <terminator> >
    <obs('$= variable', 'Form module')>
}

token special_variable:sym<@+> {
    <sym> <?before \s | ',' | <terminator> >
    <obs('@+ variable', '.to method')>
}

token special_variable:sym<%+> {
    <sym> <?before \s | ',' | <terminator> >
    <obs('%+ variable', '.to method')>
}

token special_variable:sym<$+[ ]> {
    '$+['
    <obs('@+ variable', '.to method')>
}

token special_variable:sym<@+[ ]> {
    '@+['
    <obs('@+ variable', '.to method')>
}

token special_variable:sym<@+{ }> {
    '@+{'
    <obs('%+ variable', '.to method')>
}

token special_variable:sym<@-> {
    <sym> <?before \s | ',' | <terminator> >
    <obs('@- variable', '.from method')>
}

token special_variable:sym<%-> {
    <sym> <?before \s | ',' | <terminator> >
    <obs('%- variable', '.from method')>
}

token special_variable:sym<$+[ ]> {
    '$+['
    <obs('@- variable', '.from method')>
}

token special_variable:sym<@+[ ]> {
    '@+['
    <obs('@- variable', '.from method')>
}

token special_variable:sym<@+{ }> {
    '@+{'
    <obs('%- variable', '.from method')>
}

token special_variable:sym<$+> {
    <sym> <?before \s | ',' | <terminator> >
    <obs('$+ variable', 'Form module')>
}

token special_variable:sym<${^ }> {
    ( <sigil> '{^' (.*?) '}' )
    <obscaret($0, $<sigil>, $1)>
}

# XXX should eventually rely on multi instead of nested cases here...
multi method obscaret (Str $var, Str $sigil, Str $name) {
    my $repl = do given $sigil {
        when '$' {
            given $name {
                when 'MATCH'         { '$/' }
                when 'PREMATCH'      { 'an explicit pattern before <(' }
                when 'POSTMATCH'     { 'an explicit pattern after )>' }
                when 'ENCODING'      { '$?ENCODING' }
                when 'UNICODE'       { '$?UNICODE' }  # XXX ???
                when 'TAINT'         { '$*TAINT' }
                when 'OPEN'          { 'filehandle introspection' }
                when 'N'             { '$-1' } # XXX ???
                when 'L'             { 'Form module' }
                when 'A'             { 'Form module' }
                when 'E'             { '$!.extended_os_error' }
                when 'C'             { 'COMPILING namespace' }
                when 'D'             { '$*DEBUGGING' }
                when 'F'             { '$*SYSTEM_FD_MAX' }
                when 'H'             { '$?FOO variables' }
                when 'I'             { '$*INPLACE' } # XXX ???
                when 'O'             { '$?OS or $*OS' }
                when 'P'             { 'whatever debugger Perl 6 comes with' }
                when 'R'             { 'an explicit result variable' }
                when 'S'             { 'the context function' } # XXX ???
                when 'T'             { '$*BASETIME' }
                when 'V'             { '$*PERL_VERSION' }
                when 'W'             { '$*WARNING' }
                when 'X'             { '$*EXECUTABLE_NAME' }
                when *               { "a global form such as $sigil*$name" }
            }
        }
        when '%' {
            given $name {
                when 'H'             { '$?FOO variables' }
                when *               { "a global form such as $sigil*$name" }
            }
        }
        when * { "a global form such as $sigil*$0" }
    }; # XXX pugs needs semi here for some reason
    return self.obs($¢, "$var variable", $repl);
}

token special_variable:sym<${ }> {
    ( <sigil> '{' (.*?) '}' )
    <obs("$0 variable", "{$<sigil>}($1)")>
}

token special_variable:sym<$[> {
    <sym> <?before \s | ',' | '=' | <terminator> >
    <obs('$[ variable', 'user-defined array indices')>
}

token special_variable:sym<$]> {
    <sym> <?before \s | ',' | <terminator> >
    <obs('$] variable', '$*PERL_VERSION')>
}

token special_variable:sym<$\\> {
    <sym> <?before \s | ',' | '=' | <terminator> >
    <obs('$\\ variable', "the filehandle's :ors attribute")>
}

token special_variable:sym<$|> {
    <sym> <?before \s | ',' | '=' | <terminator> >
    <obs('$| variable', 'Form module')>
}

token special_variable:sym<$:> {
    <sym> <?before \s | ',' | '=' | <terminator> >
    <obs('$: variable', 'Form module')>
}

token special_variable:sym<$;> {
    <sym> <?before \s | ',' | '=' | <terminator> >
    <obs('$; variable', 'real multidimensional hashes')>
}

token special_variable:sym<$'> { #'
    <sym> <?before \s | ',' | <terminator> >
    <obs('$' ~ "'" ~ 'variable', "explicit pattern after )\x3E")>
}

token special_variable:sym<$"> {
    <sym> <?before \s | ',' | '=' | <terminator> >
    <obs('$" variable', '.join() method')>
}

token special_variable:sym<$,> {
    <sym> <?before \s | ',' | <terminator> >
    <obs(q/$, variable/, ".join() method")>
}

token special_variable:sym{'$<'} {
    <sym> <!before \s* \w+ \s* '>' >
    <obs('$< variable', "$*UID")>
}

token special_variable:sym«$>» {
    <sym> <?before \s | ',' | <terminator> >
    <obs("$() variable", "$*EUID")>
}

token special_variable:sym<$.> {
    <sym> <?before \s | ',' | <terminator> >
    <obs(q/$. variable/, "filehandle's .line method")>
}

token special_variable:sym<$?> {
    <sym> <?before \s | ',' | <terminator> >
    <obs('$? variable as child error', '$!')>
}

# desigilname should always follow a sigiltwigil

token desigilname {
    [
    | <?before '$' > <variable>
    | <name>
    ]
    {*}
}

token variable {
    [
    | <special_variable> {*}                                    #= special
    | <sigiltwigil>
        [
        || <?{ $<sigiltwigil><sigil> eq '&' }> ::
            <sublongname> {*}                                   #= subnoun
        || <desigilname> {*}                                    #= desigilname
        ]
        [
        | <?{ $<sigiltwigil><twigil> eq '.' }>
            <?unsp>? <?before '('> <postcircumfix> {*}          #= methcall
        | <null> {*}                                            #= $?foo
        ]
    | <sigil> \d+ {*}                                           #= $0
    | <sigil> <?before '<' | '('> <postcircumfix> {*}           #= $()
    | <name> '::' <?before '<' | '«' | '{' > <postcircumfix> {*} #= FOO::<$x>
    ]
    {*}
}

token sigiltwigil {
    <sigil> <twigil>?
    <?{{ {*}; 1 }}>               # XXX right way to allow use in longer token?
}

# Note, don't reduce on a bare sigil unless you don't want a twigil or
# you otherwise don't care what the longest token is.

token sigil:sym<$>  { <sym> }
token sigil:sym<@@> { <sym> }
token sigil:sym<@>  { <sym> }
token sigil:sym<%>  { <sym> }
token sigil:sym<&>  { <sym> }
token sigil:sym<::> { <sym> }

token twigil:sym<.> { <sym> }
token twigil:sym<!> { <sym> }
token twigil:sym<^> { <sym> }
token twigil:sym<*> { <sym> }
token twigil:sym<+> { <sym> }
token twigil:sym<?> { <sym> }
token twigil:sym<=> { <sym> }

token name {
    [
    | <ident> <nofat> [ '::' <ident> ]*
    | [ '::' <ident> ]+
    ]
    {*}
}

token subshortname {
    [
    | <category> <colonpair>+
    | <desigilname>
    ]
    {*}
}

token sublongname {
    <subshortname> <sigterm>?
    {*}
}

token subcall {
    # XXX should this be sublongname?
    <subshortname> <?unsp>? '.'? '(' <semilist> ')'
    {*}
}

token value {
    [
    | <quote>
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
    # parametric type?
    <?unsp>? [ <?before '['> <postcircumfix> ]?
    {*}
}

regex fulltypename {
    <typename>
    [ <?ws> of <?ws> <fulltypename> ]?
    {*}
}

token number {
    [
    | <integer>
    | <dec_number>
    | <rad_number>
    ]
    {*}
}

token integer {
    [
    | 0 [ b <[01]>+           [ _ <[01]>+ ]*
        | o <[0..7]>+         [ _ <[0..7]>+ ]*
        | x <[0..9a..fA..F]>+ [ _ <[0..9a..fA..F]>+ ]*
        | d \d+               [ _ \d+]*
        | \d+[_\d+]*
            {{ START { warn("Leading 0 does not indicate octal in Perl 6") } }}
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
    \d+[_\d+]* [ '.' \d+[_\d+]* [ <[Ee]> <[+\-]>? \d+ ]? ]
    {*}
}

token rad_number {
    ':' $<radix> := [\d+] <?unsp>?      # XXX optional dot here?
    [
    || '<'
            $<radint> := [<[ 0..9 a..z A..Z ]>+
            $<radfrac> := [ '.' <[ 0..9 a..z A..Z ]>+ ]? ]
            [ '*' $<base> := <radint> '**' $<exp> := <radint> ]?
       '>'
      { return radcalc($<radix>, $<radint>, $<radfrac>, $<base>, $<exp>) }
    || <?before '['> <postcircumfix>
    || <?before '('> <postcircumfix>
    ]
    {*}
}

token octint {
    <[ 0..7 ]>+
}

token hexint {
    <[ 0..9 a..f A..F ]>+
}

our @herestub_queue;

token q_herestub ($lang) {
    $<delimstr> := <quotesnabber()>  # force raw semantics on /END/ marker
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

method heredoc ($¢) {
    while my $herestub = shift @herestub_queue {
        my $delim = $herestub.delim;
        my $lang = $herestub.lang;
        my $doc;
        my $ws = "";
        my $stoppat = $delim eq "" ?? rx[^^ \h* $$]
                                   !! rx[^^ $ws:=(\h*?) $delim \h* $$ \n?];
        my @heredoc_initial_ws is context<rw>;
        if m:p/$doc:=<q_unbalanced($lang, :stop($stoppat))>/ {
            if $ws and @heredoc_initial_ws {
                my $wsequiv = $ws;
                $wsequiv ~~ s/^ (\t+) /{ ' ' x ($0 * 8) }/; # per spec
                for @heredoc_initial_ws {
                    next if s/^ $ws //;   # reward consistent tabbing
                    s/^^ (\t+) /{
                        ' ' x ($0.chars * (COMPILING::<$?TABSTOP> // 8))
                    }/;
                    s/^ $wsequiv // or s/^ \h+ //;
                }
            }
            $herestub.orignode<doc> = $doc;
        }
        else {
            self.panic($¢, "Ending delimiter $delim not found");
        }
    }
}

token quote:sym<' '>   { <?before "'"  > <quotesnabber(":q")>        }
token quote:sym<" ">   { <?before '"'  > <quotesnabber(":qq")>       }
token quote:sym<« »>   { <?before '«'  > <quotesnabber(":qq",":ww")> }
token quote:sym«<< >>» { <?before '<<' > <quotesnabber(":qq",":ww")> }
token quote:sym«< >»   { <?before '<'  > <quotesnabber(":q", ":w")>  }

token quote:sym</ />   {
    <?before '/'  > <quotesnabber(":regex")>
    [ (< i g s m x c e ] >+) 
        # note: only the submatch fails here on the obs call
        [ $0 ~~ 'i' <obs("/i",":i")> ]?
        [ $0 ~~ 'g' <obs("/g",":g")> ]?
        [ $0 ~~ 's' <obs("/s","^^ and $$ anchors")> ]?
        [ $0 ~~ 'm' <obs("/m",". or \N")> ]?
        [ $0 ~~ 'x' <obs("/x","normal default whitespace")> ]?
        [ $0 ~~ 'c' <obs("/c",":c or :p")> ]?
        [ $0 ~~ 'e' <obs("/e","interpolated {...} or s{} = ... form")> ]?
        <obs("suffix regex modifiers","prefix adverbs")>
    ]?
}

# handle composite forms like qww
token quote:qq { <sym> <quote_mod>
    <quotesnabber(':qq', $<quote_mod>)> }
token quote:q { <sym>  <quote_mod>
    <quotesnabber(':q', $<quote_mod>)> }

token quote_mod:w  { <sym> }
token quote_mod:ww { <sym> }
token quote_mod:x  { <sym> }
token quote_mod:to { <sym> }
token quote_mod:s  { <sym> }
token quote_mod:a  { <sym> }
token quote_mod:h  { <sym> }
token quote_mod:f  { <sym> }
token quote_mod:c  { <sym> }
token quote_mod:b  { <sym> }

token quote:rx { <sym> <quotesnabber(':regex')> }

token quote:m { <sym>  <quotesnabber(':regex')> }
token quote:mm { <sym> <quotesnabber(':regex', ':s')> }

token quote:s { <sym>  $<pat> := <quotesnabber(':regex')>
                        <finish_subst($<pat>)> }
token quote:ss { <sym> $<pat> := <quotesnabber(':regex', ':s')>
                        <finish_subst($<pat>)> }

token quote:tr { <sym> $<pat> := <quotesnabber(':trans')>
                        <finish_trans(<$pat>)> }

token finish_subst ($pat, :%thisop is context<rw>) {
    [
    # bracketed form
    | <?{ $pat<delim> == 2 }> ::
          <?ws>
          <infix>            # looking for pseudoassign here
          { %+thisop<prec> == %item_assignment<prec> or
              self.panic($¢, "Bracketed subst must use some form of assignment") }
          $<repl> := <EXPR(%item_assignment)>
    # unbracketed form
    | $<repl> := <q_unbalanced(qlang('Q',':qq'), $pat<delim>[0])>
    ]
}

token finish_trans ($pat) {
    [
    # bracketed form
    | <?{ $pat<delim> == 2 }> ::
          <?ws>
          $<repl> := <q_pickdelim(qlang('Q',':tr'))>
    # unbracketed form
    | $<repl> := <q_unbalanced(qlang('Q',':tr'), $pat<delim>[0])>
    ]
}

# The key observation here is that the inside of quoted constructs may
# be any of a lot of different sublanguages, and we have to parameterize
# which parse rule to use as well as what options to feed that parse rule.

role QLang {
    has %.option;
    has $.tweaker handles 'tweak';
    has $.parser;
    has $.escrule;

    # a method, so that everything is overridable in derived grammars
    method root_of_Q () {
        return
            tweaker => ::Q_tweaker,      # class name should be virtual here!
            parser => &Perl::q_pickdelim,
            option => < >,
            escrule => &Perl::quote_escapes;
    }

    method new (@pedigree) {
        if @pedigree == 1 {
#           my %start = try { self."root_of_@pedigree[0]" } //
            my %start = try { self.root_of_Q } //
                panic("Quote construct @pedigree[0] not recognized");
            return self.bless(|%start);
        }
        else {
            my $tail = pop @pedigree;
            my $self = qlang(@pedigree).clone
                err fail "Can't clone {@pedigree}: $!";
            return $self.tweak($tail);
        }
    }
}

sub qlang (@pedigree) {
    my $pedigree = @pedigree.join;
    (state %qlang){$pedigree} //= new QLang(@pedigree);
}

class Q_tweaker does QLang {
    has @.escapes;

    method escset {
        @.escapes ||=               # presumably resolves after adverbs
           '\\' xx ?%.option<b>,
            '$' xx ?%.option<s>,
            '@' xx ?%.option<a>,
            '%' xx ?%.option<h>,
            '&' xx ?%.option<f>,
            '{' xx ?%.option<c>;
    } #'

    multi method tweak (:q($single)) {
        $single or panic("Can't turn :q back off");
        %.option.keys and panic("Too late for :q");
        %.option = (:b, :!s, :!a, :!h, :!f, :!c);
    }

    multi method tweak (:qq($double)) {
        $double or panic("Can't turn :qq back off");
        %.option.keys and panic("Too late for :qq");
        %.option = (:b, :s, :a, :h, :f, :c);
    }

    multi method tweak (:b($backslash))   { %.option<b>  = $backslash }
    multi method tweak (:s($scalar))      { %.option<s>  = $scalar }
    multi method tweak (:a($array))       { %.option<a>  = $array }
    multi method tweak (:h($hash))        { %.option<h>  = $hash }
    multi method tweak (:f($function))    { %.option<f>  = $function }
    multi method tweak (:c($closure))     { %.option<c>  = $closure }

    multi method tweak (:x($exec))        { %.option<c>  = $exec }
    multi method tweak (:w($words))       { %.option<w>  = $words }
    multi method tweak (:ww($quotewords)) { %.option<ww> = $quotewords }

    multi method tweak (:to($heredoc)) {
        $.parser = &Perl::q_heredoc;
        %.option<to> = $heredoc;
    }

    multi method tweak (:$regex) {
        $.tweaker = ::RX_tweaker,
        $.parser = &Perl::rx_pickdelim;
        %.option = < >;
        $.escrule = &Perl::regex_metachar;
    }

    multi method tweak (:$trans) {
        $.tweaker = ::TR_tweaker,
        $.parser = &Perl::tr_pickdelim;
        %.option = < >;
        $.escrule = &Perl::trans_metachar;
    }

    multi method tweak (:$code) {
        $.tweaker = ::RX_tweaker,
        $.parser = &Perl::rx_pickdelim;
        %.option = < >;
        $.escrule = &Perl::regex_metachar;
    }

    multi method tweak (*%x) {
        panic("Unrecognized quote modifier: %x.keys()");
    }

}

class RX_tweaker does QLang {
    multi method tweak (:g($global))      { %.option<g>  = $global }
    multi method tweak (:i($ignorecase))  { %.option<i>  = $ignorecase }
    multi method tweak (:c($continue))    { %.option<c>  = $continue }
    multi method tweak (:p($pos))         { %.option<p>  = $pos }
    multi method tweak (:ov($overlap))    { %.option<ov> = $overlap }
    multi method tweak (:ex($exhaustive)) { %.option<ex> = $exhaustive }
    multi method tweak (:s($sigspace))    { %.option<s>  = $sigspace }

    multi method tweak (:$bytes)  { %.option<UNILEVEL> = 'bytes' }
    multi method tweak (:$codes)  { %.option<UNILEVEL> = 'codes' }
    multi method tweak (:$graphs) { %.option<UNILEVEL> = 'graphs' }
    multi method tweak (:$langs)  { %.option<UNILEVEL> = 'langs' }

    multi method tweak (:$rw)      { %.option<rw>      = $rw }
    multi method tweak (:$ratchet) { %.option<ratchet> = $ratchet }
    multi method tweak (:$keepall) { %.option<keepall> = $keepall }
    multi method tweak (:$panic)   { %.option<panic>   = $panic }

    # XXX probably wrong
    multi method tweak (:P5($Perl5)) {
        %.option<P5> = $Perl5;
        $.tweaker = ::P5RX_tweaker,
        $.parser = &Perl::p5rx_pickdelim;
        $.escrule = &Perl::p5regex_metachar;
    }

    multi method tweak (:$nth)            { %.option<nth> = $nth }
    multi method tweak (:x($times))       { %.option<x> = $times }

    # Ain't we special!
    multi method tweak (*%x) {
        my $na = %x.keys();
        my ($n,$a) = $na ~~ /^(\d+)(<[a-z]>+)$/
            or panic("Unrecognized regex modifier: $na");
        if $a eq 'x' {
            %.option{x} = $n;
        }
        elsif $a eq 'st' | 'nd' | 'rd' | 'th' {
            %.option{nth} = $n;
        }
    }
}

class P5RX_tweaker does QLang {
    multi method tweak (:$g) { %.option<g>  = $g }
    multi method tweak (:$i) { %.option<i>  = $i }
    multi method tweak (:$s) { %.option<s>  = $s }
    multi method tweak (:$m) { %.option<m>  = $m }

    multi method tweak (*%x) {
        panic("Unrecognized Perl 5 regex modifier: %x.keys()");
    }
}

class TR_tweaker does QLang {
    multi method tweak (:$c) { %.option<c>  = $c }
    multi method tweak (:$d) { %.option<d>  = $d }
    multi method tweak (:$s) { %.option<s>  = $s }

    multi method tweak (*%x) {
        panic("Unrecognized transliteration modifier: %x.keys()");
    }
}

token quotesnabber (*@q, :$delim is context<rw> = '') {
    <!before \w> <nofat> ::
    <?ws>

    [ (<quotepair>) { push @q, $0 } <?ws> ]*

    # Dispatch to current lang's subparser.
    {{
        let $<lang> = qlang('Q', @q);
        $<delimited> := $<lang>.parser.($<lang>);  # XXX probably wrong
        let $<delim> = $delim;
    }}
    {*}
}

# XXX should eventually be derived from current Unicode tables.
constant %open2close = {
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

method peek_delimiters ($¢) {
    return peek_brackets ||
        substr($_,0,1) xx 2;
}

method peek_brackets ($¢) {
    my $start;
    my $stop;
    if m:p/ \s / {
        self.panic($¢, "Whitespace not allowed as delimiter");
    }
    elsif m:p/ <?before <isPe>> / {
        self.panic($¢, "Use a closing delimiter for an opener is reserved");
    }
    elsif m:p/ <?before $start := [ (leftbrack) $0* ] > / {
        $stop = %open2close{$0} err
            self.panic($¢, "Don't know how to flip $start bracket");
        $stop x= $start.chars;
        return $start, $stop;
    }
    else {
        return ();
    }
}

regex bracketed ($lang = qlang("Q")) {
     <?{ ($<start>,$<stop>) = self.peek_brackets($¢) }>
     $<q> := <q_balanced($lang, $<start>, $<stop>)>
    {*}
}

regex q_pickdelim ($lang) {
    {{
       ($<start>,$<stop>) = self.peek_delimiters($¢);
       if $<start> eq $<stop> {
           $<q> := self.q_unbalanced($¢, $lang, $<stop>);
       }
       else {
           $<q> := self.q_balanced($¢, $lang, $<start>, $<stop>);
       }
    }}
    {*}
}

regex rx_pickdelim ($lang) {
    [
    | <?{ ($<start>,$<stop>) = self.peek_delimiters($¢) }>
      $<start>
      $<r> := <regex($<stop>)>        # counts its own brackets, we hope
    | [ $<stop> := [\S] || <panic: Regex delimiter must not be whitespace> ]
      $<r> := <regex($<stop>)>
    ]
    {*}
}

regex tr_pickdelim ($lang) {
    [
    | <?{ ($<start>,$<stop>) = self.peek_delimiters($¢) }>
      $<start>
      $<r> := <transliterator($<stop>)>
    | [ $<stop> := [\S] || <panic: tr delimiter must not be whitespace> ]
      $<r> := <transliterator($<stop>)>
    ]
    {*}
}

regex transliterator($stop) {
    # XXX your ad here
}

regex q_balanced ($lang, $start, $stop, :@esc = $lang.escset) {
    $<start> := <$start>
    $<text> := [.*?]
    @<more> := (
        <!before <$stop>>
        [ # XXX triple rule should just be in escapes to be customizable
        | <?before <$start> ** 3>
            $<dequote> := <EXPR(%LOOSEST,/<$stop> ** 3/)>
        | <?before <$start>>
            $<subtext> := <q_balanced($lang, $start, $stop, :@esc)>
        | <?before @esc>
            $<escape> := [ <q_escape($lang)> ]
        ]
        $<text> := [.*?]
    )*
    $<stop> := <$stop>
    {*}
}

regex q_unbalanced ($lang, $stop, :@esc = $lang.escset) {
    $<text> := [.*?]
    @<more> := (
      <!before <$stop>>
      <?before @esc> $<escape> := [ <q_escape($lang)> ]
      $<text> := [.*?]
    )*
    $<stop> := <$stop>
    {*}
}

# We get here only for escapes in escape set, even though more are defined.
method q_escape ($¢, $lang) {
    $lang<escrule>(self, $¢);
    {*}
}

token quote_escapes {
    [
    || \\ <qq_backslash>
    || <?before '{'> <block>
    || <?before '$'> <variable> <extrapost>?
    || <variable> <extrapost>
    || .
    ]
    {*}
}

# Note, backtracks!  So expect_postfix mustn't commit to anything permanent.
regex extrapost ($inquote is context = 1) {
    <expect_postfix>*
    # XXX Shouldn't need a backslash on anything but the right square here
    <?after <[ \] \} \> \) ]> > 
    {*}
}

rule multisig {
    ':'?'(' <signature> ')' [ '|' ':'?'(' <signature> ')' ]*
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
    | <?before <sigil> '.' [ '[' | '{' | '(' ] > <sigiltwigil> <postcircumfix>
    ]
    <trait>*
    <block>
    {*}
}

rule regex_def {
    <ident>?
    <trait>*
    [ ':'?'(' <signature> ')']?
    <regex_block>
    {*}
}

# XXX redundant with routine_def?
rule macro_def {
    | <ident>?  <multisig>?
    <trait>*
    <block>
    {*}
}

rule trait { <trait_verb> | <trait_auxiliary> }

rule trait_auxiliary:is   { <sym> <ident><postcircumfix>? }
rule trait_auxiliary:will { <sym> <ident> <block> }

rule trait_verb:of      { <sym> <fulltypename> }
rule trait_verb:returns { <sym> <fulltypename> }
rule trait_verb:handles { <sym> <EXPR> }

token capterm {
    '\\(' <capture> ')'
    {*}
}

rule capture {
    <EXPR>
    {*}
}

token sigterm {
    ':(' <signature> ')'
    {*}
}

rule signature (:$zone is context<rw> = 'posreq') {
    @<parsep> := ( <parameter>
                    ( ',' | ':' | ';' | ';;' | <?before '-->' | ')' | '{' > )
                 )*
    [ '-->' <fulltypename> ]?
    {*}
}

rule type_declarator:subset {
    <sym>
    <name>
    [ of <fulltypename> ]?
    where <EXPR>
    {*}
}

rule type_constraint {
    [
    | <value>
    | where <EXPR(%chaining)>
    ]
    {*}
}

rule post_constraint {
    [
    | <multisig>
    | where <EXPR(%chaining)>
    ]
    {*}
}

token param_var {
    <sigiltwigil>
    [
        # Is it a longname declaration?
    || <?{ $<sigiltwigil><sigil> eq '&' }> <?before <?ident> > ::
        $<ident> := <sublongname>

    ||  # Is it a shaped array or hash declaration?
        <?{ $<sigiltwigil><sigil> eq '@' | '%' }>
        <ident>?
        <?ws>
        <?before <[ \< \( \[ \{ ]> >
        <postcircumfix>

        # ordinary parameter name
    || <ident>

        # bare sigil?
    || <null>
    ]
}

token parameter {

    <type_constraint>*
    [
    | $<slurp> := [ $<quantchar>:=[ '*' ] <param_var> ]
        { let $<quant> := '*' }
    |   [ $<named> :=
            [ $<quantchar>:=[ ':' ]
                [
                | $<name>:=<?ident> '(' <param_var>  ')'
                | <param_var> { $<name> := $<param_var><ident> }
                ]
                { let $<quant> := '*' }
            ]
        | <param_var>
            { let $<quant> := '!'; }
        ]
        [ $<quantchar> := <[ ? ! ]> { let $<quant> := $<quantchar> } ]?
    ]
    <trait>*

    <post_constraint>*

    [
        <default_value> {{
            given $<quantchar> {
              when '!' { self.panic($¢, "Can't put a default on a required parameter") }
              when '*' { self.panic($¢, "Can't put a default on a slurpy parameter") }
            }
            let $<quant> := '?';
        }}
    ]?

    # enforce zone constraints
    {{
        given $<quant> {
            when '!' {
                given $+zone {
                    when 'posopt' {
self.panic($¢, "Can't use required parameter in optional zone");
                    }
                    when 'var' {
self.panic($¢, "Can't use required parameter in variadic zone");
                    }
                }
            }
            when '?' {
                given $+zone {
                    when 'posreq' { $+zone = 'posopt' }
                    when 'var' {
self.panic($¢, "Can't use optional positional parameter in variadic zone");
                    }
                }
            }
            when '*' {
                $+zone = 'var';
            }
        }
    }}
    {*}
}

rule default_value {
    '=' <EXPR(%item_assignment)>
}

rule statement_prefix:do      { <sym> <statement> {*} }         #= do
rule statement_prefix:try     { <sym> <statement> {*} }         #= try
rule statement_prefix:gather  { <sym> <statement> {*} }         #= gather
rule statement_prefix:contend { <sym> <statement> {*} }         #= contend
rule statement_prefix:async   { <sym> <statement> {*} }         #= async
rule statement_prefix:lazy    { <sym> <statement> {*} }         #= lazy

## term
token term:sym<undef> ( --> Term) {
    <sym> \h* <nofat>
    [ <?before '$/' >
        <obs('$/ variable as input record separator',
             "the filehandle's .slurp method")>
    ]?
    [ <?before < $ @ % & > >
        <obs('undef as a verb', 'undefine function')>
    ]?
    {*}                                                         #= undef
}

token term:sym<*> ( --> Term)
    { <sym> {*} }                                               #= *

token circumfix:sigil ( --> Term)
    { $<sym>:=<sigil> $<sym>:='(' <semilist> $<sym>:=')' {*} }    #= $( ) 

token circumfix:typecast ( --> Term)
    { $<sym>:=<typename> $<sym>:='(' <semilist> $<sym>:=')' {*} } #= Type( ) 

token circumfix:sym<( )> ( --> Term)
    { '(' <statementlist> ')'  {*} }                            #= ( )

token circumfix:sym<[ ]> ( --> Term)
    { '[' <statementlist> ']' {*} }                             #= [ ]

token circumfix:sym«< >» ( --> Term)
    { '<'  <anglewords '>'> '>'  {*} }                        #'#= < >
token circumfix:sym«<< >>» ( --> Term)
    { '<<' <shellwords '>>'> '>>' {*} }                       #'#= << >>
token circumfix:sym<« »> ( --> Term)
    { '«'  <shellwords '»'> '»'  {*} }                          #= « »

## methodcall

token infix:sym<.> ( --> Methodcall)
    { '.' <obs('. to concatenate strings', '~')> }

token postfix:sym{'->'} ( --> Methodcall)
    { '->' <obs('-> to call a method', '.')> }

## autoincrement
token postfix:sym<++> ( --> Autoincrement)
    { <sym> {*} }                                               #= ++

token postfix:sym<--> ( --> Autoincrement)
    { <sym> {*} }                                               #= --

token prefix:sym<++> ( --> Autoincrement)
    { <sym> {*} }                                               #= ++

token prefix:sym<--> ( --> Autoincrement)
    { <sym> {*} }                                               #= --


## exponentiation
token infix:sym<**> ( --> Exponentiate)
    { <sym> {*} }                                               #= **

## symbolic unary
token prefix:sym<!> ( --> Symbolic_unary)
    { <sym> {*} }                                               #= !

token prefix:sym<+> ( --> Symbolic_unary)
    { <sym> {*} }                                               #= +

token prefix:sym<-> ( --> Symbolic_unary)
    { <sym> {*} }                                               #= -

token prefix:sym<~> ( --> Symbolic_unary)
    { <sym> {*} }                                               #= ~

token prefix:sym<?> ( --> Symbolic_unary)
    { <sym> {*} }                                               #= ?

token prefix:sym<=> ( --> Symbolic_unary)
    { <sym> {*} }                                               #= =

token prefix:sym<*> ( --> Symbolic_unary)
    { <sym> {*} }                                               #= *

token prefix:sym<**> ( --> Symbolic_unary)
    { <sym> {*} }                                               #= **

token prefix:sym<~^> ( --> Symbolic_unary)
    { <sym> {*} }                                               #= ~^

token prefix:sym<+^> ( --> Symbolic_unary)
    { <sym> {*} }                                               #= +^

token prefix:sym<?^> ( --> Symbolic_unary)
    { <sym> {*} }                                               #= ?^

token prefix:sym<^> ( --> Symbolic_unary)
    { <sym> {*} }                                               #= ^

token prefix:sym<|> ( --> Symbolic_unary)
    { <sym> {*} }                                               #= |


## multiplicative
token infix:sym<*> ( --> Multiplicative)
    { <sym> {*} }                                               #= *

token infix:sym</> ( --> Multiplicative)
    { <sym> {*} }                                               #= /

token infix:sym<%> ( --> Multiplicative)
    { <sym> {*} }                                               #= %

# Note: no word boundary check after x, relies on longest token for x2 xx2 etc
token infix:sym<x> ( --> Multiplicative)
    { <sym> {*} }                                               #= x

token infix:sym<xx> ( --> Multiplicative)
    { <sym> {*} }                                               #= xx

token infix:sym<+&> ( --> Multiplicative)
    { <sym> {*} }                                               #= +&

token infix:sym« +< » ( --> Multiplicative)
    { <sym> {*} }                                               #= +<

token infix:sym« << » ( --> Multiplicative)
    { <sym> <obs('<< to do left shift', '+< or ~<')> }

token infix:sym« >> » ( --> Multiplicative)
    { <sym> <obs('>> to do right shift', '+> or ~>')> }

token infix:sym« +> » ( --> Multiplicative)
    { <sym> {*} }                                               #= +>

token infix:sym<~&> ( --> Multiplicative)
    { <sym> {*} }                                               #= ~&

token infix:sym« ~< » ( --> Multiplicative)
    { <sym> {*} }                                               #= ~<

token infix:sym« ~> » ( --> Multiplicative)
    { <sym> {*} }                                               #= ~>


## additive
token infix:sym<+> ( --> Additive)
    { <sym> {*} }                                               #= +

token infix:sym<-> ( --> Additive)
    { <sym> {*} }                                               #= -

token infix:sym<~> ( --> Additive)
    { <sym> {*} }                                               #= ~

token infix:sym<+|> ( --> Additive)
    { <sym> {*} }                                               #= +|

token infix:sym<+^> ( --> Additive)
    { <sym> {*} }                                               #= +^

token infix:sym<~|> ( --> Additive)
    { <sym> {*} }                                               #= ~|

token infix:sym<~^> ( --> Additive)
    { <sym> {*} }                                               #= ~^

token infix:sym<?|> ( --> Additive)
    { <sym> {*} }                                               #= ?|

token infix:sym<?^> ( --> Additive)
    { <sym> {*} }                                               #= ?^


## junctive and (all)
token infix:sym<&> ( --> Junctive_and)
    { <sym> {*} }                                               #= &


## junctive or (any)
token infix:sym<|> ( --> Junctive_or)
    { <sym> {*} }                                               #= |

token infix:sym<^> ( --> Junctive_or)
    { <sym> {*} }                                               #= ^


## named unary examples
token prefix:rand ( --> Named_unary)
    { <sym> {*} }                                               #= rand

token prefix:sleep ( --> Named_unary)
    { <sym> {*} }                                               #= sleep

token prefix:abs ( --> Named_unary)
    { <sym> {*} }                                               #= abs

## nonchaining binary
token infix:sym« <=> » ( --> Nonchaining)
    { <sym> {*} }                                               #= <=>

token infix:cmp ( --> Nonchaining)
    { <sym> {*} }                                               #= cmp

token infix:is ( --> Nonchaining)
    { <sym> {*} }                                               #= is

token infix:but ( --> Nonchaining)
    { <sym> {*} }                                               #= but

token infix:does ( --> Nonchaining)
    { <sym> {*} }                                               #= does

token infix:sym<..> ( --> Nonchaining)
    { <sym> {*} }                                               #= ..

token infix:sym<^..> ( --> Nonchaining)
    { <sym> {*} }                                               #= ^..

token infix:sym<..^> ( --> Nonchaining)
    { <sym> {*} }                                               #= ..^

token infix:sym<^..^> ( --> Nonchaining)
    { <sym> {*} }                                               #= ^..^

token infix:sym<ff> ( --> Nonchaining)
    { <sym> {*} }                                               #= ff

token infix:sym<^ff> ( --> Nonchaining)
    { <sym> {*} }                                               #= ^ff

token infix:sym<ff^> ( --> Nonchaining)
    { <sym> {*} }                                               #= ff^

token infix:sym<^ff^> ( --> Nonchaining)
    { <sym> {*} }                                               #= ^ff^

token infix:sym<fff> ( --> Nonchaining)
    { <sym> {*} }                                               #= fff

token infix:sym<^fff> ( --> Nonchaining)
    { <sym> {*} }                                               #= ^fff

token infix:sym<fff^> ( --> Nonchaining)
    { <sym> {*} }                                               #= fff^

token infix:sym<^fff^> ( --> Nonchaining)
    { <sym> {*} }                                               #= ^fff^


## chaining binary
token infix:sym<==> ( --> Chaining)
    { <sym> {*} }                                               #= ==

token infix:sym<!=> ( --> Chaining)
    { <sym> {*} }                                               #= !=

token infix:sym« < » ( --> Chaining)
    { <sym> {*} }                                               #= <

token infix:sym« <= » ( --> Chaining)
    { <sym> {*} }                                               #= <=

token infix:sym« > » ( --> Chaining)
    { <sym> {*} }                                               #= >

token infix:sym« >= » ( --> Chaining)
    { <sym> {*} }                                               #= >=

token infix:sym<~~> ( --> Chaining)
    { <sym> {*} }                                               #= ~~

token infix:sym<!~> ( --> Chaining)
    { <obs('!~ to do negated pattern matching', '!~~')> }

token infix:sym<=~> ( --> Chaining)
    { <obs('=~ to do pattern matching', '~~')> }

token infix:sym<eq> ( --> Chaining)
    { <sym> {*} }                                               #= eq

token infix:sym<ne> ( --> Chaining)
    { <sym> {*} }                                               #= ne

token infix:sym<lt> ( --> Chaining)
    { <sym> {*} }                                               #= lt

token infix:sym<le> ( --> Chaining)
    { <sym> {*} }                                               #= le

token infix:sym<gt> ( --> Chaining)
    { <sym> {*} }                                               #= gt

token infix:sym<ge> ( --> Chaining)
    { <sym> {*} }                                               #= ge

token infix:sym<=:=> ( --> Chaining)
    { <sym> {*} }                                               #= =:=

token infix:sym<===> ( --> Chaining)
    { <sym> {*} }                                               #= ===


## tight and
token infix:sym<&&> ( --> Tight_and)
    { <sym> {*} }                                               #= &&


## tight or
token infix:sym<||> ( --> Tight_or)
    { <sym> {*} }                                               #= ||

token infix:sym<^^> ( --> Tight_or)  {
    <sym>
    { $<assoc> := 'list' }  # override Tight_or's 'left' associativity
    {*}                                                         #= ^^
}

token infix:sym<//> ( --> Tight_or)
    { <sym> {*} }                                               #= //


## conditional
token infix:sym<?? !!> ( --> Conditional) {
    '??'
    <EXPR(%conditional)>
    [ '!!' ||
        [
        || <?before '='> <panic: Assignment not allowed within ??!!>
        || <?before '::'> <panic: Please use !! rather than ::>
        || <?before <infix>>    # Note: a tight infix would have parsed right
            <panic: Precedence too loose within ??!!; use ??()!! instead >
        || <panic: Found ?? but no !!; possible precedence problem>
        ]
    ]
    {*}                                                         #= ?? !!
}

token infix:sym<?> ( --> Conditional)
    { <obs('?: for the conditional operator', '??!!')> }


## assignment
token infix:sym<=> ( --> Assignment)
    { <sym> {*} }                                               #= =

token infix:sym<:=> ( --> Assignment)
    { <sym> {*} }                                               #= :=

token infix:sym<::=> ( --> Assignment)
    { <sym> {*} }                                               #= ::=

# XXX need to do something to turn subcall into method call here...
token infix:sym<.=> ( --> Assignment)
    { <sym> {*} }                                               #= .=

# Note, other assignment ops generated by infix_postfix_meta_operator rule

## loose unary
token prefix:sym<true> ( --> Loose_unary)
    { <sym> {*} }                                               #= true

token prefix:sym<not> ( --> Loose_unary)
    { <sym> {*} }                                               #= not

## list item separator
token infix:sym<,> ( --> Comma)
    { <sym> {*} }                                               #= ,

## list infix
token infix:sym<X> ( --> List_infix)
    { <sym> {*} }                                               #= X

token infix:sym<Z> ( --> List_infix)
    { <sym> {*} }                                               #= Z

token infix:sym<minmax> ( --> List_infix)
    { <sym> {*} }                                               #= minmax

token prefix:sigil ( --> List_prefix)
    { $<sym>:=<sigil> \s <arglist> {*} }                          #= $

token prefix:typecast ( --> List_prefix)
    { $<sym>:=<typename> \s <arglist> {*} }                       #= Type

# unrecognized identifiers are assumed to be post-declared listops.
# (XXX for cheating purposes this rule must be the last prefix: rule)
token prefix:listop ( --> List_prefix)
    { ::                        # call this rule last (as "shortest" token)
        $<sym>:=<ident>
        [
        || \s <nofat> <arglist> {*}                             #= listop args
        || <nofat> {*}                                          #= listop noarg
        ]
        {*}                                                     #= listop
    }

## loose and
token infix:sym<and> ( --> Loose_and)
    { <sym> {*} }                                               #= and

## loose or
token infix:sym<or> ( --> Loose_or)
    { <sym> {*} }                                               #= or

token infix:sym<xor> ( --> Loose_or)
    { <sym> {*} }                                               #= xor

token infix:sym<err> ( --> Loose_or)
    { <sym> {*} }                                               #= err

## expression terminator

token terminator:sym<;> ( --> Terminator)
    { <?before ';' > {*} }                                               #= ;

token terminator:sym« <== » ( --> Terminator)
    { <?before '<==' > {*} }                                    #= <==

token terminator:sym« ==> » ( --> Terminator)
    { <?before '==>' > {*} }              #'                    #= ==>

token terminator:sym« --> » ( --> Terminator)
    { <?before '-->' > {*} }              #'                    #= -->

token terminator:sym<)> ( --> Terminator)
    { <?before <sym> > {*} }                                    #= )

token terminator:sym<]> ( --> Terminator)
    { <?before ']' > {*} }                                      #= ]

token terminator:sym<}> ( --> Terminator)
    { <?before '}' > {*} }                                      #= }

token terminator:sym<!!> ( --> Terminator)
    { <?before '!!' > {*} }                                     #= !!

regex stdstopper {
    | $
    | <terminator>
    | <statement_mod_cond>
    | <statement_mod_loop>
    | <?{ $¢.to === $+endstmt }>
    | <?{ $¢.to === $+endargs }>
#    | <$+unitstopper>
}

# XXX not the correct way to add in a terminator for a sublanguage...
token assertstopper { <stdstopper> | '>' }

# A fairly complete (but almost certainly buggy) operator precedence parser

method EXPR ($¢, %preclim = %LOOSEST,
                :$stop = &stdstopper,
               # :$seen = self.expect_term($¢)
                )
{
    my $preclim = %preclim<prec>;
    my $inquote is context = 0;
#    my @terminator = self.before($¢, -> $¢ { $stop(self: $¢) } );
#    return () if @terminator and @terminator[0].bool;
    my $prevop is context<rw>;
    my %thisop is context<rw>;
    my @termstack;
    my @opstack;

    push @opstack, %term;         # (just a sentinel value)
    my @t = self.expect_term($¢);
    my $seen = @t[0];
    push @termstack, $seen;

    my &reduce := -> {
        say "entering reduce, termstack == ", +@termstack, " opstack == ", +@opstack;
        my $op = pop @opstack;
        given $op<assoc> {
            when 'chain' {
                say "reducing chain";
                my @chain;
                push @chain, pop(@termstack);
                push @chain, $op;
                while @opstack {
                    last if $op<prec> ne @opstack[-1]<prec>;
                    push @chain, pop(@termstack);
                    push @chain, pop(@opstack)<top>;
                }
                push @chain, pop(@termstack);
                $op<top><chain> = reverse @chain;
                push @termstack, $op<top>;
            }
            when 'list' {
                say "reducing list";
                my @list;
                push @list, pop(@termstack);
                while @opstack {
                    last if $op<top><sym> ne @opstack[-1]<top><sym>;
                    push @list, pop(@termstack);
                    pop(@opstack);
                }
                push @list, pop(@termstack);
                $op<top><list> = reverse @list;
                push @termstack, $op<top>;
            }
            default {
                say "reducing";
                my @list;
                say +@termstack;
                $op<top><right> = pop @termstack;
                $op<top><left> = pop @termstack;
                push @termstack, $op<top>;
            }
        }
    }

    loop {
        my @terminator = self.before($seen, -> $¢ { $stop(self: $¢) } );
        my $t = @terminator[0];
    last if defined $t and @terminator[0].bool;
        %thisop = ();
#        my @infix = self.expect_tight_infix($seen, $preclim);
        my @infix = self.expect_infix($seen);
        my $infix = @infix[0];
        $seen = $infix;
        
        # XXX might want to allow this in a declaration though
        if not $infix { self.panic($¢, "Can't have two terms in a row") }

        if not defined %thisop<prec> {
            say "No prec given in thisop!";
            %thisop = %terminator;
        }
        my Str $newprec = %thisop<prec>;

        # Does new infix (or terminator) force any reductions?
        while @opstack[-1]<prec> lt $newprec {
            reduce();
        }

        # Not much point in reducing the sentinels...
        last if $newprec lt $LOOSEST;

        # Equal precedence, so use associativity to decide.
        if @opstack[-1]<prec> eq $newprec {
            given %thisop<assoc> {
                when 'non'   { self.panic($¢, qq["$infix" is not associative]) }
                when 'left'  { reduce() }   # reduce immediately
                when 'right' | 'chain' { }  # just shift
                when 'list'  {              # if op differs reduce else shift
                    reduce() if %thisop<top><sym> !eqv @opstack[-1]<top><sym>;
                }
                default { self.panic($¢, qq[Unknown associativity "$_" for "$infix"]) }
            }
        }
        push @opstack, %thisop;
        my @terminator = self.before($seen, -> $¢ { $stop(self: $¢) } );
        if @terminator and @terminator[0].bool {
            self.panic($¢, "$infix.perl() is missing right term");
        }
        %thisop = ();
        my @t = self.expect_term($seen);
        $seen = @t[0];
        push @termstack, $seen;
        say "after push: " ~ +@termstack;
    }
    reduce() while +@termstack > 1;
    @termstack == 1 or self.panic($¢, "Internal operator parser error, termstack == {+@termstack}");
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
    '||'?
    <regex_ordered_conjunction> ** '||'
    {*}
}

rule regex_ordered_conjunction {
    <regex_submatch> ** '&&'
    {*}
}

rule regex_submatch {
    <regex_unordered_disjunction> ** [ \!?'~~' ]
    {*}
}

rule regex_unordered_disjunction {
    [ '|' <!before '|'> ]?
    <regex_unordered_conjunction> ** [ '|' <!before '|'> ]
    {*}
}

rule regex_unordered_conjunction {
    <regex_sequence> ** [ '&' <!before '&'> ]
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
    || (\w)
    || <panic: "unrecognized metacharacter">
    ]
    {*}
}

# sequence stoppers
token regex_metachar:sym« > » { '>'  :: <fail> }
token regex_metachar:sym<&&>  { '&&' :: <fail> }
token regex_metachar:sym<&>   { '&'  :: <fail> }
token regex_metachar:sym<||>  { '||' :: <fail> }
token regex_metachar:sym<|>   { '|'  :: <fail> }
token regex_metachar:sym<]>   { ']'  :: <fail> }
token regex_metachar:sym<)>   { ')'  :: <fail> }
token regex_metachar:sym<\\\\> { \\\\ :: <fail> }

token regex_metachar:quant { <regex_quantifier> <panic: quantifier quantifies nothing> }

# "normal" metachars
token regex_metachar:sym<{ }> {
    <block>
    {{ @<sym> := <{ }> }}
    {*}                                                         #= { }
}

token regex_metachar:mod {
    <regex_mod_internal>
    { @<sym> := $<regex_mod_internal><sym> }
    {*}                                                         #= :mod
}

token regex_metachar:sym<[ ]> {
    '[' <regex ']'> ']'
    { @<sym>:=<[ ]> }
    {*}                                                         #= [ ]
}

token regex_metachar:sym<( )> {
    '(' <regex ')'> ')'
    { @<sym>:=<( )> }
    {*}                                                         #= ( )
}

token regex_metachar:sym« <( » { '<(' {*} }                     #= <(
token regex_metachar:sym« )> » { ')>' {*} }                     #= )>

token regex_metachar:sym« << » { '<<' {*} }                     #= <<
token regex_metachar:sym« >> » { '>>' {*} }                     #= >>
token regex_metachar:sym< « > { '«' {*} }                       #= «
token regex_metachar:sym< » > { '»' {*} }                       #= »

token regex_metachar:qw {
    <?before '<' \s >  # (note required whitespace)
    <quote>
    {*}                                                         #= quote
}

token regex_metachar:sym«< >» {
    '<' <unsp>? <regex_assertion> '>'
    {*}                                                         #= < >
}
token regex_metachar:sym<\\> { <sym> <regex_backslash> {*} }    #= \
token regex_metachar:sym<.>  { <sym> {*} }                      #= .
token regex_metachar:sym<^^> { <sym> {*} }                      #= ^^
token regex_metachar:sym<^>  { <sym> {*} }                      #= ^
token regex_metachar:sym<$$> {
    <sym>
    [ <?before (\w+)> <obs("\$\$$0 to deref var inside a regex","\$(\$$0)")> ]?
    {*}
}
token regex_metachar:sym<$>  {
    '$'
    <before
    | $
    | \s
    | '|'
    | ')'
    | ']'
    | '>'
    >
    {*}                                                         #= $
}

token regex_metachar:sym<' '> { <?before "'"  > <quotesnabber(":q")>  }
token regex_metachar:sym<" "> { <?before '"'  > <quotesnabber(":qq")> }

token regex_metachar:var {
    <!before '$$'>
    $<sym>:=<variable> <?ws>
    $<binding> := ( ':=' <?ws> <regex_quantified_atom> )?
    {*}                                                         #= var
}

token codepoint {
    '[' (.*?) ']'
}

token q_backslash:qq { <?before qq> <quote> }
token q_backslash:sym<\\> { <sym> }
token q_backslash:misc { :: (.) }

token qq_backslash:a { <sym> }
token qq_backslash:b { <sym> }
token qq_backslash:c { <sym>
    [
    || '[' <-[ \] \v ]>* ']'
    || <codepoint>
    ]
}
token qq_backslash:e { <sym> }
token qq_backslash:f { <sym> }
token qq_backslash:n { <sym> }
token qq_backslash:o { <sym> [ <octint> | '['<octint>[','<octint>]*']' ] }
token qq_backslash:r { <sym> }
token qq_backslash:t { <sym> }
token qq_backslash:x { <sym> [ <hexint> | '['<hexint>[','<hexint>]*']' ] }
token qq_backslash:misc { :: \W || <panic: unrecognized backslash sequence> }

token regex_backslash:a { :i <sym> }
token regex_backslash:b { :i <sym> }
token regex_backslash:c { :i <sym>
    [
    || '[' <-[ \] \v ]>* ']'
    || <codepoint>
    ]
}
token regex_backslash:d { :i <sym> }
token regex_backslash:e { :i <sym> }
token regex_backslash:f { :i <sym> }
token regex_backslash:h { :i <sym> }
token regex_backslash:n { :i <sym> }
token regex_backslash:o { :i <sym> [ <octint> | '['<octint>[','<octint>]*']' ] }
token regex_backslash:r { :i <sym> }
token regex_backslash:t { :i <sym> }
token regex_backslash:v { :i <sym> }
token regex_backslash:w { :i <sym> }
token regex_backslash:x { :i <sym> [ <hexint> | '['<hexint>[','<hexint>]*']' ] }
token regex_backslash:oops { :: <panic: unrecognized regex backslash sequence> }

token regex_assertion:sym<?> { <sym> <regex_assertion> }
token regex_assertion:sym<!> { <sym> <regex_assertion> }

token regex_assertion:sym<{ }> { <block> }
token regex_assertion:variable {
    <?before <sigil>>  # note: semantics must be determined per-sigil
    <EXPR(%LOOSEST,&assertstopper)>
    {*}                                                        #= variable
}
token regex_assertion:method {
    <?before '.' <!before '>'> >
    <EXPR(%LOOSEST,&assertstopper)>
    {*}                                                        #= method
}
token regex_assertion:ident { <ident> [               # is qq right here?
                                | ':' <?ws>
                                    <q_unbalanced(qlang('Q',':qq'), :stop«>»)>
                                | '(' <semilist> ')'
                                | <?ws> <EXPR(%LOOSEST,&assertstopper)>
                                ]?
}

token regex_assertion:sym<[> { <before '[' > <cclass_elem>+ }
token regex_assertion:sym<+> { <before '+' > <cclass_elem>+ }
token regex_assertion:sym<-> { <before '-' > <cclass_elem>+ }
token regex_assertion:sym<.> { <sym> }
token regex_assertion:sym<,> { <sym> }
token regex_assertion:sym<~~> { <sym> <desigilname>? }

token regex_assertion:bogus { <panic: unrecognized regex assertion> }

token cclass_elem {
    [ '+' | '-' | <null> ]
    [
    | <name>
    | <before '['> <bracketed(QLang('cclass'))>
    ]
}

token regex_mod_arg { '(' <semilist> ')' }

token regex_mod_internal:adv {
    <quotepair> { @<sym> := «: $<quotepair><key>» }
}
token regex_mod_internal:sym<:i> { <sym> <regex_mod_arg>? }
token regex_mod_internal:sym<:!i> { <sym> }
token regex_mod_internal:oops { <panic: unrecognized regex modifier> }

# token regex_mod_external:adv {
#    <quotepair> { @<sym> := «: $<quotepair><key>» }
#}
# token regex_mod_external:sym<:g> { <sym> <regex_mod_arg> }
# token regex_mod_external:sym<:global> { <sym> <regex_mod_arg> }
# token regex_mod_external:sym<:s> { <sym> <regex_mod_arg> }
# token regex_mod_external:sym<:sigspace> { <sym> <regex_mod_arg> }
# token regex_mod_external:sym<:nth> { <sym> <regex_mod_arg> }
# token regex_mod_external:nth { ':'\d+ ( x | st | nd | rd | th ) }
# token regex_mod_external:oops { <panic: unrecognized regex modifier> }

token regex_quantifier:sym<*>  { <sym> <quantmod> }
token regex_quantifier:sym<+>  { <sym> <quantmod> }
token regex_quantifier:sym<?>  { <sym> <quantmod> }
token regex_quantifier:sym<**> { <sym> <quantmod> <?ws>
    [
    | \d+ [ '..' [ \d+ | '*' ] ]?
    | <block>
    | <regex_atom>
    ]
}

token quantmod { [ '?' | '!' | ':' | '+' ]? }

# The <panic: message> rule is called for syntax errors.
# If there are any <suppose> points, backtrack and retry parse
# with a different supposition.  If it gets farther than the
# panic point, print out the supposition ("Looks like you
# used a Perl5-style shift operator (<<) at line 42.  Maybe
# you wanted +< or |< instead.")  Or some such...
# In any event, this is only for better diagnostics, and
# further compilation is suppressed by the <commit><fail>.

rule panic (Str $s) { <commit> <fail($s)> }

# 3rd arg assumes more things will become obsolete after Perl 6 comes out...
method obs ($¢, Str $old, Str $new, Str $when = ' in Perl 6') {
    self.panic($¢, "Obsolete use of $old;$when please use $new instead");
}

say "Starting...";
$_ = '42';
my $result = Perl.new(:targ('42+1')).EXPR(0);
print $result.yaml;

## vim: expandtab sw=4
