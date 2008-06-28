grammar Perl:ver<6.0.0.alpha>:auth<http://perl.org>;

my $LANG is context;

# random rule for debugging, please ignore
regex foo {
   .*? X
}

=begin things todo

    add more suppositions and figure out exact error continuation semantics
    think about longest-token-defeating {*} that maybe should be <?{ {*}; 1}>
    add parsing this file to sanity tests :)
    evaluate "is context<rw>" for reentrancy brokenness

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
to the match state within the action block, so we need not mention it
explicitly.

Also, some rules are named by syntactic category plus an additonal symbol
specified in adverbial form, either in bare :name form or in :sym<name>
form.  (It does not matter which form you use for identifier symbols,
except that to specify a symbol "sym" you must use the :sym<sym> form
of adverb.)  If you use the <sym> rule within the rule, it will parse the
symbol at that point.  At the final reduction point of a rule, if $sym
has been set, that is used as the final symbol name for the rule.  This
need not match the symbol specified as part the rule name; that is just
for disambiguating the name.  However, if no $sym is set, the original
symbol will be used by default.

Note that rules with only one action need no #= comment, so the identifier
of the following stub is just "TOP".

Another nod toward preprocessing is that blocks that contain nested braces
are delimited by double braces so that the preprocessor does not need to
understand Perl 6 code.

This grammar also assumes transitive longest-token semantics, though
we make a feeble attempt to order rules so a procedural interpretation
of alternation can usually produce a correct parse.  (This will tend
to become less true over time.)

=end comment overview

method TOP ($STOP = undef) {
    if defined $STOP {
        self.unitstop($STOP).comp_unit;
    }
    else {
        self.comp_unit;
    }
}


# The internal precedence levels are *not* part of the public interface.
# The current values are mere implementation; they may change at any time.
# Users should specify precedence only in relation to existing levels.

constant %term            = (:prec<z=>);
constant %methodcall      = (:prec<y=>);
constant %autoincrement   = (:prec<x=>);
constant %exponentiation  = (:prec<w=>, :assoc<right>, :assign);
constant %symbolic_unary  = (:prec<v=>);
constant %multiplicative  = (:prec<u=>, :assoc<left>,  :assign);
constant %additive        = (:prec<t=>, :assoc<left>,  :assign);
constant %replication     = (:prec<s=>, :assoc<left>,  :assign);
constant %concatenation   = (:prec<r=>, :assoc<left>,  :assign);
constant %junctive_and    = (:prec<q=>, :assoc<list>,  :assign);
constant %junctive_or     = (:prec<p=>, :assoc<list>,  :assign);
constant %named_unary     = (:prec<o=>);
constant %nonchaining     = (:prec<n=>, :assoc<non>);
constant %chaining        = (:prec<m=>, :assoc<chain>, :bool);
constant %tight_and       = (:prec<l=>, :assoc<left>,  :assign);
constant %tight_or        = (:prec<k=>, :assoc<left>,  :assign);
constant %conditional     = (:prec<j=>, :assoc<right>);
constant %item_assignment = (:prec<i=>, :assoc<right>);
constant %loose_unary     = (:prec<h=>);
constant %comma           = (:prec<g=>, :assoc<list>);
constant %list_infix      = (:prec<f=>, :assoc<list>,  :assign);
constant %list_assignment = (:prec<i=>, :sub<e=>, :assoc<right>);
constant %list_prefix     = (:prec<e=>);
constant %loose_and       = (:prec<d=>, :assoc<left>,  :assign);
constant %loose_or        = (:prec<c=>, :assoc<left>,  :assign);
constant %LOOSEST         = (:prec<a=!>);
constant %terminator      = (:prec<a=>, :assoc<list>);

# "epsilon" tighter than terminator
#constant $LOOSEST = %LOOSEST<prec>;
constant $LOOSEST = "a=!"; # XXX preceding line is busted


role PrecOp {

    # This is hopefully called on a match to mix in operator info by type.
    method coerce (Match $m) {
        # $m but= ::?CLASS;
        my $var = self.WHAT ~ '::o';
        my $d = %::($var); 
        if not $d<transparent> {
            for keys(%$d) { $m<O>{$_} = $d.{$_} };
            $m.deb("coercing to " ~ self) if $DEBUG +& DEBUG::EXPR;
        }
        return $m;
    }

} # end role

class Hyper does PrecOp {
 our %o = (:transparent);
} # end class

class Term does PrecOp {
    our %o = %term;
} # end class
class Methodcall does PrecOp {
    our %o = %methodcall;
} # end class
class Autoincrement does PrecOp {
    our %o = %autoincrement;
} # end class
class Exponentiation does PrecOp {
    our %o = %exponentiation;
} # end class
class Symbolic_unary does PrecOp {
    our %o = %symbolic_unary;
} # end class
class Multiplicative does PrecOp {
    our %o = %multiplicative;
} # end class
class Additive does PrecOp {
    our %o = %additive;
} # end class
class Replication does PrecOp {
    our %o = %replication;
} # end class
class Concatenation does PrecOp {
    our %o = %concatenation;
} # end class
class Junctive_and does PrecOp {
    our %o = %junctive_and;
} # end class
class Junctive_or does PrecOp {
    our %o = %junctive_or;
} # end class
class Named_unary does PrecOp {
    our %o = %named_unary;
} # end class
class Nonchaining does PrecOp {
    our %o = %nonchaining;
} # end class
class Chaining does PrecOp {
    our %o = %chaining;
} # end class
class Tight_and does PrecOp {
    our %o = %tight_and;
} # end class
class Tight_or does PrecOp {
    our %o = %tight_or;
} # end class
class Conditional does PrecOp {
    our %o = %conditional;
} # end class
class Item_assignment does PrecOp {
    our %o = %item_assignment;
} # end class
class Loose_unary does PrecOp {
    our %o = %loose_unary;
} # end class
class Comma does PrecOp {
    our %o = %comma;
} # end class
class List_infix does PrecOp {
    our %o = %list_infix;
} # end class
class List_assignment does PrecOp {
    our %o = %list_assignment;
} # end class
class List_prefix does PrecOp {
    our %o = %list_prefix;
} # end class
class Loose_and does PrecOp {
    our %o = %loose_and;
} # end class
class Loose_or does PrecOp {
    our %o = %loose_or;
} # end class
class Terminator does PrecOp {
    our %o = %terminator;
} # end class

# Categories are designed to be easily extensible in derived grammars
# by merely adding more rules in the same category.  The rules within
# a given category start with the category name followed by a differentiating
# adverbial qualifier to serve (along with the category) as the longer name.

# The endsym context, if specified, says what to implicitly check for in each
# rule right after the initial <sym>.  Normally this is used to make sure
# there's appropriate whitespace.  # Note that endsym isn't called if <sym>
# isn't called.

my $endsym is context = "null";
my $endargs is context = -1;

proto token category { <...> }

token category:category { <sym> }

token category:sigil { <sym> }
proto token sigil { <...> }

token category:twigil { <sym> }
proto token twigil { <...> }

token category:special_variable { <sym> }
proto token special_variable { <...> }

token category:version { <sym> }
proto token version { <...> }

token category:module_name { <sym> }
proto token module_name { <...> }

token category:term { <sym> }
proto token term { <...> }

token category:quote { <sym> }
proto token quote () { <...> }

token category:prefix { <sym> }
proto token prefix is unary is defequiv(%symbolic_unary) { <...> }

token category:infix { <sym> }
proto token infix is binary is defequiv(%additive) { <...> }

token category:postfix { <sym> }
proto token postfix is unary is defequiv(%autoincrement) { <...> }

token category:dotty { <sym> }
proto token dotty (:$endsym is context = 'unspacey') { <...> }

token category:circumfix { <sym> }
proto token circumfix { <...> }

token category:postcircumfix { <sym> }
proto token postcircumfix is unary { <...> }  # unary as far as EXPR knows...

token category:quote_mod { <sym> }
proto token quote_mod { <...> }

token category:trait_verb { <sym> }
proto token trait_verb (:$endsym is context = 'spacey') { <...> }

token category:trait_auxiliary { <sym> }
proto token trait_auxiliary (:$endsym is context = 'spacey') { <...> }

token category:type_declarator { <sym> }
proto token type_declarator () { <...> }

token category:scope_declarator { <sym> }
proto token scope_declarator () { <...> }

token category:package_declarator { <sym> }
proto token package_declarator () { <...> }

token category:multi_declarator { <sym> }
proto token multi_declarator () { <...> }

token category:routine_declarator { <sym> }
proto token routine_declarator () { <...> }

token category:regex_declarator { <sym> }
proto token regex_declarator () { <...> }

token category:statement_prefix { <sym> }
proto rule  statement_prefix () { <...> }

token category:statement_control { <sym> }
proto rule  statement_control (:$endsym is context = 'spacey') { <...> }

token category:statement_mod_cond { <sym> }
proto rule  statement_mod_cond () { <...> }

token category:statement_mod_loop { <sym> }
proto rule  statement_mod_loop () { <...> }

token category:infix_prefix_meta_operator { <sym> }
proto token infix_prefix_meta_operator is binary { <...> }

token category:infix_postfix_meta_operator { <sym> }
proto token infix_postfix_meta_operator is binary { <...> }

token category:infix_circumfix_meta_operator { <sym> }
proto token infix_circumfix_meta_operator is binary { <...> }

token category:postfix_prefix_meta_operator { <sym> }
proto token postfix_prefix_meta_operator is unary { <...> }

token category:prefix_postfix_meta_operator { <sym> }
proto token prefix_postfix_meta_operator is unary { <...> }

token category:prefix_circumfix_meta_operator { <sym> }
proto token prefix_circumfix_meta_operator is unary { <...> }

token category:terminator { <sym> }
proto token terminator { <...> }

token unspacey { <.unsp>? }
token spacey { <?before \s | '#'> }

# Lexical routines

token ws {
    # XXX exists is a p5ism
    :my @stub = return self if exists self.<_>[self.pos]<ws>;
    :my $startpos = self.pos;

    [
	| \h+ <![#\s\\]> { $¢.<_>[$¢.pos]<ws> = $startpos; }	# common case
	| <?before \w> <?after \w> :::
	    { $¢.<_>[$startpos]<ws> = undef; }
	    <!>        # must \s+ between words
    ]
    ||
    [
    | <.unsp>
    | <.vws> <.heredoc>
    | <.unv>
    | $ { $¢.moreinput }
    ]*

    { $¢.<_>[$¢.pos]<ws> =
	$¢.pos == $startpos
	    ?? undef
	    !! $startpos;
    }
    {*}
}

token unsp {
    \\ <?before [\s|'#'] >
    [
    | <.vws>                     {*}                             #= vwhite
    | <.unv>                  {*}                                #= unv
    | $ { $¢.moreinput }
    ]*
    {*}
}

token vws {
    \v
    [ '#DEBUG -1' { say "DEBUG"; $Perl::DEBUG = $Cursor5::DEBUG = $::DEBUG = -1; } ]?
}

# We provide two mechanisms here:
# 1) define $+moreinput, or
# 2) override moreinput method
method moreinput () {
    $+moreinput.() if $+moreinput;
}

token unv {
   | \h+                 {*}                                    #= hwhite
   | <?before '='> ^^ :: <.pod_comment>  {*}                    #= pod
   | \h* '#' :: [
         # assuming <bracketed> defaults to standard set
         |  <?opener> ::
            [
               <?after ^^ . > <.panic: "Can't use embedded comments in column 1">
            || <.bracketed>   {*}                               #= embedded
            ]
         | :: \N*            {*}                                 #= end
         ]
}

token ident {
    <.alpha> \w*
}

# XXX We need to parse the pod eventually to support $= variables.

token pod_comment {
    ^^ '=' <.unsp>?
    [
    | 'begin' \h+ <ident> :: .*? \n
      '=' <.unsp>? 'end' \h+ $<ident> » \N*         {*}         #= tagged
    | 'begin' » :: \h* \n .*? \n
      '=' <.unsp>? 'end' » \N*                      {*}         #= anon
    | :: \N*                                           {*}         #= misc
    ]
    {*}
}

# Top-level rules

# Note: we only check for the stopper.  We don't check for ^ because
# we might be embedded in something else.
rule comp_unit {
    :my $begin_compunit is context = 1;
    :my $endargs        is context<rw> = -1;

    <statementlist>
    [ <?unitstopper> || <.panic: "Can't understand next input--giving up"> ]
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


token lambda { '->' | '<->' }


token block {
    '{'
    <statementlist>
    [ '}' || <.panic: "Missing right brace"> ]

    [
    | <?before \h* \n> <.ws>	# (usual case without comments)
	{ $¢.<_>[$¢.pos]<endstmt> = 1; } {*}                    #= endstmt simple 
    | \h* <.unsp>? <?before <[,:]>> {*}                         #= normal 
    | <.unv>? <?before \n > <.ws>
	{ $¢.<_>[$¢.pos]<endstmt> = 1; } {*}                    #= endstmt complex
    | {*} { $¢.<_>[$¢.pos]<endargs> = 1; }                      #= endargs
    ]
    {*}
}

token regex_block {  # XXX make polymorphic and combine with block someday
    '{'
    <regex( ::Regex.unbalanced('}') )>
    [ '}' || <.panic: "Missing right brace"> ]

    [
    | <?before \h* \n> <.ws>	# (usual case without comments)
	{ $¢.<_>[$¢.pos]<endstmt> = 1; } {*}                    #= endstmt simple 
    | \h* <.unsp>? <?before <[,:]>> {*}                         #= normal 
    | <.unv>? <?before \n > <.ws>
	{ $¢.<_>[$¢.pos]<endstmt> = 1; } {*}                    #= endstmt complex
    | {*} { $¢.<_>[$¢.pos]<endargs> = 1; }                      #= endargs
    ]
    {*}
}

# statement semantics
rule statementlist {
    [<statement><.eat_terminator> ]*
    {*}
}

# embedded semis, context-dependent semantics
rule semilist {
    [<statement><.eat_terminator> ]*
    {*}
}


token label {
    <ident> ':' <?before \s> <.ws>

    [ <?{ $¢.is_type($<ident>.text) }>
      <.panic("You tried to use an existing typename as a label")>
#      <suppose("You tried to use an existing name $/{'ident'} as a label")>
    ]?

    # add label as a pseudo type
    {{ eval 'COMPILING::{"::$<ident>"} = Label.new($<ident>)' }}  # XXX need statement ref too?

    {*}
}

token statement {
    :my $endargs is context = -1;
    <label>*                                     {*}            #= label
    [
    | <statement_control>                        {*}            #= control
    | <EXPR> {*}                                                #= expr
	[
	| <statement_mod_loop> {*}                              #= mod loop
	| <statement_mod_cond> {*}                              #= mod cond
	    [
	    | <statement_mod_loop> {*}                          #= mod condloop
	    ]?
	]?
        {*}                                                     #= modexpr
    | <?before ';'> {*}                                         #= null
    ]
    {*}
}


token eat_terminator {
    [
    || ';'
    || <?{ $¢.<_>[$¢.pos]<endstmt> }>
    || <?before <terminator>>
    || $
    || {{ if $¢.<_>[$¢.pos]<ws> { $¢.pos = $¢.<_>[$¢.pos]<ws>; } }}   # undo any line transition
        <.panic: "Statement not terminated properly">  # "can't happen" anyway :)
    ]
}


rule statement_control:use {\
    <sym>
    <module_name> <EXPR>?
    {*}
}


rule statement_control:no {\
    <sym>
    <module_name> <EXPR>?
    {*}
}


rule statement_control:if {\
    <sym>
    <EXPR>                           {*}                        #= if expr
    <pblock>                         {*}                        #= if block
    $<elsif> = ( 'elsif'<?spacey> <EXPR>       {*}                #= elsif expr
                        <pblock>     {*} )*                     #= elsif block
    $<else> = ( 'else'<?spacey> <pblock>       {*} )?             #= else
    {*}
}


rule statement_control:unless {\
    <sym> 
    <EXPR>                           {*}                        #= expr
    <pblock>                         {*}                        #= block
    {*}
}


rule statement_control:while {\
    <sym>
    [ <?before '(' ['my'? '$'\w+ '=']? '<' '$'?\w+ '>' ')'>   #'
        <.panic: "This appears to be Perl 5 code"> ]?
    <EXPR>                             {*}                      #= expr
    <pblock>                           {*}                      #= block
    {*}
}


rule statement_control:until {\
    <sym>
    <EXPR>                             {*}                      #= expr
    <pblock>                           {*}                      #= block
    {*}
}


rule statement_control:repeat {\
    <sym>
    [
        | ('while'|'until') <EXPR>         {*}                      #= wu expr
          <block>                      {*}                      #= wu block
        | <block>                      {*}                      #= block wu
          ('while'|'until') <EXPR>         {*}                      #= expr wu
    ]
    {*}
}


rule statement_control:loop {\
    <sym>
    $<eee> = (
        '('
            $<e1> = <EXPR> ';'   {*}                            #= e1
            $<e2> = <EXPR> ';'   {*}                            #= e2
            $<e3> = <EXPR>       {*}                            #= e3
        ')'                      {*}                            #= eee
    )?
    <block>                     {*}                             #= block
    {*}
}


rule statement_control:for {\
    <sym>
    [ <?before [my]? '$'\w+ '(' >
        <.panic: "This appears to be Perl 5 code"> ]?
    <EXPR>                             {*}                      #= expr
    <pblock>                           {*}                      #= block
    {*}
}


rule statement_control:given {\
    <sym>
    <EXPR>                             {*}                      #= expr
    <pblock>                           {*}                      #= block
    {*}
}
rule statement_control:when {\
    <sym>
    <EXPR>                             {*}                      #= expr
    <pblock>                           {*}                      #= block
    {*}
}
rule statement_control:default {<sym> <block> {*} }

rule statement_control:BEGIN   {<sym> <block> {*} }
rule statement_control:CHECK   {<sym> <block> {*} }
rule statement_control:INIT    {<sym> <block> {*} }
rule statement_control:END     {<sym> <block> {*} }
rule statement_control:START   {<sym> <block> {*} }
rule statement_control:ENTER   {<sym> <block> {*} }
rule statement_control:LEAVE   {<sym> <block> {*} }
rule statement_control:KEEP    {<sym> <block> {*} }
rule statement_control:UNDO    {<sym> <block> {*} }
rule statement_control:FIRST   {<sym> <block> {*} }
rule statement_control:NEXT    {<sym> <block> {*} }
rule statement_control:LAST    {<sym> <block> {*} }
rule statement_control:PRE     {<sym> <block> {*} }
rule statement_control:POST    {<sym> <block> {*} }
rule statement_control:CATCH   {<sym> <block> {*} }
rule statement_control:CONTROL {<sym> <block> {*} }

rule term:BEGIN   {<sym> <block> {*} }
rule term:CHECK   {<sym> <block> {*} }
rule term:INIT    {<sym> <block> {*} }
rule term:START   {<sym> <block> {*} }
rule term:ENTER   {<sym> <block> {*} }
rule term:FIRST   {<sym> <block> {*} }

rule modifier_expr { <EXPR> {*} }

rule statement_mod_cond:if     {<sym> <modifier_expr> {*} }     #= if
rule statement_mod_cond:unless {<sym> <modifier_expr> {*} }     #= unless
rule statement_mod_cond:when   {<sym> <modifier_expr> {*} }     #= when

rule statement_mod_loop:while {<sym> <modifier_expr> {*} }      #= while
rule statement_mod_loop:until {<sym> <modifier_expr> {*} }      #= until

rule statement_mod_loop:for   {<sym> <modifier_expr> {*} }      #= for
rule statement_mod_loop:given {<sym> <modifier_expr> {*} }      #= given

token role_name { <module_name> [ <?before '['> <postcircumfix> ]? }

token module_name:normal {
    <name>                                          {*}         #= name
    <colonpair>*
    {*}
}

token module_name:deprecated { 'v6-alpha' }

token version:sym<v> {
    'v' [\d+ | '*'] ** '.' '+'?
}

###################################################

token pre {
    [
    | <prefix>
        { $<O> = $<prefix><O> }
                                                    {*}         #= prefix
    | <prefix_circumfix_meta_operator>
        { $<O> = $<prefix_circumfix_meta_operator><O> }
                                                    {*}         #= precircum
    ]
    # XXX assuming no precedence change
    ::
    <prefix_postfix_meta_operator>*                 {*}         #= prepost
    <.ws>
    {*}
}

token expect_term {
    [
    | <noun>
    | <pre>+ :: <noun>
    ]
    ::

    # also queue up any postfixes, since adverbs could change things
    [ <?stdstopper> ||
	<post>*
	<.ws>
	<adverbs>?
    ]
}

token adverbs {
    <!stdstopper>
    [ <colonpair> <.ws> ]+
    {
        my $prop = $+prevop orelse
            $¢.panic('No previous operator visible to adverbial pair');
        $prop.adverb($<colonpair>);
    }
    {*}
}

token noun {
    [
    | <fatarrow>
    | <package_declarator>
    | <scope_declarator>
    | <multi_declarator>
    | <routine_declarator>
    | <regex_declarator>
    | <type_declarator>
    | <circumfix>
    | <dotty>
#    | <subcall>
    | <variable> { $<sigil> = $<variable><sigil> }
    | <value>
    | <capterm>
    | <sigterm>
    | <term>
    | <statement_prefix>
    | <colonpair>
    ]
    {*}
}


token fatarrow {
    <key=ident> \h* '=>' :: <.ws> <val=EXPR(item %item_assignment)>
}

token colonpair {
    ':'
    [
    | '!' <ident>                                        {*}    #= false
    | <ident> [ <.unsp>? <postcircumfix> ]?              {*}    #= value
    | <postcircumfix>                                    {*}    #= structural
    | <sigil> :: <twigil>? <desigilname>                 {*}    #= varname
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

token expect_tight_infix ($loosest) {
    <!before '{' | <lambda> >     #'  # presumably a statement control block
    <expect_infix>
    { $<O> := $<expect_infix><O> }
    ::: <?{ $<O><prec> ge $loosest }>
}

token expect_infix {
    :my $op is context;         # (used in infix_postfix_meta_operator)
    <!stdstopper>
    <!infixstopper>
    [
    | <infix> { $op = $<infix>; }
       <infix_postfix_meta_operator>*  # may modify $op
       { $<O> = $op<O>; $<sym> = $op<sym>; }
    | <infix_prefix_meta_operator>
        { $<O> = $<infix_prefix_meta_operator><O>;
        $<sym> = $<infix_prefix_meta_operator><sym>; }
    | <infix_circumfix_meta_operator>
        { $<O> = $<infix_circumfix_meta_operator><O>;
        $<sym> = $<infix_circumfix_meta_operator><sym>; }
    ]
    {*}
}

# doing fancy as one rule simplifies LTM
token dotty:sym<.*> ( --> Methodcall) {
    ('.' <[+*?=^:]>) <?unspacey> <methodop>
    { $<sym> = $0.item; }
    {*}
}

token dotty:sym<.> ( --> Methodcall) {
    <sym> <dottyop>
    {*}
}

token privop ( --> Methodcall) {
    '!' <methodop>
    {*}
}

token dottyop {
    [
    | <methodop>
    | <postop>     # forcing postop's precedence to methodcall here
    ]
    {*}
}

# Note, this rule mustn't do anything irreversible because it's used
# as a lookahead by the quote interpolator.

token post {
    <!stdstopper>

    # last whitespace didn't end here
    <!{ $¢.<_>[$¢.pos]<ws> }>

    <?unspacey>

    [ ['.' <.unsp>?]? <postfix_prefix_meta_operator> <.unsp>? ]*

    [
    | <dotty>  { $<O> = $<dotty><O> }
    | <privop> { $<O> = $<privop><O> }
    | <postop> { $<O> = $<postop><O> }
    ]
    {*}
}

# Note: backtracks, or we'd never get to parse [LIST] on seeing [+ and such.
# (Also backtracks if on \op when no \op infix exists.)
regex prefix_circumfix_meta_operator:reduce {
    '[' <?before \S* ']' > ::
    \\??   # prefer no meta \ if op has \
    <expect_infix>
    ']'
    { $<O> = $<expect_infix><O>; }

    [ <!{ $<O><assoc> eq 'non' }>
        || <.panic: "Can't reduce a non-associative operator"> ]

    [ <!{ $<O><prec> eq %conditional<prec> }>
        || <.panic: "Can't reduce a conditional operator"> ]

    {*}
}

token prefix_postfix_meta_operator:sym< « >    { <sym> | '<<' {*} }

token postfix_prefix_meta_operator:sym< » >    { <sym> | '>>' {*} }

token infix_prefix_meta_operator:sym<!> ( --> Chaining) {
    <sym> <!before '!'> <infix> ::

    { $<O> = $<infix><O>; }
    <?lex1: 'negation'>

    [
    || <?{ $<O><assoc> eq 'chain'}>
    || <?{ $<O><assoc> and $<O><bool> }>
    || <.panic: "Only boolean infix operators may be negated">
    ]

    { $<O><hyper> and $¢.panic("Negation of hyper operator not allowed") }

    {*}
}

method lex1 (Str $s) {
    self.<O>{$s}++ or self.panic("Nested $s metaoperators not allowed");
}

token infix_circumfix_meta_operator:sym<X X> ( --> List_infix) {
    X <infix> X
    { $<O> = $<infix><O>; }
    <?lex1: 'cross'>
    {*}
}

token infix_circumfix_meta_operator:sym<« »> ( --> Hyper) {
    [
    | [ '«' | '»' ] <infix> [ '«' | '»' ]
    | [ '<<' | '>>' ] <infix> [ '<<' | '>>' ]
    ]
    { $<O> := $<infix><O>; }
    <?lex1: 'hyper'>
    {*}
}

token infix_postfix_meta_operator:sym<=> ( --> Item_assignment) {
    '=' ::
    { $<O> = $+op<O>; }
    <?lex1: 'assignment'>

    [
    || <?{ $<O><prec> gt %item_assignment<prec> }>
    || <.panic: "Can't make assignment op of operator looser than assignment">
    ]

    [
    || <!{ $<O><assoc> eq 'chain' }>
    || <.panic: "Can't make assignment op of boolean operator">
    ]
    
    [
    || <!{ $<O><assoc> eq 'non' }>
    || <.panic: "Can't make assignment op of non-associative operator">
    ]
    
    {*}
}

token postcircumfix:sym<( )> ( --> Methodcall)
    { '(' <semilist> ')' {*} }

token postcircumfix:sym<[ ]> ( --> Methodcall)
    { '[' <semilist> ']' {*} }

token postcircumfix:sym<{ }> ( --> Methodcall)
    { '{' <semilist> '}' {*} }

token postcircumfix:sym«< >» ( --> Methodcall)
    { '<' <nibble(Perl::Q.tweak(:q).tweak(:w), rx/\>/)> '>' {*} }

token postcircumfix:sym«<< >>» ( --> Methodcall)
    { '<<' <nibble(Perl::Q.tweak(:qq).tweak(:ww), rx/\>\>/)> '>>' {*}}

token postcircumfix:sym<« »> ( --> Methodcall)
    { '«' <nibble(Perl::Q.tweak(:qq).tweak(:ww), rx/\»/)> {*} }

token postop {
    | <postfix>         { $<O> := $<postfix><O> }
    | <postcircumfix>   { $<O> := $<postcircumfix><O> }
}

token methodop {
    [
    | <name>
    | <?before '$' | '@' > <variable>
    | <?before <[ ' " ]> > <quote>
        { $<quote> ~~ /\W/ or $¢.panic("Useless use of quotes") }
    ] <.unsp>? 

    [
    | '.'? <.unsp>? '(' <semilist> ')'
    | ':' <?before \s> <!{ $+inquote }> <arglist>
    ]?
    {*}
}

token arglist {
    :my StrPos $endargs is context<rw> = 0;
    <.ws>
#    <EXPR(item %list_prefix)>
    <EXPR>
    {*}
}

token circumfix:sym<{ }> ( --> Term) {
    <?before '{' | <lambda> > <pblock>
    {*}
}

token variable_declarator {
    <variable> { $<sigil> = $<variable><sigil> }
    [   # Is it a shaped array or hash declaration?
      #  <?{ $<sigil> eq '@' | '%' }>
        <?before [ '<' | '(' |  '[' | '{' ] >
        <postcircumfix>
    ]?

    <trait>*
    <.ws>
    [
    | '=' <.ws> <EXPR( ($<sigil> // '') eq '$' ?? item %item_assignment !! item %list_prefix )>
    | '.=' <.ws> <EXPR(item %item_assignment)>
    ]?
}

rule scoped {
    [
    | <declarator>
    | <regex_declarator>
    | <package_declarator>
    | <fulltypename>+ <multi_declarator>
    | <multi_declarator>
    ]
    
    {*}
}


token scope_declarator:my       { <sym> <scoped> {*} }
token scope_declarator:our      { <sym> <scoped> {*} }
token scope_declarator:state    { <sym> <scoped> {*} }
token scope_declarator:constant { <sym> <scoped> {*} }
token scope_declarator:has      { <sym> <scoped> {*} }

token package_declarator:class   { <sym> <package_def('module_name')> {*} }
token package_declarator:grammar { <sym> <package_def('module_name')> {*} }
token package_declarator:module  { <sym> <package_def('module_name')> {*} }
token package_declarator:package { <sym> <package_def('module_name')> {*} }
token package_declarator:role    { <sym> <package_def('role_name')> {*} }

token package_declarator:require {   # here because of declarational aspects
    <sym> <.ws>
    <module_name> <EXPR>?
    {*}
}

token package_declarator:trusts {
    <sym> <.ws>
    <module_name>
    {*}
}

rule package_def ($namerule) {
    $<name>=<$namerule>? <trait>*
    [
       <block> {*}                                                     #= block
    || <?{ $+begin_compunit }> :: <?before ';'>
        {
            $<name> orelse $¢.panic("Compilation unit cannot be anonymous");
	    # do something semantic with name here
            $+begin_compunit = 0;
        }
        {*}                                                     #= semi
    || <panic: 'No block found for module definition'>
    ]
    {*}
}

token declarator {
    [
    | <variable_declarator>
    | '(' <signature> [ ')' || <.panic: "Missing right parenthsesis"> ] <trait>*
    | <routine_declarator>
    | <regex_declarator>
    | <type_declarator>
    ]
    {*}
}

token multi_declarator:multi { <sym> <.ws> <declarator> {*} }
token multi_declarator:proto { <sym> <.ws> <declarator> {*} }
token multi_declarator:only  { <sym> <.ws> <declarator> {*} }

token routine_declarator:sub       { <sym> <routine_def> {*} }
token routine_declarator:method    { <sym> <method_def> {*} }
token routine_declarator:submethod { <sym> <method_def> {*} }
token routine_declarator:macro     { <sym> <macro_def> {*} }

token regex_declarator:regex { <sym>       <regex_def> {*} }
token regex_declarator:token { <sym>       <regex_def> {*} }
token regex_declarator:rule  { <sym>       <regex_def> {*} }

# Most of these special variable rules are there simply to catch old p5 brainos

token special_variable:sym<$¢> { <sym> {*} }

token special_variable:sym<$!> { <sym> <!before \w> {*} }

token special_variable:sym<$!{ }> {
    # XXX the backslashes are necessary here for bootstrapping, not for P6...
    ( '$!{' :: (.*?) '}' )
    <obs("$0 variable", 'smart match against $!')>
}

token special_variable:sym<$/> {
    <sym>
    # XXX assuming nobody ever wants to assign $/ directly anymore...
    [ <?before \h* '=' <![=]> >
        <obs('$/ variable as input record separator',
             "filehandle's :irs attribute")>
    ]?
    {*}
}

token special_variable:sym<$~> {
    <sym> :: <?before \s | ',' | '=' | <terminator> >
    <obs('$~ variable', 'Form module')>
}

token special_variable:sym<$`> {
    <sym> :: <?before \s | ',' | <terminator> >
    <obs('$` variable', 'explicit pattern before <(')>
}

token special_variable:sym<$@> {
    <sym> ::
    <obs('$@ variable as eval error', '$!')>
}

token special_variable:sym<$#> {
    <sym> ::
    [
    || (\w+) <obs("\$#$0 variable", "@{$0}.end")>
    || <obs('$# variable', '.fmt')>
    ]
}
token special_variable:sym<$$> {
    <sym> :: <?before \s | ',' | <terminator> >
    <obs('$$ variable', '$*PID')>
}
token special_variable:sym<$%> {
    <sym> ::
    <obs('$% variable', 'Form module')>
}

# Note: this works because placeholders are restricted to lowercase
token special_variable:sym<$^X> {
    ( <sigil> '^' (<[A..Z]>) \W )
    <obscaret($0, $<sigil>, $1)>
}

token special_variable:sym<$^> {
    <sym> :: <?before \s | ',' | '=' | <terminator> >
    <obs('$^ variable', 'Form module')>
}

token special_variable:sym<$&> {
    <sym> :: <?before \s | ',' | <terminator> >
    <obs('$& variable', '$/ or $()')>
}

token special_variable:sym<$*> {
    <sym> :: <?before \s | ',' | '=' | <terminator> >
    <obs('$* variable', '^^ and $$')>
}

token special_variable:sym<$)> {
    <sym> :: <?before \s | ',' | <terminator> >
    <obs('$) variable', '$*EGID')>
}

token special_variable:sym<$-> {
    <sym> :: <?before \s | ',' | '=' | <terminator> >
    <obs('$- variable', 'Form module')>
}

token special_variable:sym<$=> {
    <sym> :: <?before \s | ',' | '=' | <terminator> >
    <obs('$= variable', 'Form module')>
}

token special_variable:sym<@+> {
    <sym> :: <?before \s | ',' | <terminator> >
    <obs('@+ variable', '.to method')>
}

token special_variable:sym<%+> {
    <sym> :: <?before \s | ',' | <terminator> >
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
    <sym> :: <?before \s | ',' | <terminator> >
    <obs('@- variable', '.from method')>
}

token special_variable:sym<%-> {
    <sym> :: <?before \s | ',' | <terminator> >
    <obs('%- variable', '.from method')>
}

token special_variable:sym<$-[ ]> {
    '$-['
    <obs('@- variable', '.from method')>
}

token special_variable:sym<@-[ ]> {
    '@-['
    <obs('@- variable', '.from method')>
}

token special_variable:sym<%-{ }> {
    '@-{'
    <obs('%- variable', '.from method')>
}

token special_variable:sym<$+> {
    <sym> :: <?before \s | ',' | <terminator> >
    <obs('$+ variable', 'Form module')>
}

token special_variable:sym<${^ }> {
    ( <sigil> '{^' :: (.*?) '}' )
    <obscaret($0, $<sigil>, $1)>
}

# XXX should eventually rely on multi instead of nested cases here...
method obscaret (Str $var, Str $sigil, Str $name) {
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
    return self.obs("$var variable", $repl);
}

token special_variable:sym<${ }> {
    ( <sigil> '{' :: (.*?) '}' )
    <obs("$0 variable", "{$<sigil>}($1)")>
}

token special_variable:sym<$[> {
    <sym> :: <?before \s | ',' | '=' | <terminator> >
    <obs('$[ variable', 'user-defined array indices')>
}

token special_variable:sym<$]> {
    <sym> :: <?before \s | ',' | <terminator> >
    <obs('$] variable', '$*PERL_VERSION')>
}

token special_variable:sym<$\\> {
    <sym> :: <?before \s | ',' | '=' | <terminator> >
    <obs('$\\ variable', "the filehandle's :ors attribute")>
}

token special_variable:sym<$|> {
    <sym> :: <?before \s | ',' | '=' | <terminator> >
    <obs('$| variable', 'Form module')>
}

token special_variable:sym<$:> {
    <sym> :: <?before \s | ',' | '=' | <terminator> >
    <obs('$: variable', 'Form module')>
}

token special_variable:sym<$;> {
    <sym> :: <?before \s | ',' | '=' | <terminator> >
    <obs('$; variable', 'real multidimensional hashes')>
}

token special_variable:sym<$'> { #'
    <sym> :: <?before \s | ',' | <terminator> >
    <obs('$' ~ "'" ~ 'variable', "explicit pattern after )\x3E")>
}

token special_variable:sym<$"> {
    <sym> :: <?before \s | ',' | '=' | <terminator> >
    <obs('$" variable', '.join() method')>
}

token special_variable:sym<$,> {
    <sym> :: <?before \s | ',' | <terminator> >
    <obs(q/$, variable/, ".join() method")>
}

token special_variable:sym['$<'] {
    <sym> :: <!before \s* \w+ \s* '>' >
    <obs('$< variable', '$*UID')>
}

token special_variable:sym«$>» {
    <sym> :: <?before \s | ',' | <terminator> >
    <obs("$() variable", '$*EUID')>
}

token special_variable:sym<$.> {
    <sym> :: <?before \s | ',' | <terminator> >
    <obs(q/$. variable/, "filehandle's .line method")>
}

token special_variable:sym<$?> {
    <sym> :: <?before \s | ',' | <terminator> >
    <obs('$? variable as child error', '$!')>
}

# desigilname should only follow a sigil/twigil

token desigilname {
    [
    | <?before '$' > <variable>
    | <name>
    ]
    {*}
}

token variable {
    <?before <sigil> > ::
    [
    | <sigil> <twigil>?
        [
        || <?{ $<sigil> eq '&' }> ::
            <sublongname> {*}                                   #= subnoun
        || <desigilname> {*}                                    #= desigilname
        ]
        [ <?{ $<twigil> eq '.' }>
            <.unsp>? <?before '('> <postcircumfix> {*}          #= methcall
        ]?
    | <special_variable> {*}                                    #= special
    | <sigil> \d+ {*}                                           #= $0
    # Note: $() can also parse as contextualizer in an expression; should have same effect
    | <sigil> <?before '<' | '('> <postcircumfix> {*}           #= $()
    ]
    {*}
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
token twigil:sym<:> { <sym> <!before ':'> }
token twigil:sym<*> { <sym> }
token twigil:sym<+> { <sym> }
token twigil:sym<?> { <sym> }
token twigil:sym<=> { <sym> }

token longname {
    <name> <colonpair>*
}

token name {
    [
    | <ident> <morename>*
    | <morename>+
    ]
    {*}
}

token morename {
    '::'
    <?before <alpha> | '(' > ::
    [
    | <ident>
    | '(' <EXPR> [ ')' || <.panic: "Missing right parenthesis"> ]
    ]
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

#token subcall {
#    # XXX should this be sublongname?
#    <subshortname> <.unsp>? '.'? '(' <semilist> ')'
#    {*}
#}

#token packagevar {
#    # Note: any ::() are handled within <name>, and subscript must be final part.
#    # A bare ::($foo) is not considered a variable, but ::($foo)::<$bar> is.
#    # (The point being that we want a sigil either first or last but not both.)
#    <?before [\w+] ** '::' [ '<' | '«' | '{' ]> ::
#    <name> '::' <postcircumfix> {*}                            #= FOO::<$x>
#}

token value {
    [
    | <quote>
    | <number>
    | <version>
#    | <packagevar>     # XXX see term:name for now
#    | <fulltypename>   # XXX see term:name for now
    ]
    {*}
}

token typename {
    <name>
    <?{
        $¢.is_type($<name>.text)
    }>
    # parametric type?
    <.unsp>? [ <?before '['> <postcircumfix> ]?
    {*}
}

rule fulltypename {<typename>
    [ of <fulltypename> ]?
    {*}
}

token number {
    [
    | <dec_number>
    | <integer>
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
            {{ START { worry("Leading 0 does not indicate octal in Perl 6") } }}
        ]
    | \d+[_\d+]*
    ]
    {*}
}

token radint {
    [
    | <integer>
    | <?before ':'> <rad_number> <?{
                        defined $<rad_number><intpart>
                        and
                        not defined $<rad_number><fracpart>
                    }>
    ]
    {*}
}

token dec_number {
    \d+[_\d+]*
        [
        | '.' \d+[_\d+]* [ <[Ee]> <[+\-]>? \d+ ]?
        |                  <[Ee]> <[+\-]>? \d+
        ] # careful to distinguish from both integer and 42.method
    {*}
}

token rad_number {
    ':' $<radix> = [\d+] <.unsp>?      # XXX optional dot here?
    ::           # don't recurse in lexer
    [
    || '<'
            $<intpart> = <[ 0..9 a..z A..Z ]>+ [ _ <[ 0..9 a..z A..Z ]>+ ]*
            $<fracpart> = [ '.' <[ 0..9 a..z A..Z ]>+ [ _ <[ 0..9 a..z A..Z ]>+ ]* ]?
            [ '*' <base=radint> '**' <exp=radint> ]?
       '>'
#      { make radcalc($<radix>, $<intpart>, $<fracpart>, $<base>, $<exp>) }
    || <?before '['> <postcircumfix>
    || <?before '('> <postcircumfix>
    ]
    {*}
}

token octint {
    <[ 0..7 ]>+ [ _ <[ 0..7 ]>+ ]*
}

token hexint {
    <[ 0..9 a..f A..F ]>+ [ _ <[ 0..9 a..f A..F ]>+ ]*
}

our @herestub_queue;

token q_herestub ($lang) {
    $<delimstr> = <quibble('Perl::Q')>  # force raw semantics on /END/ marker
    {
        push @herestub_queue,
            Herestub.new(
                delim => $<delimstr><delimited><q><text>, # XXX or some such
                orignode => $_,
                lang => $lang,
            );
    }
    {*}
}

class Herestub {
    has Str $.delim;
    has $.orignode;
    has $.lang;
} # end class

token theredoc {
    ^^ $<ws>=(\h*?) $+DELIM \h* $$ \n?
}

# XXX be sure to temporize @herestub_queue on reentry to new line of heredocs

method heredoc () {
    return if self.peek;
    my $here = self;
    while my $herestub = shift @herestub_queue {
        my $DELIM is context = $herestub.delim;
        my $lang = $herestub.lang;
        my $doc;
        my $ws = "";
        $here = $here.q_unbalanced_rule($lang, :stop(&theredoc)).MATCHIFY;
        if $here {
            if $ws {
                my $wsequiv = $ws;
                $wsequiv ~~ s/^ (\t+) /{ ' ' x ($0 * 8) }/; # per spec
                $here<text>[0] ~~ s/^/\n/; # so we don't match ^^ after escapes
                for @($here<text>) {
                    s:g[\n ($ws || \h*)] = do {
                        my $white = $0;
                        if $white eq $ws {
                            '';
                        }
                        else {
                            $white ~~ s[^ (\t+) ] = do {
                                ' ' x ($0.chars * (COMPILING::<$?TABSTOP> // 8))
                            };
                            $white ~~ s/^ $wsequiv //
                                ?? $white
                                !! '';
                        }
                    }
                }
                $here<text>[0] ~~ s/^ \n //;
            }
            $herestub.orignode<doc> = $here;
        }
        else {
            self.panic("Ending delimiter $DELIM not found");
        }
    }
    return $here;
}

token quibble ($lang) {
    :my ($start,$stop) = self.peek_delimiters();
    :my $sublang = $start ne $stop ?? $lang.balanced($start,$stop)
                                   !! $lang.unbalanced($stop);
    $start <nibble($sublang)> $stop
}

method nibble ($lang) {
    my $outerlang = self.WHAT;
    my $LANG is context = $outerlang;
    self.cursor_fresh($lang).nibbler;
}

method regex ($lang) {
    my $outerlang = self.WHAT;
    my $LANG is context = $outerlang;
    self.cursor_fresh($lang).regex;
}

token quote:sym<' '>   { "'" <nibble(Perl::Q.tweak(:q).unbalanced("'"))> "'" }
token quote:sym<" ">   { '"' <nibble(Perl::Q.tweak(:qq).unbalanced('"'))> '"' }

token quote:sym<« »>   { '«' <nibble(Perl::Q.tweak(:qq).tweak(:ww).balanced('«','»'))> '»' }
token quote:sym«<< >>» { '<<' <nibble(Perl::Q.tweak(:qq).tweak(:ww).balanced('<<','>>'))> '>>' }
token quote:sym«< >»   { '<' <nibble(Perl::Q.tweak(:q).tweak(:w).balanced('<','>'))> '>' }

token quote:sym</ />   {
    '/' <regex( ::Regex.unbalanced("/") )> '/'
    [ (< i g s m x c e ] >+) 
        # note: inner failure of obs caught by ? so we report all suggestions
        [ $0 ~~ 'i' <obs('/i',':i')> ]?
        [ $0 ~~ 'g' <obs('/g',':g')> ]?
        [ $0 ~~ 's' <obs('/s','^^ and $$ anchors')> ]?
        [ $0 ~~ 'm' <obs('/m','. or \N')> ]?
        [ $0 ~~ 'x' <obs('/x','normal default whitespace')> ]?
        [ $0 ~~ 'c' <obs('/c',':c or :p')> ]?
        [ $0 ~~ 'e' <obs('/e','interpolated {...} or s{} = ... form')> ]?
        <obs('suffix regex modifiers','prefix adverbs')>
    ]?
}

# handle composite forms like qww
token quote:qq {
    'qq' <quote_mod>? »
    <quibble(Perl::Q.tweak(:qq).tweak($<quote_mod>))>
}
token quote:q {
    'q' <quote_mod>? »
    <quibble(Perl::Q.tweak(:q).tweak($<quote_mod>))>
}

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

token quote:rx { <sym> » <quibble( ::Regex )> }

token quote:m  { <sym> » <quibble( ::Regex )> }
token quote:mm { <sym> » <quibble( ::Regex.tweak(:s))> }

token quote:s {
    <sym> » <pat=quibble( ::Regex )>
    <finish_subst($<pat>)>
}
token quote:ss {
    <sym> » <pat=quibble( ::Regex.tweak(:s))>
    <finish_subst($<pat>)>
}
token quote:tr {
    <sym> » <pat=quibble( ::Trans )>
    <finish_trans($<pat>)>
}

#token finish_subst ($pat) {
#    [
#    # bracketed form
#    | <?{ $pat<delim> == 2 }> ::
#          <.ws>
#          <infix>            # looking for pseudoassign here
#          { $<infix><O><prec> == %item_assignment<prec> or
#              $¢.panic("Bracketed subst must use some form of assignment") }
#          <repl=EXPR(item %item_assignment)>
#    # unbracketed form
#    | <repl=q_unbalanced(qlang('Q',':qq'), $pat<delim>[0])>
#    ]
#}
#
#token finish_trans ($pat) {
#    [
#    # bracketed form
#    | <?{ $pat<delim> == 2 }> ::
#          <.ws>
#          <repl=q_pickdelim(qlang('Q',':tr'))>
#    # unbracketed form
#    | <repl=q_unbalanced(qlang('Q',':tr'), $pat<delim>[0])>
#    ]
#}

# XXX should eventually be derived from current Unicode tables.
constant %open2close = (
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
);

token opener {
  <[\x0028 \x003C \x005B
    \x007B \x00AB \x0F3A
    \x0F3C \x169B \x2039
    \x2045 \x207D \x208D
    \x2208 \x2209 \x220A
    \x2215 \x223C \x2243
    \x2252 \x2254 \x2264
    \x2266 \x2268 \x226A
    \x226E \x2270 \x2272
    \x2274 \x2276 \x2278
    \x227A \x227C \x227E
    \x2280 \x2282 \x2284
    \x2286 \x2288 \x228A
    \x228F \x2291 \x2298
    \x22A2 \x22A6 \x22A8
    \x22A9 \x22AB \x22B0
    \x22B2 \x22B4 \x22B6
    \x22C9 \x22CB \x22D0
    \x22D6 \x22D8 \x22DA
    \x22DC \x22DE \x22E0
    \x22E2 \x22E4 \x22E6
    \x22E8 \x22EA \x22EC
    \x22F0 \x22F2 \x22F3
    \x22F4 \x22F6 \x22F7
    \x2308 \x230A \x2329
    \x23B4 \x2768 \x276A
    \x276C \x276E \x2770
    \x2772 \x2774 \x27C3
    \x27C5 \x27D5 \x27DD
    \x27E2 \x27E4 \x27E6
    \x27E8 \x27EA \x2983
    \x2985 \x2987 \x2989
    \x298B \x298D \x298F
    \x2991 \x2993 \x2995
    \x2997 \x29C0 \x29C4
    \x29CF \x29D1 \x29D4
    \x29D8 \x29DA \x29F8
    \x29FC \x2A2B \x2A2D
    \x2A34 \x2A3C \x2A64
    \x2A79 \x2A7D \x2A7F
    \x2A81 \x2A83 \x2A8B
    \x2A91 \x2A93 \x2A95
    \x2A97 \x2A99 \x2A9B
    \x2AA1 \x2AA6 \x2AA8
    \x2AAA \x2AAC \x2AAF
    \x2AB3 \x2ABB \x2ABD
    \x2ABF \x2AC1 \x2AC3
    \x2AC5 \x2ACD \x2ACF
    \x2AD1 \x2AD3 \x2AD5
    \x2AEC \x2AF7 \x2AF9
    \x2E02 \x2E04 \x2E09
    \x2E0C \x2E1C \x3008
    \x300A \x300C \x300E
    \x3010 \x3014 \x3016
    \x3018 \x301A \x301D
    \xFD3E \xFE17 \xFE35
    \xFE37 \xFE39 \xFE3B
    \xFE3D \xFE3F \xFE41
    \xFE43 \xFE47 \xFE59
    \xFE5B \xFE5D \xFF08
    \xFF1C \xFF3B \xFF5B
    \xFF5F \xFF62]>
}

# assumes whitespace is eaten already

method peek_delimiters {
    my $buf = self.orig;
    my $pos = self.pos;
    my $startpos = $pos;
    my $char = substr($$buf,$pos++,1);
    if $char ~~ /^\s$/ {
	self.panic("Whitespace not allowed as delimiter");
    }

# XXX not defined yet
#    <?before <+isPe> > {
#        self.panic("Use a closing delimiter for an opener is reserved");
#    }

    my $rightbrack = %open2close{$char};
    if not defined $rightbrack {
	return $char, $char;
    }
    while substr($$buf,$pos,1) eq $char {
	$pos++;
    }
    my $len = $pos - $startpos;
    my $start = $char x $len;
    my $stop = $rightbrack x $len;
    return $start, $stop;
}

role startstop[$start,$stop] {
    token starter { $start }
    token stopper { $stop }
} # end role

role stop[$stop] {
    token starter { <!> }
    token stopper { $stop }
} # end role

role unitstop[$stop] {
    token unitstopper { $stop }
} # end role

token unitstopper { $ }

method balanced ($start,$stop) { self.mixin( ::startstop[$start,$stop] ); }
method unbalanced ($stop) { self.mixin( ::stop[$stop] ); }
method unitstop ($stop) { self.mixin( ::unitstop[$stop] ); }

token codepoint {
    '[' :: ( [<!before ']'> .]*? ) ']'
}

grammar Q is Perl {
    proto token backslash {}
    proto token escape {}
    token starter { <!> }

    role b {
        token escape:sym<\\> { <sym> <item=backslash> }
        token backslash:qq { <?before 'q'> { $<quote> = $+LANG.quote(); } }
        token backslash:sym<\\> { <text=sym> }
        token backslash:stopper { <text=stopper> }
        token backslash:a { <sym> }
        token backslash:b { <sym> }
        token backslash:c { <sym>
            [
            || '[' <-[ \] \v ]>* ']'
            || <codepoint>
            ]
        }
        token backslash:e { <sym> }
        token backslash:f { <sym> }
        token backslash:n { <sym> }
        token backslash:o { <sym> [ <octint> | '['<octint>[','<octint>]*']' ] }
        token backslash:r { <sym> }
        token backslash:t { <sym> }
        token backslash:x { <sym> [ <hexint> | '['<hexint>[','<hexint>]*']' ] }
        token backslash:sym<0> { <sym> }
    } # end role

    role _b {
        token escape:sym<\\> { <!> }
    } # end role

    role c {
        token escape:sym<{ }> { <?before '{'> <block> }
    } # end role

    role _c {
        token escape:sym<{ }> { <!> }
    } # end role

    role s {
        token escape:sym<$> { <?before '$'> <variable> <extrapost>? }
    } # end role

    role _s {
        token escape:sym<$> { <!> }
    } # end role

    role a {
        token escape:sym<@> { <?before '@'> <variable> <extrapost> }
    } # end role

    role _a {
        token escape:sym<@> { <!> }
    } # end role

    role h {
        token escape:sym<%> { <?before '%'> <variable> <extrapost> }
    } # end role

    role _h {
        token escape:sym<%> { <!> }
    } # end role

    role f {
        token escape:sym<&> { <?before '&'> <variable> <extrapost> }
    } # end role

    role _f {
        token escape:sym<&> { <!> }
    } # end role

    role w {
        token stopper { '>' } # wrong
        method postprocess ($s) { $s.comb }
    } # end role

    role _w {
        method postprocess ($s) { $s }
    } # end role

    role q {
        token stopper { \' }

        token escape:sym<\\> { <sym> <item=backslash> }

        token backslash:qq { <?before 'q'> { $<quote> = $+LANG.quote(); } }
        token backslash:sym<\\> { <text=sym> }
        token backslash:stopper { <text=stopper> }

        # in single quotes, keep backslash on random character by default
        token backslash:misc { :: (.) { $<text> = "\\$0"; } }

        # begin tweaks (DO NOT ERASE)
        multi method tweak (:single(:$q)) { self.panic("Too late for :q") }
        multi method tweak (:double(:$qq)) { self.panic("Too late for :qq") }
        multi method tweak (*%x) { self.HOW.find_next_method_by_name('tweak').(self,%x) }
        # end tweaks (DO NOT ERASE)

    } # end role

    role qq does b does c does s does a does h does f {
        token stopper { \" }
        # in double quotes, omit backslash on random \W backslash by default
        token backslash:misc { :: [ (\W) { $<text> = "$0"; } | (\w) <.panic: "unrecognized backslash sequence: '\\$0'"> ] }

        # begin tweaks (DO NOT ERASE)
        multi method tweak (:single(:$q)) { self.panic("Too late for :q") }
        multi method tweak (:double(:$qq)) { self.panic("Too late for :qq") }
        multi method tweak (*%x) { self.HOW.find_next_method_by_name('tweak').(self,%x) }
        # end tweaks (DO NOT ERASE)

    } # end role

    # note: polymorphic over many quote languages, we hope
    token nibbler {
	:my $text = '';
        :my @nibbles = ();
        :my $buf = self.orig;
        [
            [
            | <?before <stopper> > :: <fail>
            | <0=starter> :: <nibbler> <1=stopper>
                            {
                                my @n = $<nibbler><nibbles>.list;
                                $text ~= $0 ~ shift(@n);
                                $text = (@n ?? pop(@n) !! '') ~ $1;
                                push @nibbles, @n;
                            }
            | <escape>   :: {
                                push @nibbles, $text, $<escape>;
                                $text = '';
                            }
            |            :: <!stopper> .
                            {
                                $text ~= substr($$buf, $¢.pos-1, 1);
                            }
            ]
        ]*
        { push @nibbles, $text; $<nibbles> = [@nibbles]; }
        {*}
    }

    # begin tweaks (DO NOT ERASE)

    multi method tweak (:single(:$q)) { self.mixin( ::q ); }

    multi method tweak (:double(:$qq)) { self.mixin( ::qq ); }

    multi method tweak (:backslash(:$b))   { self.mixin($b ?? ::b !! ::_b) }
    multi method tweak (:scalar(:$s))      { self.mixin($s ?? ::s !! ::_s) }
    multi method tweak (:array(:$a))       { self.mixin($a ?? ::a !! ::_a) }
    multi method tweak (:hash(:$h))        { self.mixin($h ?? ::h !! ::_h) }
    multi method tweak (:function(:$f))    { self.mixin($f ?? ::f !! ::_f) }
    multi method tweak (:closure(:$c))     { self.mixin($c ?? ::c !! ::_c) }

    multi method tweak (:exec(:$x))        { self.mixin($x ?? ::x !! ::_x) }
    multi method tweak (:words(:$w))       { self.mixin($w ?? ::w !! ::_w) }
    multi method tweak (:quotewords(:$ww)) { self.mixin($ww ?? ::ww !! ::_ww) }

    multi method tweak (:heredoc(:$to)) {
        # $.parser = &Perl::q_heredoc;
        # %.option<to> = $to;
    }

    multi method tweak (:$regex) {
        return ::Regex;
    }

    multi method tweak (:$trans) {
        return ::Trans;
    }

    multi method tweak (:$code) {
        return ::RegexCode;
    }

    multi method tweak (*%x) {
        my @k = keys(%x);
        Perl.panic("Unrecognized quote modifier: " ~ @k);
    }
    # end tweaks (DO NOT ERASE)


} # end grammar

#token q_unbalanced_rule ($lang, $stop, :@esc = $lang.escset) {
#    $<text> = [ [ [ <?before @esc> <escape=q_escape($lang)> | <!$stop>. ] ]*? ]
#    <stop=$stop>
#    {*}
#}
#
#token q_unbalanced ($lang, $stop, :@esc = $lang.escset) {
#    $stop
#    $<text> = [ [ [ <?before @esc> <escape=q_escape($lang)> | <!before $stop >. ] ]*? ]
#    $stop
#    {*}
#}
#
## We get here only for escapes in escape set, even though more are defined.
#method q_escape ($lang) {
#    $lang<escrule>(self);
#}
#
#token quote_escapes {
#    [
#    || \\ <qq_backslash>
#    || <?before '{'> <block>
#    || <?before '$'> <variable> <extrapost>?
#    || <variable> <extrapost>
#    || .
#    ]
#    {*}
#}

# Note, backtracks!  So post mustn't commit to anything permanent.
regex extrapost {
    :my $inquote is context = 1;
    <post>*
    # XXX Shouldn't need a backslash on anything but the right square here
    <?after <[ \] } > ) ]> > 
    {*}
}

rule multisig {
    [
	':'?'(' <signature> [ ')' || <.panic: "Missing right parenthesis"> ]
    ]
    ** '|'
}

rule routine_def {
    <longname>?  <multisig>?
    <trait>*
    <block>
    {*}
}

rule method_def {
    [
    | <longname>  <multisig>?
    | <?before <sigil> '.' [ '[' | '{' | '(' ] > <sigil> <postcircumfix>
    ]
    <trait>*
    <block>
    {*}
}

rule regex_def {
    <longname>?
    <trait>*
    [ ':'?'(' <signature> ')']?
    <regex_block>
    {*}
}

# XXX redundant with routine_def?
rule macro_def {
    | <longname>?  <multisig>?
    <trait>*
    <block>
    {*}
}

rule trait {
    [
    | <trait_verb>
    | <trait_auxiliary>
    ]
}

rule trait_auxiliary:is   {<sym> <name><postcircumfix>? }
rule trait_auxiliary:does {<sym> <role_name> }
rule trait_auxiliary:will {<sym> <ident> <block> }

rule trait_verb:of      {<sym> <fulltypename> }
rule trait_verb:returns {<sym> <fulltypename> }
rule trait_verb:handles {<sym> <EXPR> }

token capterm {
    '\\(' <capture> ')'
    {*}
}

rule capture {
    <EXPR>
    {*}
}

token sigterm {
    ':(' <signature> [ ')' || <.panic: "Missing right parenthesis"> ]
    {*}
}

rule param_sep { [','|':'|';'|';;'] }

token signature {
    :my $zone is context<rw> = 'posreq';
    <.ws>
    [
    | <?before '-->' | ')' | '{' >
    | <parameter>
    ] ** <param_sep>
    <.ws>
    [ '-->' <.ws> <fulltypename> ]?
    {*}
}

rule type_declarator:subset {\
    <sym>
    <name>
    [ of <fulltypename> ]?
    where <EXPR>
    {*}
}

rule type_constraint {
    [
    | <value>
    | <fulltypename>
    | where <EXPR(item %chaining)>
    ]
    {*}
}

rule post_constraint {
    [
    | <multisig>
    | where <EXPR(item %chaining)>
    ]
    {*}
}

token param_var {
    <sigil> <twigil>?
    [
        # Is it a longname declaration?
    || <?{ $<sigil> eq '&' }> <?ident> ::
        <ident=sublongname>

    ||  # Is it a shaped array or hash declaration?
        <?{ $<sigil> eq '@' || $<sigil> eq '%' }>
        <ident>?
        <.ws>
        <?before <[ \< \( \[ \{ ]> >
        <postcircumfix>

        # ordinary parameter name
    || <ident>

        # bare sigil?
    ]?
}

token parameter {
    :my $quant;
    <type_constraint>*
    [
    | $<slurp> = [ $<quantchar>=[ '*' ] <param_var> ]
        { let $quant := '*' }
    |   [ $<named> =
            [ $<quantchar> = [ ':' ]
                [
                | <name=ident> '(' <param_var>  ')'
                | <param_var> { $<name> := $<param_var><ident> }
                ]
                { let $quant = '*' }
            ]
        | <param_var>
            { let $quant := '!'; }
        ]
        [ $<quantchar> = <[ ? ! ]> { let $quant := $<quantchar> } ]?
    ]
    <trait>*

    <post_constraint>*

    [
        <default_value> {{
            given $<quantchar> {
              when '!' { $¢.panic("Can't put a default on a required parameter") }
              when '*' { $¢.panic("Can't put a default on a slurpy parameter") }
            }
            let $quant := '?';
        }}
    ]?

    # enforce zone constraints
    {{
        given $quant {
            when '!' {
                given $+zone {
                    when 'posopt' {
$¢.panic("Can't use required parameter in optional zone");
                    }
                    when 'var' {
$¢.panic("Can't use required parameter in variadic zone");
                    }
                }
            }
            when '?' {
                given $+zone {
                    when 'posreq' { $+zone = 'posopt' }
                    when 'var' {
$¢.panic("Can't use optional positional parameter in variadic zone");
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
    '=' <EXPR(item %item_assignment)>
}

token statement_prefix:do      { <sym> <.ws> <statement> {*} }
token statement_prefix:try     { <sym> <.ws> <statement> {*} }
token statement_prefix:gather  { <sym> <.ws> <statement> {*} }
token statement_prefix:contend { <sym> <.ws> <statement> {*} }
token statement_prefix:async   { <sym> <.ws> <statement> {*} }
token statement_prefix:lazy    { <sym> <.ws> <statement> {*} }

## term
token term:sym<undef> ( --> Term) {
    <sym> » \h*
    [ <?before '$/' >
        <obs('$/ variable as input record separator',
             "the filehandle's .slurp method")>
    ]?
    [ <?before < $ @ % & > >
        <obs('undef as a verb', 'undefine function')>
    ]?
    {*}
}

token term:sym<self> ( --> Term)
    { <sym> » {*} }

token term:rand ( --> Named_unary)
    { <sym> » {*} }

token term:sym<*> ( --> Term)
    { <sym> {*} }

token circumfix:sigil ( --> Term)
    { <sigil> '(' <semilist> ')' {*} }

#token circumfix:typecast ( --> Term)
#    { <typename> '(' <semilist> ')' {*} }

token circumfix:sym<( )> ( --> Term)
    { '(' <semilist> ')'  {*} }

token circumfix:sym<[ ]> ( --> Term)
    { '[' <semilist> ']' {*} }

## methodcall

token infix:sym<.> ()
    { '.' <obs('. to concatenate strings', '~')> }

token postfix:sym['->'] ()
    { '->' <obs('-> to call a method', '.')> }

## autoincrement
token postfix:sym<++> ( --> Autoincrement)
    { <sym> {*} }

token postfix:sym<--> ( --> Autoincrement)
    { <sym> {*} }

token prefix:sym<++> ( --> Autoincrement)
    { <sym> {*} }

token prefix:sym<--> ( --> Autoincrement)
    { <sym> {*} }

token postfix:sym<i> ( --> Autoincrement)
    { <sym> » {*} }

## exponentiation
token infix:sym<**> ( --> Exponentiation)
    { <sym> {*} }

## symbolic unary
token prefix:sym<!> ( --> Symbolic_unary)
    { <sym> {*} }

token prefix:sym<+> ( --> Symbolic_unary)
    { <sym> {*} }

token prefix:sym<-> ( --> Symbolic_unary)
    { <sym> {*} }

token prefix:sym<~> ( --> Symbolic_unary)
    { <sym> {*} }

token prefix:sym<?> ( --> Symbolic_unary)
    { <sym> {*} }

token prefix:sym<=> ( --> Symbolic_unary)
    { <sym> {*} }

token prefix:sym<*> ( --> Symbolic_unary)
    { <sym> {*} }

token prefix:sym<**> ( --> Symbolic_unary)
    { <sym> {*} }

token prefix:sym<~^> ( --> Symbolic_unary)
    { <sym> {*} }

token prefix:sym<+^> ( --> Symbolic_unary)
    { <sym> {*} }

token prefix:sym<?^> ( --> Symbolic_unary)
    { <sym> {*} }

token prefix:sym<^> ( --> Symbolic_unary)
    { <sym> {*} }

token prefix:sym<|> ( --> Symbolic_unary)
    { <sym> {*} }


## multiplicative
token infix:sym<*> ( --> Multiplicative)
    { <sym> {*} }

token infix:sym</> ( --> Multiplicative)
    { <sym> {*} }

token infix:sym<%> ( --> Multiplicative)
    { <sym> {*} }

token infix:sym<+&> ( --> Multiplicative)
    { <sym> {*} }

token infix:sym« +< » ( --> Multiplicative)
    { <sym> {*} }

token infix:sym« << » ( --> Multiplicative)
    { <sym> <obs('<< to do left shift', '+< or ~<')> }

token infix:sym« >> » ( --> Multiplicative)
    { <sym> <obs('>> to do right shift', '+> or ~>')> }

token infix:sym« +> » ( --> Multiplicative)
    { <sym> {*} }

token infix:sym<~&> ( --> Multiplicative)
    { <sym> {*} }

token infix:sym« ~< » ( --> Multiplicative)
    { <sym> {*} }

token infix:sym« ~> » ( --> Multiplicative)
    { <sym> {*} }


## additive
token infix:sym<+> ( --> Additive)
    { <sym> {*} }

token infix:sym<-> ( --> Additive)
    { <sym> {*} }

token infix:sym<+|> ( --> Additive)
    { <sym> {*} }

token infix:sym<+^> ( --> Additive)
    { <sym> {*} }

token infix:sym<~|> ( --> Additive)
    { <sym> {*} }

token infix:sym<~^> ( --> Additive)
    { <sym> {*} }

token infix:sym<?|> ( --> Additive)
    { <sym> {*} }

token infix:sym<?^> ( --> Additive)
    { <sym> {*} }

## replication
# Note: no word boundary check after x, relies on longest token for x2 xx2 etc
token infix:sym<x> ( --> Replication)
    { <sym> » {*} }

token infix:sym<xx> ( --> Replication)
    { <sym> » {*} }

## concatenation
token infix:sym<~> ( --> Concatenation)
    { <sym> {*} }


## junctive and (all)
token infix:sym<&> ( --> Junctive_and)
    { <sym> {*} }


## junctive or (any)
token infix:sym<|> ( --> Junctive_or)
    { <sym> {*} }

token infix:sym<^> ( --> Junctive_or)
    { <sym> {*} }


## named unary examples
token prefix:sleep ( --> Named_unary)
    { <sym> » {*} }

token prefix:abs ( --> Named_unary)
    { <sym> » {*} }

## nonchaining binary
token infix:sym« <=> » ( --> Nonchaining)
    { <sym> {*} }

token infix:cmp ( --> Nonchaining)
    { <sym> » {*} }

token infix:is ( --> Nonchaining)
    { <sym> » {*} }

token infix:but ( --> Nonchaining)
    { <sym> » {*} }

token infix:does ( --> Nonchaining)
    { <sym> » {*} }

token infix:sym<..> ( --> Nonchaining)
    { <sym> {*} }

token infix:sym<^..> ( --> Nonchaining)
    { <sym> {*} }

token infix:sym<..^> ( --> Nonchaining)
    { <sym> {*} }

token infix:sym<^..^> ( --> Nonchaining)
    { <sym> {*} }

token infix:sym<ff> ( --> Nonchaining)
    { <sym> » {*} }

token infix:sym<^ff> ( --> Nonchaining)
    { <sym> » {*} }

token infix:sym<ff^> ( --> Nonchaining)
    { <sym> {*} }

token infix:sym<^ff^> ( --> Nonchaining)
    { <sym> {*} }

token infix:sym<fff> ( --> Nonchaining)
    { <sym> » {*} }

token infix:sym<^fff> ( --> Nonchaining)
    { <sym> » {*} }

token infix:sym<fff^> ( --> Nonchaining)
    { <sym> {*} }

token infix:sym<^fff^> ( --> Nonchaining)
    { <sym> {*} }


## chaining binary
token infix:sym<==> ( --> Chaining)
    { <sym> {*} }

token infix:sym<!=> ( --> Chaining)
    { <sym> {*} }

token infix:sym« < » ( --> Chaining)
    { <sym> {*} }

token infix:sym« <= » ( --> Chaining)
    { <sym> {*} }

token infix:sym« > » ( --> Chaining)
    { <sym> {*} }

token infix:sym« >= » ( --> Chaining)
    { <sym> {*} }

token infix:sym<~~> ( --> Chaining)
    { <sym> {*} }

token infix:sym<!~> ( --> Chaining)
    { <sym> <obs('!~ to do negated pattern matching', '!~~')> }

token infix:sym<=~> ( --> Chaining)
    { <sym> <obs('=~ to do pattern matching', '~~')> }

token infix:sym<eq> ( --> Chaining)
    { <sym> » {*} }

token infix:sym<ne> ( --> Chaining)
    { <sym> » {*} }

token infix:sym<lt> ( --> Chaining)
    { <sym> » {*} }

token infix:sym<le> ( --> Chaining)
    { <sym> » {*} }

token infix:sym<gt> ( --> Chaining)
    { <sym> » {*} }

token infix:sym<ge> ( --> Chaining)
    { <sym> » {*} }

token infix:sym<=:=> ( --> Chaining)
    { <sym> {*} }

token infix:sym<===> ( --> Chaining)
    { <sym> {*} }


## tight and
token infix:sym<&&> ( --> Tight_and)
    { <sym> {*} }


## tight or
token infix:sym<||> ( --> Tight_or)
    { <sym> {*} }

token infix:sym<^^> ( --> Tight_or)  {
    <sym>
    { $<O><assoc> := 'list' }  # override Tight_or's 'left' associativity
    {*}
}

token infix:sym<//> ( --> Tight_or)
    { <sym> {*} }


## conditional
token infix:sym<?? !!> ( --> Conditional) {
    '??'
    <.ws>
    <EXPR(item %conditional)>
    [ '!!' ||
        [
        || <?before '='> <.panic: "Assignment not allowed within ??!!">
        || <?before '::'> <.panic: "Please use !! rather than ::">
        || <?before <infix>>    # Note: a tight infix would have parsed right
            <.panic: "Precedence too loose within ??!!; use ??()!! instead ">
        || <.panic: "Found ?? but no !!; possible precedence problem">
        ]
    ]
    {*}
}

token infix:sym<?> ( --> Conditional)
    { <sym> <obs('?: for the conditional operator', '??!!')> }


## assignment
# There is no "--> type" because assignment may be coerced to either
# item assignment or list assignment at "make" time.

token infix:sym<=> ()
{
    <sym>
    { $¢ = (self.<sigil>//'') eq '$' 
        ?? Perl::Item_assignment.coerce($¢)
        !! Perl::List_assignment.coerce($¢);
    }
    {*}
}

token infix:sym<:=> ( --> Item_assignment)
    { <sym> {*} }

token infix:sym<::=> ( --> Item_assignment)
    { <sym> {*} }

# XXX need to do something to turn subcall into method call here...
token infix:sym<.=> ( --> Item_assignment)
    { <sym> {*} }

token infix:sym« => » ( --> Item_assignment)
    { <sym> {*} }

# Note, other assignment ops generated by infix_postfix_meta_operator rule

## loose unary
token prefix:sym<true> ( --> Loose_unary)
    { <sym> » {*} }

token prefix:sym<not> ( --> Loose_unary)
    { <sym> » {*} }

## list item separator
token infix:sym<,> ( --> Comma)
    { <sym> {*} }

token infix:sym« p5=> » ( --> Comma)
    { <sym> {*} }

## list infix
token infix:sym<X> ( --> List_infix)
    { <sym> » {*} }

token infix:sym<Z> ( --> List_infix)
    { <sym> » {*} }

# XXX tre workaround
# token infix:sym<minmax> ( --> List_infix)
#     { <sym> » {*} }

token term:sigil ( --> List_prefix)
{
    <sigil> <?before \s> <arglist>
    { $<sym> = $<sigil>.item; }
    {*}
}

# token term:typecast ( --> List_prefix)
#     { <typename> <?spacey> <arglist> { $<sym> = $<typename>.item; } {*} }

# unrecognized identifiers are assumed to be post-declared listops.
# (XXX for cheating purposes this rule must be the last term: rule)
token term:name ( --> List_prefix)
{
    <name> ::
    [
    ||  <?{
            $¢.is_type($<name>.text)
        }> ::
        # parametric type?
        <.unsp>? [ <?before '['> <postcircumfix> ]?
        [
            '::'
            <?before [ '«' | '<' | '{' | '<<' ] > <postcircumfix>
            {*}                                                 #= packagevar 
        ]?
        {*}                                                     #= typename
    ||
        [
        | '.(' <semilist> ')' {*}                               #= func args
        | '(' <semilist> ')' {*}                                #= func args
        | <?before \s> <arglist> {*}                            #= listop args
        | <.unsp> '.'? '(' <semilist> ')' {*}                   #= func args
        | :: {*}                                                #= listop noarg
        ]
    ]
    {*}
}

## loose and
token infix:sym<and> ( --> Loose_and)
    { <sym> » {*} }

token infix:sym<andthen> ( --> Loose_and)
    { <sym> » {*} }

# XXX tre workaround
# token infix:sym<andthen> ( --> Loose_and)
#     { <sym> » {*} }

## loose or
token infix:sym<or> ( --> Loose_or)
    { <sym> » {*} }

token infix:sym<orelse> ( --> Loose_or)
    { <sym> » {*} }

token infix:sym<xor> ( --> Loose_or)
    { <sym> » {*} }

# XXX tre workaround
# token infix:sym<orelse> ( --> Loose_or)
#     { <sym> » {*} }

## expression terminator

token terminator:sym<;> ( --> Terminator)
    { <?before ';' > {*} }

token terminator:sym<if> ( --> Terminator)
    { <?before 'if' » > {*} }

token terminator:sym<unless> ( --> Terminator)
    { <?before 'unless' » > {*} }

token terminator:sym<while> ( --> Terminator)
    { <?before 'while' » > {*} }

token terminator:sym<until> ( --> Terminator)
    { <?before 'until' » > {*} }

token terminator:sym<for> ( --> Terminator)
    { <?before 'for' » > {*} }

token terminator:sym<given> ( --> Terminator)
    { <?before 'given' » > {*} }

token terminator:sym<when> ( --> Terminator)
    { <?before 'when' » > {*} }

token terminator:sym« <== » ( --> Terminator)
    { <?before '<==' > {*} }

token terminator:sym« ==> » ( --> Terminator)
    { <?before '==>' > {*} }              #'

token terminator:sym« --> » ( --> Terminator)
    { <?before '-->' > {*} }              #'

token terminator:sym<)> ( --> Terminator)
    { <?before <sym> > {*} }

token terminator:sym<]> ( --> Terminator)
    { <?before ']' > {*} }

token terminator:sym<}> ( --> Terminator)
    { <?before '}' > {*} }

token terminator:sym<!!> ( --> Terminator)
    { <?before '!!' > {*} }

regex infixstopper {
    <?before '{' | <lambda> > <?{ $¢.<_>[$¢.pos]<ws> }>
}

# hopefully we can include these tokens in any outer LTM matcher
regex stdstopper {
    <?{ $¢.<_>[$¢.pos]<endstmt> }>	# nullary but likely, check first
    [
    | <?terminator>
    | <?unitstopper>
    | $					# unlikely, check last (normal LTM behavior)
    ]
    { $¢.<_>[$¢.pos]<endstmt> = 1; }
}

# A fairly complete operator precedence parser

method EXPR ($preclvl)
{
    temp $CTX = self.callm if $DEBUG +& DEBUG::trace_call;
    if self.peek {
        return self._AUTOLEXpeek('EXPR');
    }
    my $preclim = $preclvl ?? $preclvl.<prec> // $LOOSEST !! $LOOSEST;
    my $inquote is context = 0;
    my $prevop is context<rw>;
    my @termstack;
    my @opstack;

    push @opstack, { 'O' => item %terminator, 'sym' => '' };         # (just a sentinel value)

    my $here = self;
    self.deb("In EXPR, at ", $here.pos) if $DEBUG +& DEBUG::EXPR;

    my &reduce := -> {
        self.deb("entering reduce, termstack == ", +@termstack, " opstack == ", +@opstack) if $DEBUG +& DEBUG::EXPR;
        my $op = pop @opstack;
        given $op<O><assoc> // 'unary' {
            when 'chain' {
                self.deb("reducing chain") if $DEBUG +& DEBUG::EXPR;
                my @chain;
                push @chain, pop(@termstack);
                push @chain, $op;
                while @opstack {
                    last if $op<O><prec> ne @opstack[*-1]<O><prec>;
                    push @chain, pop(@termstack).cleanup;
                    push @chain, pop(@opstack);
                }
                push @chain, pop(@termstack).cleanup;
                @chain = reverse @chain if @chain > 1;
                $op<O><chain> = @chain;
                push @termstack, $op;
            }
            when 'list' {
                self.deb("reducing list") if $DEBUG +& DEBUG::EXPR;
                my @list;
                push @list, pop(@termstack);
                while @opstack {
                    self.deb($op<sym>." ne ".@opstack[*-1]<sym>) if $DEBUG +& DEBUG::EXPR;
                    last if $op<sym> ne @opstack[*-1]<sym>;
                    push @list, pop(@termstack).cleanup;
                    pop(@opstack);
                }
                push @list, pop(@termstack).cleanup;
                @list = reverse @list if @list > 1;
                $op<list> = [@list];
                push @termstack, $op;
            }
            when 'unary' {
                self.deb("reducing") if $DEBUG +& DEBUG::EXPR;
                my @list;
                self.deb("Termstack size: ", +@termstack) if $DEBUG +& DEBUG::EXPR;

                self.deb($op.dump) if $DEBUG +& DEBUG::EXPR;
		$op<arg> = (pop @termstack).cleanup;
		$op<_from> = $op<arg><_from>
		    if $op<_from> > $op<arg><_from>;
		$op<_to> = $op<arg><_to>
		    if $op<_to> < $op<arg><_to>;

                push @termstack, $op;
            }
            default {
                self.deb("reducing") if $DEBUG +& DEBUG::EXPR;
                my @list;
                self.deb("Termstack size: ", +@termstack) if $DEBUG +& DEBUG::EXPR;

                self.deb($op.dump) if $DEBUG +& DEBUG::EXPR;
		$op<right> = (pop @termstack).cleanup;
		$op<left> = (pop @termstack).cleanup;
		$op<_from> = $op<left><_from>;
		$op<_to> = $op<right><_to>;

                push @termstack, $op;
            }
        }
    };

    loop {
        self.deb("In loop, at ", $here.pos) if $DEBUG +& DEBUG::EXPR;
        my $oldpos = $here.pos;
        my @t = $here.expect_term();       # eats ws too
        last unless @t;
        $here = @t[0];
        $here.panic("Expected term") unless $here.pos > $oldpos;

        # interleave prefix and postfix, pretend they're infixish
        my $M = $here;
        my @pre;
        @pre = @($M<pre>) if $M<pre>;
        my @post;
        @post = @($M<post>) if $M<post>;
        loop {
            if @pre {
                if @post and @post[0]<O><prec> gt @pre[0]<O><prec> {
                    push @opstack, shift @post;
                }
                else {
                    push @opstack, pop @pre;
                }
            }
            elsif @post {
                push @opstack, shift @post;
            }
            else {
                last;
            }
        }

        push @termstack, $here;
        self.deb("after push: " ~ (0+@termstack)) if $DEBUG +& DEBUG::EXPR;
#        my @infix = $here.expect_tight_infix($preclim);
        $oldpos = $here.pos;
        my @infix = $here.cursor_fresh.expect_infix();
        last unless @infix;
        my $infix = @infix[0];
        last unless $infix.pos > $oldpos;
        
        # XXX might want to allow this in a declaration though
        if not $infix { $here.panic("Can't have two terms in a row") }

        if not $infix<sym> {
            die $infix.dump if $DEBUG +& DEBUG::EXPR;
        }


        my $inO = $infix<O>;
        my Str $inprec = $inO<prec>;
        if not defined $inprec {
            self.deb("No prec given in infix!") if $DEBUG +& DEBUG::EXPR;
            die $infix.dump if $DEBUG +& DEBUG::EXPR;
            $inprec = %terminator<prec>;
        }

        last unless $inprec gt $preclim;

        $here = $infix.cursor_fresh.ws();


        # substitute precedence for listops
        $inO<prec> = $inO<sub> if $inO<sub>;

        # Does new infix (or terminator) force any reductions?
        while @opstack[*-1]<O><prec> gt $inprec {
            reduce();
        }

        # Not much point in reducing the sentinels...
        last if $inprec lt $LOOSEST;

        # Equal precedence, so use associativity to decide.
        if @opstack[*-1]<O><prec> eq $inprec {
            given $inO<assoc> {
                when 'non'   { $here.panic(qq["$infix" is not associative]) }
                when 'left'  { reduce() }   # reduce immediately
                when 'right' { }            # just shift
                when 'chain' { }            # just shift
                when 'list'  {              # if op differs reduce else shift
                    reduce() if $infix<sym> !eqv @opstack[*-1]<sym>;
                }
                default { $here.panic(qq[Unknown associativity "$_" for "$infix"]) }
            }
        }
        push @opstack, $infix;
    }
    reduce() while +@termstack > 1;
    if @termstack {
        +@termstack == 1 or $here.panic("Internal operator parser error, termstack == " ~ (+@termstack));
        @termstack[0]<_from> = self.pos;
        @termstack[0]<_to> = $here.pos;
    }
    @termstack;
}

#################################################
## Regex
#################################################

grammar Regex is Perl {

    token category:metachar { <sym> }
    proto token metachar { <...> }

    token category:backslash { <sym> }
    proto token backslash { <...> }

    token category:assertion { <sym> }
    proto token assertion { <...> }

    token category:quantifier { <sym> }
    proto token quantifier { <...> }

    token category:mod_internal { <sym> }
    proto token mod_internal { <...> }

    proto token rxinfix { <...> }

    token ws {
        <?{ $+sigspace }>
	|| [ <?before \s | '#'> <nextsame> ]?   # still get all the pod goodness, hopefully
    }

    token sigspace {
	<?before \s | '#'> [ :lang($+LANG) <.ws> ]
    }

    rule regex {
        :my $sigspace    is context<rw> = $+sigspace    // 0;
        :my $ratchet     is context<rw> = $+ratchet     // 0;
        :my $ignorecase is context<rw> = $+ignorecase // 0;
        :my $ignoreaccent    is context<rw> = $+ignoreaccent    // 0;
        < || | && & >?
        <EXPR>
        {*}
    }

    token expect_term {
	<.ws>
        <quantified_atom>+
    }
    token expect_infix {
        <!infixstopper>
        <!stdstopper>
        <rxinfix>
        {
            $<O> = $<rxinfix><O>;
            $<sym> = $<rxinfix><sym>;
        }
    }

    token rxinfix:sym<||> ( --> Tight_or ) { <sym> }
    token rxinfix:sym<&&> ( --> Tight_and ) { <sym> }
    token rxinfix:sym<|> ( --> Junctive_or ) { <sym> }
    token rxinfix:sym<&> ( --> Junctive_and ) { <sym> }

    token quantified_atom {
        <!stopper>
        <!rxinfix>
        <atom>
        [ <.ws> <quantifier>
#            <?{ $<atom>.max_width }>
#                || <.panic: "Can't quantify zero-width atom">
        ]?
	<.ws>
        {*}
    }

    token atom {
        [
        | \w
        | <metachar>
        | :: <.panic: "unrecognized metacharacter">
        ]
        {*}
    }

    # sequence stoppers
    token metachar:sym« > » { '>'  :: <fail> }
    token metachar:sym<&&>  { '&&' :: <fail> }
    token metachar:sym<&>   { '&'  :: <fail> }
    token metachar:sym<||>  { '||' :: <fail> }
    token metachar:sym<|>   { '|'  :: <fail> }
    token metachar:sym<]>   { ']'  :: <fail> }
    token metachar:sym<)>   { ')'  :: <fail> }

    token metachar:quant { <quantifier> <.panic: "quantifier quantifies nothing"> }

    # "normal" metachars

    token metachar:sigwhite {
	<sigspace>
    }

    token metachar:sym<{ }> {
        [ :lang($+LANG) <block> ]
        {{ $/<sym> := <{ }> }}
        {*}
    }

    token metachar:mod {
        <mod_internal>
        { $/<sym> := $<mod_internal><sym> }
        {*}
    }

    token metachar:sym<:> {
        <sym>
        {*}
    }

    token metachar:sym<::> {
        <sym>
        {*}
    }

    token metachar:sym<:::> {
        <sym>
        {*}
    }

    token metachar:sym<[ ]> {
        '[' :: [:lang(self.unbalanced(']')) <regex>]
        [ ']' || <.panic: "Missing right bracket"> ]
        { $/<sym> := <[ ]> }
        {*}
    }

    token metachar:sym<( )> {
        '(' :: [:lang(self.unbalanced(')')) <regex>]
        [ ')' || <.panic: "Missing right parenthesis"> ]
        { $/<sym> := <( )> }
        {*}
    }

    token metachar:sym« <( » { '<(' {*} }
    token metachar:sym« )> » { ')>' {*} }

    token metachar:sym« << » { '<<' {*} }
    token metachar:sym« >> » { '>>' {*} }
    token metachar:sym< « > { '«' {*} }
    token metachar:sym< » > { '»' {*} }

    token metachar:qw {
        <?before '<' \s >  # (note required whitespace)
        <quote>
        {*}
    }

    token metachar:sym«< >» {
        '<' <unsp>? <assertion>
        [ '>' || <.panic: "regex assertion not terminated by angle bracket"> ]
        {*}
    }

    token metachar:sym<\\> { <sym> <backslash> {*} }
    token metachar:sym<.>  { <sym> {*} }
    token metachar:sym<^^> { <sym> {*} }
    token metachar:sym<^>  { <sym> {*} }
    token metachar:sym<$$> {
        <sym>
        [ <?before (\w+)> <obs("\$\$$0 to deref var inside a regex","\$(\$$0)")> ]?
        {*}
    }
    token metachar:sym<$>  {
        '$'
        <before
        | \s
        | '|'
        | '&'
        | ')'
        | ']'
        | '>'
        | $
        >
        {*}
    }

    token metachar:sym<' '> { <?before "'"> [:lang($+LANG) <quote>] }
    token metachar:sym<" "> { <?before '"'> [:lang($+LANG) <quote>] }

    token metachar:var {
        <!before '$$'>
        <?before <sigil>>
        [:lang($+LANG) <variable>]
        <.ws>
        $<binding> = ( '=' <.ws> <quantified_atom> )?
        { $<sym> = $<variable>.item; }
        {*}
    }

    token backslash:unspace { <?before \s> <.SUPER::ws> }

    token backslash:a { :i <sym> }
    token backslash:b { :i <sym> }
    token backslash:c { :i <sym>
        [
        || '[' <-[ \] \v ]>* ']'
        || <codepoint>
        ]
    }
    token backslash:d { :i <sym> }
    token backslash:e { :i <sym> }
    token backslash:f { :i <sym> }
    token backslash:h { :i <sym> }
    token backslash:n { :i <sym> }
    token backslash:o { :i <sym> [ <octint> | '['<octint>[','<octint>]*']' ] }
    token backslash:r { :i <sym> }
    token backslash:s { :i <sym> }
    token backslash:t { :i <sym> }
    token backslash:v { :i <sym> }
    token backslash:w { :i <sym> }
    token backslash:x { :i <sym> [ <hexint> | '['<hexint>[','<hexint>]*']' ] }
    token backslash:misc { $<litchar>=(\W) }
    token backslash:oops { :: <.panic: "unrecognized regex backslash sequence"> }

    token assertion:sym<...> { <sym> {*} }
    token assertion:sym<???> { <sym> {*} }
    token assertion:sym<!!!> { <sym> {*} }

    token assertion:sym<?> { <sym> [ <?before '>'> | <assertion> ] }
    token assertion:sym<!> { <sym> [ <?before '>'> | <assertion> ] }

    token assertion:sym<{ }> { [ :lang($+LANG) <block> ] }

    token assertion:variable {
        <?before <sigil>>  # note: semantics must be determined per-sigil
        [:lang($+LANG) <variable=EXPR(item %LOOSEST)>]
        {*}
    }

    token assertion:method {
        '.' [
            | <?before <alpha> > <assertion>
            | [ :lang($+LANG) <dottyop> ]
            ]
        {*}
    }

    token assertion:ident { <ident> [               # is qq right here?
                                    | <?before '>' >
                                    | <.ws> <regex>
                                    | '=' <assertion>
                                    | ':' <.ws>
                                        [ :lang($+LANG.unbalanced('>')) <arglist> ]
                                    | '(' ::
					[ :lang($+LANG) <arglist> ]
					[ ')' || <.panic: "Assertion call missing right parenthesis"> ]
                                    ]?
    }

    token assertion:sym<[> { <before '[' > <cclass_elem>+ }
    token assertion:sym<+> { <before '+' > <cclass_elem>+ }
    token assertion:sym<-> { <before '-' > <cclass_elem>+ }
    token assertion:sym<.> { <sym> }
    token assertion:sym<,> { <sym> }
    token assertion:sym<~~> { <sym> <desigilname>? }

    token assertion:bogus { <.panic: "unrecognized regex assertion"> }

    token cclass_elem {
        [ '+' | '-' ]?
        [
        | <name>
        | <before '['> <quibble(Perl::Q.tweak(:q))>
        ]
    }

    token mod_arg { '(' <semilist> [ ')' || <.panic: "Missing right parenthesis"> ] }

    token mod_internal:adv {
        <quotepair> { $/<sym> := «: $<quotepair><key>» }
    }

    token mod_internal:sym<:my>    { ':' <?before 'my' \s > [:lang($+LANG) <statement> <eat_terminator> ] }

    token mod_internal:sym<:i>    { $<sym>=[':i'|':ignorecase'] » { $+ignorecase = 1 } }
    token mod_internal:sym<:!i>   { $<sym>=[':i'|':ignorecase'] » { $+ignorecase = 0 } }
    # XXX will this please work somehow ???
    token mod_internal:sym<:i( )> { $<sym>=[':i'|':ignorecase'] <mod_arg> { $+ignorecase = $<mod_arg>.eval } }

    token mod_internal:sym<:a>    { $<sym>=[':a'|':ignoreaccent'] » { $+ignoreaccent = 1 } }
    token mod_internal:sym<:!a>   { $<sym>=[':a'|':ignoreaccent'] » { $+ignoreaccent = 0 } }
    # XXX will this please work somehow ???
    token mod_internal:sym<:a( )> { $<sym>=[':a'|':ignoreaccent'] <mod_arg> { $+ignoreaccent = $<mod_arg>.eval } }

    token mod_internal:sym<:s>    { <sym> 'igspace'? » { $+sigspace = 1 } }
    token mod_internal:sym<:!s>   { <sym> 'igspace'? » { $+sigspace = 0 } }
    token mod_internal:sym<:s( )> { <sym> 'igspace'? <mod_arg> { $+sigspace = $<mod_arg>.eval } }

    token mod_internal:sym<:r>    { <sym> 'atchet'? » { $+ratchet = 1 } }
    token mod_internal:sym<:!r>   { <sym> 'atchet'? » { $+ratchet = 0 } }
    token mod_internal:sym<:r( )> { <sym> 'atchet'? » <mod_arg> { $+ratchet = $<mod_arg>.eval } }

    token mod_internal:oops { ':' <.panic: "unrecognized regex modifier"> }

    token quantifier:sym<*>  { <sym> <quantmod> }
    token quantifier:sym<+>  { <sym> <quantmod> }
    token quantifier:sym<?>  { <sym> <quantmod> }
    token quantifier:sym<**> { <sym> <sigspace>? <quantmod> <sigspace>?
        [
        | \d+ [ '..' [ \d+ | '*' ] ]?
        | <block>
        | <quantified_atom>
        ]
    }

    token quantifier:sym<~~> { '!'? <sym> <sigspace> <quantified_atom> }

    token quantmod { [ '?' | '!' | ':' | '+' ]? }

} # end grammar

# The <panic: "message"> rule is called for syntax errors.
# If there are any <suppose> points, backtrack and retry parse
# with a different supposition.  If it gets farther than the
# panic point, print out the supposition ("Looks like you
# used a Perl5-style shift operator (<<) at line 42.  Maybe
# you wanted +< or |< instead.")  Or some such...
# In any event, this is only for better diagnostics, and
# further compilation is suppressed by the <commit><fail>.

# token panic (Str $s) { <commit> <fail($s)> }

method mess (Str $s) {
    my $orig = self.orig;
    my $text = $$orig;
    my $pre = substr($text, 0, self.pos);
    my $line = 1 + $pre ~~ tr!\n!\n!;
    $pre = substr($pre, -40, 40);
    1 while $pre =~ s!.*\n!!;
    my $post = substr($text, self.pos, 40);
    1 while $post ~~ s!(\n.*)!!;
    "############# PARSE FAILED #############\n----> $pre" ~
        '<<<HERE>>>' ~
        "$post\n$s at line $line\n";
}

method panic (Str $s) {
    die self.mess($s);
}

method worry (Str $s) {
    warn self.mess($s);
}

# "when" arg assumes more things will become obsolete after Perl 6 comes out...
method obs (Str $old, Str $new, Str $when = ' in Perl 6') {
    self.panic("Obsolete use of $old;$when please use $new instead");
}

#XXX shouldn't need this, it should all be in GLOBAL:: or the current package hash
my @typenames = (      # (need parens for gimme5 translator)
    <Bit Int Str Num Complex Bool Rat>,
    <Exception Code Block List Seq Range Set Bag Junction Pair>,
    <Mapping Signature Capture Blob Whatever Undef Failure>,
    <StrPos StrLen Version P6opaque>,
    <bit int uint buf num complex bool rat>,
    <Scalar Array Hash KeyHash KeySet KeyBag Buf IO Routine Sub Method>,
    <Submethod Macro Regex Match Package Module Class Role Grammar Any Object>,
);
my %typenames;
%typenames{@typenames} = (1 xx @typenames);

method is_type ($name) {
    return True if %typenames{$name};
    #return True if GLOBAL::{$name}.:exists;
    return False;
}

## vim: expandtab sw=4 syn=perl6
