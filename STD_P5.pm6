# STD_P5.pm
#
# Copyright 2009-2010, Larry Wall
#
# You may copy this software under the terms of the Artistic License,
#     version 2.0 or later.

grammar STD::P5 is STD;

use DEBUG;

method TOP ($STOP?) {
    if defined $STOP {
        my $*GOAL ::= $STOP;
        self.unitstop($STOP).comp_unit;
    }
    else {
        self.comp_unit;
    }
}

##############
# Precedence #
##############

# The internal precedence levels are *not* part of the public interface.
# The current values are mere implementation; they may change at any time.
# Users should specify precedence only in relation to existing levels.

constant %term            = (:dba('term')            , :prec<z=>);
constant %methodcall      = (:dba('methodcall')      , :prec<y=>, :assoc<unary>, :uassoc<left>, :fiddly);
constant %autoincrement   = (:dba('autoincrement')   , :prec<x=>, :assoc<unary>, :uassoc<non>);
constant %exponentiation  = (:dba('exponentiation')  , :prec<w=>, :assoc<right>);
constant %symbolic_unary  = (:dba('symbolic unary')  , :prec<v=>, :assoc<unary>, :uassoc<left>);
constant %binding         = (:dba('binding')         , :prec<u=>, :assoc<unary>, :uassoc<left>);
constant %multiplicative  = (:dba('multiplicative')  , :prec<t=>, :assoc<left>);
constant %additive        = (:dba('additive')        , :prec<s=>, :assoc<left>);
constant %shift           = (:dba('shift')           , :prec<r=>, :assoc<left>);
constant %named_unary     = (:dba('named unary')     , :prec<q=>, :assoc<unary>, :uassoc<left>);
constant %comparison      = (:dba('comparison')      , :prec<p=>, :assoc<non>, :diffy);
constant %equality        = (:dba('equality')        , :prec<o=>, :assoc<chain>, :diffy, :iffy);
constant %bitwise_and     = (:dba('bitwise and')     , :prec<n=>, :assoc<left>);
constant %bitwise_or      = (:dba('bitwise or')      , :prec<m=>, :assoc<left>);
constant %tight_and       = (:dba('tight and')       , :prec<l=>, :assoc<left>);
constant %tight_or        = (:dba('tight or')        , :prec<k=>, :assoc<left>);
constant %range           = (:dba('range')           , :prec<j=>, :assoc<right>, :fiddly);
constant %conditional     = (:dba('conditional')     , :prec<i=>, :assoc<right>, :fiddly);
constant %assignment      = (:dba('assignment')      , :prec<h=>, :assoc<right>);
constant %comma           = (:dba('comma operator')  , :prec<g=>, :assoc<left>, :nextterm<nulltermish>, :fiddly);
constant %listop          = (:dba('list operator')   , :prec<f=>, :assoc<unary>, :uassoc<left>);
constant %loose_not       = (:dba('not operator')    , :prec<e=>, :assoc<unary>, :uassoc<left>);
constant %loose_and       = (:dba('loose and')       , :prec<d=>, :assoc<left>);
constant %loose_or        = (:dba('loose or')        , :prec<c=>, :assoc<left>);
constant %LOOSEST         = (:dba('LOOSEST')         , :prec<a=!>);
constant %terminator      = (:dba('terminator')      , :prec<a=>, :assoc<list>);

# "epsilon" tighter than terminator
#constant $LOOSEST = %LOOSEST<prec>;
constant $LOOSEST = "a=!"; # XXX preceding line is busted

##############
# Categories #
##############

# Categories are designed to be easily extensible in derived grammars
# by merely adding more rules in the same category.  The rules within
# a given category start with the category name followed by a differentiating
# adverbial qualifier to serve (along with the category) as the longer name.

# The endsym context, if specified, says what to implicitly check for in each
# rule right after the initial <sym>.  Normally this is used to make sure
# there's appropriate whitespace.  # Note that endsym isn't called if <sym>
# isn't called.

my $*endsym = "null";
my $*endargs = -1;

proto token category { <...> }

token category:category { <sym> }

token category:p5sigil { <sym> }
proto token p5sigil { <...> }

token category:p5special_variable { <sym> }
proto token p5special_variable { <...> }

token category:p5comment { <sym> }
proto token p5comment { <...> }

token category:p5version { <sym> }
proto token p5version { <...> }

token category:p5module_name { <sym> }
proto token p5module_name { <...> }

token category:p5value { <sym> }
proto token p5value { <...> }

token category:p5term { <sym> }
proto token p5term { <...> }

token category:p5number { <sym> }
proto token p5number { <...> }

token category:p5quote { <sym> }
proto token p5quote () { <...> }

token category:p5prefix { <sym> }
proto token p5prefix is unary is defequiv(%symbolic_unary) { <...> }

token category:p5infix { <sym> }
proto token p5infix is binary is defequiv(%additive) { <...> }

token category:p5postfix { <sym> }
proto token p5postfix is unary is defequiv(%autoincrement) { <...> }

token category:p5dotty { <sym> }
proto token p5dotty (:$*endsym = 'unspacey') { <...> }

token category:p5circumfix { <sym> }
proto token p5circumfix { <...> }

token category:p5postcircumfix { <sym> }
proto token p5postcircumfix is unary { <...> }  # unary as far as EXPR knows...

token category:p5type_declarator { <sym> }
proto token p5type_declarator (:$*endsym = 'spacey') { <...> }

token category:p5scope_declarator { <sym> }
proto token p5scope_declarator (:$*endsym = 'nofun') { <...> }

token category:p5package_declarator { <sym> }
proto token p5package_declarator (:$*endsym = 'spacey') { <...> }

token category:p5routine_declarator { <sym> }
proto token p5routine_declarator (:$*endsym = 'nofun') { <...> }

token category:p5regex_declarator { <sym> }
proto token p5regex_declarator (:$*endsym = 'spacey') { <...> }

token category:p5statement_prefix { <sym> }
proto rule  p5statement_prefix () { <...> }

token category:p5statement_control { <sym> }
proto rule  p5statement_control (:$*endsym = 'spacey') { <...> }

token category:p5statement_mod_cond { <sym> }
proto rule  p5statement_mod_cond (:$*endsym = 'nofun') { <...> }

token category:p5statement_mod_loop { <sym> }
proto rule  p5statement_mod_loop (:$*endsym = 'nofun') { <...> }

token category:p5terminator { <sym> }
proto token p5terminator { <...> }

token unspacey { <.unsp>? }
token endid { <?before <-[ \- \' \w ]> > }
token spacey { <?before <[ \s \# ]> > }
token nofun { <!before '(' | '.(' | '\\' | '\'' | '-' | "'" | \w > }

##################
# Lexer routines #
##################

token ws {
    :temp @*STUB = return self if @*MEMOS[self.pos]<ws> :exists;
    :my $startpos = self.pos;

    :dba('whitespace')
    [
        | \h+ <![\#\s\\]> { @*MEMOS[$¢.pos]<ws> = $startpos; }   # common case
        | <?before \w> <?after \w> :::
            { @*MEMOS[$startpos]<ws> :delete; }
            <.panic: "Whitespace is required between alphanumeric tokens">        # must \s+ between words
    ]
    ||
    [
    | <.unsp>
    | <.vws> <.heredoc>
    | <.unv>
    | $ { $¢.moreinput }
    ]*

    {{
        if ($¢.pos == $startpos) {
            @*MEMOS[$¢.pos]<ws> :delete;
        }
        else {
            @*MEMOS[$¢.pos]<ws> :delete;
            @*MEMOS[$¢.pos]<endstmt> = @*MEMOS[$startpos]<endstmt>
                if @*MEMOS[$startpos]<endstmt> :exists;
        }
    }}
}

token unsp {
    <!>
}

token vws {
    :dba('vertical whitespace')
    \v
    [ '#DEBUG -1' { say "DEBUG"; $STD::DEBUG = $*DEBUG = -1; } ]?
}

# We provide two mechanisms here:
# 1) define $*moreinput, or
# 2) override moreinput method
method moreinput () {
    $*moreinput.() if $*moreinput;
}

token unv {
   :dba('horizontal whitespace')
   [
   | \h+
   | <?before \h* '=' [ \w | '\\'] > ^^ <.pod_comment>
   | \h* <comment=p5comment>
   ]
}

token p5comment:sym<#> {
   '#' {} \N*
}

token ident {
    <.alpha> \w*
}

token identifier {
    <.alpha> \w*
}

# XXX We need to parse the pod eventually to support $= variables.

token pod_comment {
    ^^ \h* '=' <.unsp>?
    [
    | 'begin' \h+ <identifier> ::
        [
        ||  .*? "\n" \h* '=' <.unsp>? 'end' \h+ $<identifier> » \N*
        ||  <?{ $<identifier>.Str eq 'END'}> .*
        || { my $id = $<identifier>.Str; self.panic("=begin $id without matching =end $id"); }
        ]
    | 'begin' » :: \h* [ $$ || '#' || <.panic: "Unrecognized token after =begin"> ]
        [ .*? "\n" \h* '=' <.unsp>? 'end' » \N* || { self.panic("=begin without matching =end"); } ]
        
    | 'for' » :: \h* [ <identifier> || $$ || '#' || <.panic: "Unrecognized token after =for"> ]
        [.*?  ^^ \h* $$ || .*]
    | :: 
        [ <?before .*? ^^ '=cut' » > <.panic: "Obsolete pod format, please use =begin/=end instead"> ]?
        [<alpha>||\s||<.panic: "Illegal pod directive">]
        \N*
    ]
}

###################
# Top-level rules #
###################

# Note: we only check for the stopper.  We don't check for ^ because
# we might be embedded in something else.
rule comp_unit {
    :my $*begin_compunit = 1;
    :my $*endargs = -1;
    :my %*LANG;
    :my $*PKGDECL ::= "";
    :my $*IN_DECL;
    :my $*DECLARAND;
    :my $*NEWPKG;
    :my $*NEWLEX;
    :my $*QSIGIL ::= '';
    :my $*IN_META = 0;
    :my $*QUASIMODO;
    :my $*SCOPE = "";
    :my $*LEFTSIGIL;
    :my %*MYSTERY = ();
    :my $*INVOCANT_OK;
    :my $*INVOCANT_IS;
    :my $*CURLEX;
    :my $*MULTINESS = '';

    :my $*CURPKG;
    {{

        %*LANG<MAIN>    = ::STD ;
        %*LANG<Q>       = ::STD::Q ;
        %*LANG<Regex>   = ::STD::Regex ;
        %*LANG<Trans>   = ::STD::Trans ;
        %*LANG<P5>      = ::STD::P5 ;
        %*LANG<P5Regex> = ::STD::P5::Regex ;

        @*WORRIES = ();
        self.load_setting($*SETTINGNAME);
        my $oid = $*SETTING.id;
        my $id = 'MY:file<' ~ $*FILE<name> ~ '>';
        $*CURLEX = Stash.new(
            'OUTER::' => [$oid],
            '!file' => $*FILE, '!line' => 0,
            '!id' => [$id],
        );
        $STD::ALL.{$id} = $*CURLEX;
        $*UNIT = $*CURLEX;
        $STD::ALL.<UNIT> = $*UNIT;
        self.finishlex;
    }}
    <statementlist>
    [ <?unitstopper> || <.panic: "Confused"> ]
    # "CHECK" time...
    {{
        if @*WORRIES {
            warn "Potential difficulties:\n  " ~ join( "\n  ", @*WORRIES) ~ "\n";
        }
        my $m = $¢.explain_mystery();
        warn $m if $m;
    }}
}

method explain_mystery() {
    my %post_types;
    my %unk_types;
    my %unk_routines;
    my $m = '';
    for keys(%*MYSTERY) {
        my $p = %*MYSTERY{$_}.<lex>;
        if self.is_name($_, $p) {
            # types may not be post-declared
            %post_types{$_} = %*MYSTERY{$_};
            next;
        }

        next if self.is_known($_, $p) or self.is_known('&' ~ $_, $p);

        # just a guess, but good enough to improve error reporting
        if $_ lt 'a' {
            %unk_types{$_} = %*MYSTERY{$_};
        }
        else {
            %unk_routines{$_} = %*MYSTERY{$_};
        }
    }
    if %post_types {
        my @tmp = sort keys(%post_types);
        $m ~= "Illegally post-declared type" ~ ('s' x (@tmp != 1)) ~ ":\n";
        for @tmp {
            $m ~= "\t$_ used at line " ~ %post_types{$_}.<line> ~ "\n";
        }
    }
    if %unk_types {
        my @tmp = sort keys(%unk_types);
        $m ~= "Undeclared name" ~ ('s' x (@tmp != 1)) ~ ":\n";
        for @tmp {
            $m ~= "\t$_ used at line " ~ %unk_types{$_}.<line> ~ "\n";
        }
    }
    if %unk_routines {
        my @tmp = sort keys(%unk_routines);
        $m ~= "Undeclared routine" ~ ('s' x (@tmp != 1)) ~ ":\n";
        for @tmp {
            $m ~= "\t$_ used at line " ~ %unk_routines{$_}.<line> ~ "\n";
        }
    }
    $m;
}

# Look for an expression followed by a required lambda.
token xblock {
    :my $*GOAL ::= '{';
    :dba('block expression') '(' ~ ')' <EXPR>
    <.ws>
    <block>
}

token block {
    :temp $*CURLEX;
    :dba('scoped block')
    [ <?before '{' > || <.panic: "Missing block"> ]
    <.newlex>
    <blockoid>
}

token blockoid {
    # encapsulate braided languages
    :temp %*LANG;

    <.finishlex>
    [
    | :dba('block') '{' ~ '}' <statementlist>
    | <?terminator> <.panic: 'Missing block'>
    | <?> <.panic: "Malformed block">
    ]

    [
    | <?before \h* $$>  # (usual case without comments)
        { @*MEMOS[$¢.pos]<endstmt> = 2; }
    | \h* <?before <[\\,:]>>
    | <.unv>? $$
        { @*MEMOS[$¢.pos]<endstmt> = 2; }
    | {} <.unsp>? { @*MEMOS[$¢.pos]<endargs> = 1; }
    ]
}

token regex_block {
    # encapsulate braided languages
    :temp %*LANG;

    :my $lang = %*LANG<Regex>;
    :my $*GOAL ::= '}';

    [ <quotepair> <.ws>
        {
            my $kv = $<quotepair>[*-1];
            $lang = $lang.tweak($kv.<k>, $kv.<v>)
                or self.sorry("Unrecognized adverb :" ~ $kv.<k> ~ '(' ~ $kv.<v> ~ ')');
        }
    ]*

    '{'
    <nibble( $¢.cursor_fresh($lang).unbalanced('}') )>
    [ '}' || <.panic: "Unable to parse regex; couldn't find right brace"> ]

    [
    | <?before \h* $$>  # (usual case without comments)
        { @*MEMOS[$¢.pos]<endstmt> = 2; }
    | \h* <?before <[\\,:]>>
    | <.unv>? $$
        { @*MEMOS[$¢.pos]<endstmt> = 2; }
    | {} <.unsp>? { @*MEMOS[$¢.pos]<endargs> = 1; }
    ]
}

# statement semantics
rule statementlist {
    :my $*INVOCANT_OK = 0;
    :dba('statement list')

    [
    | $
    | <?before <[\)\]\}]> >
    | [<statement><eat_terminator> ]*
    ]
}

# embedded semis, context-dependent semantics
rule semilist {
    :my $*INVOCANT_OK = 0;
    :dba('semicolon list')
    [
    | <?before <[\)\]\}]> >
    | [<statement><eat_terminator> ]*
    ]
}


token label {
    :my $label;
    <identifier> ':' <?before \s> <.ws>

    [ <?{ $¢.is_name($label = $<identifier>.Str) }>
      <.sorry("Illegal redeclaration of '$label'")>
    ]?

    # add label as a pseudo type
    {{ $¢.add_my_name($label); }}

}

token statement {
    :my $*endargs = -1;
    :my $*QSIGIL ::= 0;
    <!before <[\)\]\}]> >

    # this could either be a statement that follows a declaration
    # or a statement that is within the block of a code declaration
    <!!{ $¢ = %*LANG<MAIN>.bless($¢); }>

    [
    | <label> <statement>
    | <statement_control=p5statement_control>
    | <EXPR>
        :dba('statement end')
            <.ws>
	    :dba('statement modifier')
            [
            | <statement_mod_loop=p5statement_mod_loop>
            | <statement_mod_cond=p5statement_mod_cond>
            ]?
    | <?before ';'>
    ]
}

token eat_terminator {
    [
    || ';' [ <?before $> { $*ORIG ~~ s/\;$/ /; } ]?
    || <?{ @*MEMOS[$¢.pos]<endstmt> }> <.ws>
    || <?terminator>
    || $
    || {{ if @*MEMOS[$¢.pos]<ws> { $¢.pos = @*MEMOS[$¢.pos]<ws>; } }}   # undo any line transition
        <.panic: "Confused">
    ]
}

#####################
# statement control #
#####################

token p5statement_control:use {
    :my $longname;
    :my $*SCOPE = 'use';
    <sym> <.ws>
    [
    | <version=p5version>
    | <module_name=p5module_name>
        {{
            $longname = $<module_name><longname>;
        }}
        [
        || <.spacey> <arglist>
            {{
                $¢.do_use($longname, $<arglist>);
            }}
        || {{ $¢.do_use($longname, ''); }}
        ]
    ]
    <.ws>
}


token p5statement_control:no {
    <sym> <.ws>
    <module_name=p5module_name>[<.spacey><arglist>]?
    <.ws>
}


token p5statement_control:if {
    $<sym>=['if'|'unless']:s
    <xblock>
    [
        [ <!before 'else'\s*'if'> || <.panic: "Please use 'elsif'"> ]
        'elsif'<?spacey> <elsif=xblock>
    ]*
    [
        'else'<?spacey> <else=pblock>
    ]?
}

token p5statement_control:while {
    <sym> :s
    <xblock>
}

token p5statement_control:until {
    <sym> :s
    <xblock>
}

token p5statement_control:for {
    ['for'|'foreach'] <.ws>
    $<eee> = (
        '(' [ :s
            <e1=EXPR>? ';'
            <e2=EXPR>? ';'
            <e3=EXPR>?
        ')'||<.panic: "Malformed loop spec">]
    )? <.ws>
    <block>
}

token p5statement_control:given {
    <sym> :s
    <xblock>
}
token p5statement_control:when {
    <sym> :s
    <xblock>
}
rule p5statement_control:default {<sym> <block> }

rule p5statement_prefix:BEGIN    {<sym> <block> }
rule p5statement_prefix:CHECK    {<sym> <block> }
rule p5statement_prefix:INIT     {<sym> <block> }
rule p5statement_control:END     {<sym> <block> }

#######################
# statement modifiers #
#######################

rule modifier_expr { <EXPR> }

rule p5statement_mod_cond:if     {<sym> <modifier_expr> }
rule p5statement_mod_cond:unless {<sym> <modifier_expr> }
rule p5statement_mod_cond:when   {<sym> <modifier_expr> }

rule p5statement_mod_loop:while {<sym> <modifier_expr> }
rule p5statement_mod_loop:until {<sym> <modifier_expr> }

rule p5statement_mod_loop:for   {<sym> <modifier_expr> }
rule p5statement_mod_loop:given {<sym> <modifier_expr> }

################
# module names #
################

token def_module_name {
    <longname>
    [ :dba('generic role')
        <?before '['>
        <?{ ($*PKGDECL//'') eq 'role' }>
        <.newlex>
        '[' ~ ']' <signature>
        { $*IN_DECL = 0; }
        <.finishlex>
    ]?
}

token p5module_name:normal {
    <longname>
    [ <?before '['> :dba('generic role') '[' ~ ']' <arglist> ]?
}

token vnum {
    \d+ | '*'
}

token p5version:sym<v> {
    'v' <?before \d+> :: <vnum> +% '.' '+'?
}

###############
# Declarators #
###############

token variable_declarator {
    :my $*IN_DECL = 1;
    :my $*DECLARAND;
    <variable>
    { $*IN_DECL = 0; $¢.add_variable($<variable>.Str) }
    [   # Is it a shaped array or hash declaration?
      #  <?{ $<sigil> eq '@' | '%' }>
        <.unsp>?
        $<shape> = [
        | '(' ~ ')' <signature>
        | :dba('shape definition') '[' ~ ']' <semilist>
        | :dba('shape definition') '{' ~ '}' <semilist>
        | <?before '<'> <postcircumfix=p5postcircumfix>
        ]*
    ]?
    <.ws>

    <trait>*
}

rule scoped($*SCOPE) {
    :dba('scoped declarator')
    [
    | <declarator>
    | <regex_declarator=p5regex_declarator>
    | <package_declarator=p5package_declarator>
    ]
    || <?before <[A..Z]>><longname>{{
            my $t = $<longname>.Str;
            if not $¢.is_known($t) {
                $¢.sorry("In \"$*SCOPE\" declaration, typename $t must be predeclared (or marked as declarative with :: prefix)");
            }
        }}
        <!> # drop through
    || <.panic: "Malformed $*SCOPE">
}


token p5scope_declarator:my        { <sym> <scoped('my')> }
token p5scope_declarator:our       { <sym> <scoped('our')> }
token p5scope_declarator:state     { <sym> <scoped('state')> }

token p5package_declarator:package {
    :my $*PKGDECL ::= 'package';
    <sym> <package_def>
}

token p5package_declarator:require {   # here because of declarational aspects
    <sym> <.ws>
    [
    || <module_name=p5module_name> <EXPR>?
    || <EXPR>
    ]
}

rule package_def {
    :my $longname;
    :my $*IN_DECL = 1;
    :my $*DECLARAND;
    :my $*NEWPKG;
    :my $*NEWLEX;
    { $*SCOPE ||= 'our'; }
    [
        [
            <def_module_name>{
                $longname = $<def_module_name>[0]<longname>;
                $¢.add_name($longname.Str);
            }
        ]?
        [
        || <?before ';'>
	    {{
		$longname orelse $¢.panic("Package cannot be anonymous");
		my $shortname = $longname.<name>.Str;
		$*CURPKG = $*NEWPKG // $*CURPKG.{$shortname ~ '::'};
	    }}
        || <.panic: "Unable to parse " ~ $*PKGDECL ~ " definition">
        ]
    ] || <.panic: "Malformed $*PKGDECL">
}

token declarator {
    [
    | <constant_declarator>
    | <variable_declarator>
    | '(' ~ ')' <signature> <trait>*
    | <routine_declarator=p5routine_declarator>
    | <regex_declarator=p5regex_declarator>
    | <type_declarator=p5type_declarator>
    ]
}

token p5multi_declarator:null {
    :my $*MULTINESS = '';
    <declarator>
}

token p5routine_declarator:sub       { <sym> <routine_def> }

rule parensig {
    :dba('signature')
    '(' ~ ')' <signature(1)>
}

method checkyada {
    try {
        my $startsym = self.<blockoid><statementlist><statement>[0]<EXPR><term><sym> // '';
        if $startsym eq '...' or $startsym eq '!!!' or $startsym eq '???' {
            $*DECLARAND<stub> = 1;
        }
    };
    return self;
}

rule routine_def () {
    :temp $*CURLEX;
    :my $*IN_DECL = 1;
    :my $*DECLARAND;
    [
        [ '&'<deflongname>? | <deflongname> ]?
        <.newlex(1)>
        <parensig>?
	<trait>*
        <!{
            $*IN_DECL = 0;
        }>
        <blockoid>:!s
        <.checkyada>
        <.getsig>
    ] || <.panic: "Malformed routine">
}

rule trait {
    :my $*IN_DECL = 0;
    ':' <EXPR(item %comma)>
}

#########
# Nouns #
#########

# (for when you want to tell EXPR that infix already parsed the term)
token nullterm {
    <?>
}

token nulltermish {
    :dba('null term')
    [
    | <?stdstopper>
    | <term=termish>
        {
            $¢.<PRE>  = $<term><PRE>:delete;
            $¢.<POST> = $<term><POST>:delete;
            $¢.<~CAPS> = $<term><~CAPS>;
        }
    | <?>
    ]
}

token termish {
    :my $*SCOPE = "";
    :my $*VAR;
    :dba('prefix or term')
    [
    | <PRE> [ <!{ my $p = $<PRE>; my @p = @$p; @p[*-1]<O><term> and $<term> = pop @$p }> <PRE> ]*
        [ <?{ $<term> }> || <term> ]
    | <term=p5term>
    ]

    # also queue up any postfixes
    :dba('postfix')
    [
    || <?{ $*QSIGIL }>
        [
        || <?{ $*QSIGIL eq '$' }> [ <POST>+! <?after <[ \] } > ) ]> > ]?
        ||                          <POST>+! <?after <[ \] } > ) ]> > 
        || { $*VAR = 0; }
        ]
    || <!{ $*QSIGIL }>
        <POST>*
    ]
    {
        self.check_variable($*VAR) if $*VAR;
        $¢.<~CAPS> = $<term><~CAPS>;
    }
}

token p5term:fatarrow           { <fatarrow> }
token p5term:variable           { <variable> { $*VAR = $<variable> } }
token p5term:package_declarator { <package_declarator=p5package_declarator> }
token p5term:scope_declarator   { <scope_declarator=p5scope_declarator> }
token p5term:routine_declarator { <routine_declarator=p5routine_declarator> }
token p5term:circumfix          { <circumfix=p5circumfix> }
token p5term:dotty              { <dotty=p5dotty> }
token p5term:value              { <value=p5value> }
token p5term:capterm            { <capterm> }
token p5term:statement_prefix   { <statement_prefix=p5statement_prefix> }

token fatarrow {
    <key=identifier> \h* '=>' <.ws> <val=EXPR(item %assignment)>
}

# Most of these special variable rules are there simply to catch old p5 brainos

token p5special_variable:sym<$!> { <sym> <!before \w> }

token p5special_variable:sym<$!{ }> {
    '$!{' ~ '}' <EXPR>
}

token p5special_variable:sym<$/> {
    <sym>
}

token p5special_variable:sym<$~> {
    <sym>
}

token p5special_variable:sym<$`> {
    <sym>
}

token p5special_variable:sym<$@> {
    <sym>
}

token p5special_variable:sym<$#> {
    <sym>
}
token p5special_variable:sym<$$> {
    <sym> <!alpha>
}
token p5special_variable:sym<$%> {
    <sym>
}

token p5special_variable:sym<$^X> {
    <sigil=p5sigil> '^' $<letter> = [<[A..Z]>] <?before \W >
}

token p5special_variable:sym<$^> {
    <sym>
}

token p5special_variable:sym<$&> {
    <sym>
}

token p5special_variable:sym<$*> {
    <sym>
}

token p5special_variable:sym<$)> {
    <sym>
}

token p5special_variable:sym<$-> {
    <sym>
}

token p5special_variable:sym<$=> {
    <sym>
}

token p5special_variable:sym<@+> {
    <sym>
}

token p5special_variable:sym<%+> {
    <sym>
}

token p5special_variable:sym<$+[ ]> {
    '$+['
}

token p5special_variable:sym<@+[ ]> {
    '@+['
}

token p5special_variable:sym<@+{ }> {
    '@+{'
}

token p5special_variable:sym<@-> {
    <sym>
}

token p5special_variable:sym<%-> {
    <sym>
}

token p5special_variable:sym<$-[ ]> {
    '$-['
}

token p5special_variable:sym<@-[ ]> {
    '@-['
}

token p5special_variable:sym<%-{ }> {
    '@-{'
}

token p5special_variable:sym<$+> {
    <sym>
}

token p5special_variable:sym<${^ }> {
    <sigil=p5sigil> '{^' :: $<text>=[.*?] '}'
}

token p5special_variable:sym<::{ }> {
    '::' <?before '{'>
}

regex p5special_variable:sym<${ }> {
    <sigil=p5sigil> '{' {} $<text>=[.*?] '}'
}

token p5special_variable:sym<$[> {
    <sym>
}

token p5special_variable:sym<$]> {
    <sym>
}

token p5special_variable:sym<$\\> {
    <sym>
}

token p5special_variable:sym<$|> {
    <sym>
}

token p5special_variable:sym<$:> {
    <sym>
}

token p5special_variable:sym<$;> {
    <sym>
}

token p5special_variable:sym<$'> { #'
    <sym>
}

token p5special_variable:sym<$"> {
    <sym> <!{ $*QSIGIL }>
}

token p5special_variable:sym<$,> {
    <sym>
}

token p5special_variable:sym['$<'] {
    <sym>
}

token p5special_variable:sym«\$>» {
    <sym>
}

token p5special_variable:sym<$.> {
    <sym>
}

token p5special_variable:sym<$?> {
    <sym>
}

# desigilname should only follow a sigil

token desigilname {
    [
    | <?before '$' > <variable> { $*VAR = $<variable> }
    | <longname>
    ]
}

token variable {
    :my $*IN_META = 0;
    :my $sigil = '';
    :my $name;
    <?before <sigil=p5sigil> {
        $sigil = $<sigil>.Str;
    }> {}
    [
    || '&'
        [
        | <sublongname> { $name = $<sublongname>.Str }
        | :dba('infix noun') '[' ~ ']' <infixish(1)>
        ]
    || [
        | <sigil=p5sigil> <desigilname> { $name = $<desigilname>.Str }
        | <special_variable=p5special_variable>
        | <sigil=p5sigil> $<index>=[\d+]
        # Note: $() can also parse as contextualizer in an expression; should have same effect
        | <sigil=p5sigil> <?before '<' | '('> <postcircumfix=p5postcircumfix>
        | <sigil=p5sigil> <?{ $*IN_DECL }>
        | <?> {{
            if $*QSIGIL {
                return ();
            }
            else {
                $¢.panic("Anonymous variable requires declarator");
            }
          }}
        ]
    ]

}



# Note, don't reduce on a bare sigil unless you don't care what the longest token is.

token p5sigil:sym<$>  { <sym> }
token p5sigil:sym<@>  { <sym> }
token p5sigil:sym<%>  { <sym> }
token p5sigil:sym<&>  { <sym> }

token deflongname {
    :dba('new name to be defined')
    <name>
    [
    | <colonpair>+ { $¢.add_macro($<name>) if $*IN_DECL; }
    | { $¢.add_routine($<name>.Str) if $*IN_DECL; }
    ]
}

token longname {
    <name> <colonpair>*
}

token name {
    [
    | <identifier> <morename>*
    | <morename>+
    ]
}

token morename {
    :my $*QSIGIL ::= '';
    '::'
    [
        <?before '(' | <alpha> >
        [
        | <identifier>
        | :dba('indirect name') '(' ~ ')' <EXPR>
        ]
    ]?
}

token subshortname {
    [
    | <category>
        [ <colonpair>+ { $¢.add_macro($<category>) if $*IN_DECL; } ]?
    | <desigilname>
    ]
}

token sublongname {
    <subshortname> <sigterm>?
}

token p5value:quote   { <quote=p5quote> }
token p5value:number  { <number=p5number> }
token p5value:version { <version=p5version> }

# Note: call this only to use existing type, not to declare type
token typename {
    [
    | '::?'<identifier>                 # parse ::?CLASS as special case
    | <longname>
      <?{{
        my $longname = $<longname>.Str;
        if substr($longname, 0, 2) eq '::' {
            $¢.add_my_name(substr($longname, 2));
        }
        else {
            $¢.is_name($longname)
        }
      }}>
    ]
    # parametric type?
    <.unsp>? [ <?before '['> <postcircumfix=p5postcircumfix> ]?
    <.ws> [ 'of' <.ws> <typename> ]?
}

token numish {
    [
    | <integer>
    | <dec_number>
    | <rad_number>
    | 'NaN' »
    | 'Inf' »
    | '+Inf' »
    | '-Inf' »
    ]
}

token number:numish { <numish> }

token integer {
    [
    | 0 [ b <[01]>+           [ _ <[01]>+ ]*
        | o <[0..7]>+         [ _ <[0..7]>+ ]*
        | x <[0..9a..fA..F]>+ [ _ <[0..9a..fA..F]>+ ]*
        | d \d+               [ _ \d+]*
        | \d+[_\d+]*
            <!!{ $¢.worry("Leading 0 does not indicate octal in Perl 6") }>
        ]
    | \d+[_\d+]*
    ]
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
}

token escale {
    <[Ee]> <[+\-]>? \d+[_\d+]*
}

# careful to distinguish from both integer and 42.method
token dec_number {
    :dba('decimal number')
    [
    | $<coeff> = [           '.' \d+[_\d+]* ] <escale>?
    | $<coeff> = [\d+[_\d+]* '.' \d+[_\d+]* ] <escale>?
    | $<coeff> = [\d+[_\d+]*                ] <escale>
    ]
    <!!before [ '.' <?before \d> <.panic: "Number contains two decimal points (missing 'v' for version number?)">]? >
}

token octints { [<.ws><octint><.ws>] +% ',' }

token octint {
    <[ 0..7 ]>+ [ _ <[ 0..7 ]>+ ]*
}

token hexints { [<.ws><hexint><.ws>] +% ',' }

token hexint {
    <[ 0..9 a..f A..F ]>+ [ _ <[ 0..9 a..f A..F ]>+ ]*
}

##########
# Quotes #
##########

our @herestub_queue;

class Herestub {
    has Str $.delim;
    has $.orignode;
    has $.lang;
} # end class

role herestop {
    token stopper { ^^ {} $<ws>=(\h*?) $*DELIM \h* <.unv>?? $$ \v? }
} # end role

# XXX be sure to temporize @herestub_queue on reentry to new line of heredocs

method heredoc () {
    my $*CTX ::= self.callm if $*DEBUG +& DEBUG::trace_call;
    return if self.peek;
    my $here = self;
    while my $herestub = shift @herestub_queue {
        my $*DELIM = $herestub.delim;
        my $lang = $herestub.lang.mixin( ::herestop );
        my $doc;
        if ($doc) = $here.nibble($lang) {
            $here = $doc.trim_heredoc();
            $herestub.orignode<doc> = $doc;
        }
        else {
            self.panic("Ending delimiter $*DELIM not found");
        }
    }
    return self.cursor($here.pos);  # return to initial type
}

proto token backslash { <...> }
proto token escape { <...> }
token starter { <!> }
token escape:none { <!> }

token babble ($l) {
    :my $lang = $l;
    :my $start;
    :my $stop;

    <.ws>
    [ <quotepair> <.ws>
        {
            my $kv = $<quotepair>[*-1];
            $lang = $lang.tweak($kv.<k>, $kv.<v>)
                or self.sorry("Unrecognized adverb :" ~ $kv.<k> ~ '(' ~ $kv.<v> ~ ')');
        }
    ]*

    {
        ($start,$stop) = $¢.peek_delimiters();
        $lang = $start ne $stop ?? $lang.balanced($start,$stop)
                                !! $lang.unbalanced($stop);
        $<B> = [$lang,$start,$stop];
    }
}

token quibble ($l) {
    :my ($lang, $start, $stop);
    <babble($l)>
    { my $B = $<babble><B>; ($lang,$start,$stop) = @$B; }

    $start <nibble($lang)> [ $stop || <.panic: "Couldn't find terminator $stop"> ]

    {{
        if $lang<_herelang> {
            push @herestub_queue,
                ::Herestub.new(
                    delim => $<nibble><nibbles>[0]<TEXT>,
                    orignode => $¢,
                    lang => $lang<_herelang>,
                );
        }
    }}
}

token sibble ($l, $lang2) {
    :my ($lang, $start, $stop);
    <babble($l)>
    { my $B = $<babble><B>; ($lang,$start,$stop) = @$B; }

    $start <left=nibble($lang)> [ $stop || <.panic: "Couldn't find terminator $stop"> ]
    [ <?{ $start ne $stop }>
        <.ws> <quibble($lang2)>
    || 
        { $lang = $lang2.unbalanced($stop); }
        <right=nibble($lang)> $stop
    ]
}

token tribble ($l, $lang2 = $l) {
    :my ($lang, $start, $stop);
    <babble($l)>
    { my $B = $<babble><B>; ($lang,$start,$stop) = @$B; }

    $start <left=nibble($lang)> [ $stop || <.panic: "Couldn't find terminator $stop"> ]
    [ <?{ $start ne $stop }>
        <.ws> <quibble($lang2)>
    || 
        { $lang = $lang2.unbalanced($stop); }
        <right=nibble($lang)> $stop
    ]
}

token quasiquibble ($l) {
    :my ($lang, $start, $stop);
    :my $*QUASIMODO = 0; # :COMPILING sets true
    <babble($l)>
    { my $B = $<babble><B>; ($lang,$start,$stop) = @$B; }

    [
    || <?{ $start eq '{' }> [ :lang($lang) <block> ]
    || $start [ :lang($lang) <statementlist> ] [$stop || <.panic: "Couldn't find terminator $stop"> ]
    ]
}

# note: polymorphic over many quote languages, we hope
token nibbler {
    :my $text = '';
    :my $from = self.pos;
    :my $to = $from;
    :my @nibbles = ();
    :my $multiline = 0;
    { $<_from> = self.pos; }
    [ <!before <stopper> >
        [
        || <starter> <nibbler> <stopper>
                        {{
                            push @nibbles, $¢.makestr(TEXT => $text, _from => $from, _pos => $to ) if $from != $to;

                            my $n = $<nibbler>[*-1]<nibbles>;
                            my @n = @$n;

                            push @nibbles, $<starter>;
                            push @nibbles, @n;
                            push @nibbles, $<stopper>;

                            $text = '';
                            $to = $from = $¢.pos;
                        }}
        || <escape>     {{
                            push @nibbles, $¢.makestr(TEXT => $text, _from => $from, _pos => $to ) if $from != $to;
                            push @nibbles, $<escape>[*-1];
                            $text = '';
                            $to = $from = $¢.pos;
                        }}
        || .
                        {{
                            my $ch = substr($*ORIG, $¢.pos-1, 1);
                            $text ~= $ch;
                            $to = $¢.pos;
                            if $ch ~~ "\n" {
                                $multiline++;
                            }
                        }}
        ]
    ]*
    {{
        push @nibbles, $¢.makestr(TEXT => $text, _from => $from, _pos => $to ) if $from != $to or !@nibbles;
        $<nibbles> = \@nibbles;
        $<_pos> = $¢.pos;
        $<nibbler> :delete;
        $<escape> :delete;
        $<starter> :delete;
        $<stopper> :delete;
        $*LAST_NIBBLE = $¢;
        $*LAST_NIBBLE_MULTILINE = $¢ if $multiline;
    }}
}

# and this is what makes nibbler polymorphic...
method nibble ($lang) {
    self.cursor_fresh($lang).nibbler;
}

token p5quote:sym<' '>   { "'" <nibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:q).unbalanced("'"))> "'" }
token p5quote:sym<" ">   { '"' <nibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:qq).unbalanced('"'))> '"' }

token p5circumfix:sym«< >»   { '<'
                              <nibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:qq).tweak(:w).balanced('<','>'))> '>' }

token p5quote:sym</ />   {
    '/' <nibble( $¢.cursor_fresh( %*LANG<Regex> ).unbalanced("/") )> [ '/' || <.panic: "Unable to parse regex; couldn't find final '/'"> ]
    <p5rx_mods>?
}

# handle composite forms like qww
token quote:qq {
    'qq'
    [
    | » <.ws> <quibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:qq))>
    ]
}
token quote:q {
    'q'
    [
    | » <.ws> <quibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:q))>
    ]
}

token quote:qr {
    <sym> » <!before '('>
    <quibble( $¢.cursor_fresh( %*LANG<Regex> ) )>
    <p5rx_mods>
}

token quote:m  {
    <sym> » <!before '('>
    <quibble( $¢.cursor_fresh( %*LANG<Regex> ) )>
    <p5rx_mods>
}

token quote:s {
    <sym> » <!before '('>
    <pat=sibble( $¢.cursor_fresh( %*LANG<Regex> ), $¢.cursor_fresh( %*LANG<Q> ).tweak(:qq))>
    <p5rx_mods>
}

token quote:tr {
    <sym> » <!before '('> <pat=tribble( $¢.cursor_fresh( %*LANG<Q> ).tweak(:q))>
    <p5tr_mods>
}

token p5rx_mods {
    <!after \s>
    (< i g s m x c e >+) 
}

token p5tr_mods {
    (< c d s ] >+) 
}

# assumes whitespace is eaten already

method peek_delimiters {
    my $pos = self.pos;
    my $startpos = $pos;
    my $char = substr($*ORIG,$pos++,1);
    if $char ~~ /^\s$/ {
        self.panic("Whitespace character is not allowed as delimiter"); # "can't happen"
    }
    elsif $char ~~ /^\w$/ {
        self.panic("Alphanumeric character is not allowed as delimiter");
    }
    elsif %STD::close2open{$char} {
        self.panic("Use of a closing delimiter for an opener is reserved");
    }

    my $rightbrack = %STD::open2close{$char};
    if not defined $rightbrack {
        return $char, $char;
    }
    while substr($*ORIG,$pos,1) eq $char {
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

token charname {
    [
    | <radint>
    | <[a..z A..Z]><-[ \] , \# ]>*?<[a..z A..Z ) ]> <?before \s*<[ \] , \# ]>>
    ] || <.panic: "Unrecognized character name">
}

token charnames { [<.ws><charname><.ws>] +% ',' }

token charspec {
    [
    | :dba('character name') '[' ~ ']' <charnames>
    | \d+
    | <[ ?..Z \\.._ ]>
    | <?> <.panic: "Unrecognized \\c character">
    ]
}

method truly ($bool,$opt) {
    return self if $bool;
    self.panic("Cannot negate $opt adverb");
}

grammar Q is STD {

    role b1 {
        token p5escape:sym<\\> { <sym> <item=p5backslash> }
        token p5backslash:qq { <?before 'q'> { $<quote> = $¢.cursor_fresh(%*LANG<MAIN>).quote(); } }
        token p5backslash:sym<\\> { <text=sym> }
        token p5backslash:stopper { <text=stopper> }
        token p5backslash:a { <sym> }
        token p5backslash:b { <sym> }
        token p5backslash:c { <sym> <charspec> }
        token p5backslash:e { <sym> }
        token p5backslash:f { <sym> }
        token p5backslash:n { <sym> }
        token p5backslash:o { :dba('octal character') <sym> [ <octint> | '[' ~ ']' <octints> ] }
        token p5backslash:r { <sym> }
        token p5backslash:t { <sym> }
        token p5backslash:x { :dba('hex character') <sym> [ <hexint> | '[' ~ ']' <hexints> ] }
        token p5backslash:sym<0> { <sym> }
    } # end role

    role b0 {
        token p5escape:sym<\\> { <!> }
    } # end role

    role c1 {
        token p5escape:sym<{ }> { <?before '{'> [ :lang(%*LANG<MAIN>) <block> ] }
    } # end role

    role c0 {
        token p5escape:sym<{ }> { <!> }
    } # end role

    role s1 {
        token p5escape:sym<$> {
            :my $*QSIGIL ::= '$';
            <?before '$'>
            [ :lang(%*LANG<MAIN>) <EXPR(item %methodcall)> ] || <.panic: "Non-variable \$ must be backslashed">
        }
    } # end role

    role s0 {
        token p5escape:sym<$> { <!> }
    } # end role

    role a1 {
        token p5escape:sym<@> {
            :my $*QSIGIL ::= '@';
            <?before '@'>
            [ :lang(%*LANG<MAIN>) <EXPR(item %methodcall)> | <!> ] # trap ABORTBRANCH from variable's ::
        }
    } # end role

    role a0 {
        token p5escape:sym<@> { <!> }
    } # end role

    role h1 {
        token p5escape:sym<%> {
            :my $*QSIGIL ::= '%';
            <?before '%'>
            [ :lang(%*LANG<MAIN>) <EXPR(item %methodcall)> | <!> ]
        }
    } # end role

    role h0 {
        token p5escape:sym<%> { <!> }
    } # end role

    role f1 {
        token p5escape:sym<&> {
            :my $*QSIGIL ::= '&';
            <?before '&'>
            [ :lang(%*LANG<MAIN>) <EXPR(item %methodcall)> | <!> ]
        }
    } # end role

    role f0 {
        token p5escape:sym<&> { <!> }
    } # end role

    role w1 {
        method postprocess ($s) { $s.words }
    } # end role

    role w0 {
        method postprocess ($s) { $s }
    } # end role

    role ww1 {
        method postprocess ($s) { $s.words }
    } # end role

    role ww0 {
        method postprocess ($s) { $s }
    } # end role

    role x1 {
        method postprocess ($s) { $s.run }
    } # end role

    role x0 {
        method postprocess ($s) { $s }
    } # end role

    role q {
        token stopper { \' }

        token p5escape:sym<\\> { <sym> <item=p5backslash> }

        token p5backslash:qq { <?before 'q'> { $<quote> = $¢.cursor_fresh(%*LANG<MAIN>).quote(); } }
        token p5backslash:sym<\\> { <text=sym> }
        token p5backslash:stopper { <text=stopper> }

        # in single quotes, keep backslash on random character by default
        token p5backslash:misc { {} (.) { $<text> = "\\" ~ $0.Str; } }

        # begin tweaks (DO NOT ERASE)
        multi method tweak (:single(:$q)!) { self.panic("Too late for :q") }
        multi method tweak (:double(:$qq)!) { self.panic("Too late for :qq") }
        # end tweaks (DO NOT ERASE)

    } # end role

    role qq does b1 does c1 does s1 does a1 does h1 does f1 {
        token stopper { \" }
        # in double quotes, omit backslash on random \W backslash by default
        token p5backslash:misc { {} [ (\W) { $<text> = $0.Str; } | $<x>=(\w) <.panic("Unrecognized backslash sequence: '\\" ~ $<x>.Str ~ "'")> ] }

        # begin tweaks (DO NOT ERASE)
        multi method tweak (:single(:$q)!) { self.panic("Too late for :q") }
        multi method tweak (:double(:$qq)!) { self.panic("Too late for :qq") }
        # end tweaks (DO NOT ERASE)

    } # end role

    role p5 {
        # begin tweaks (DO NOT ERASE)
        multi method tweak (:$g!) { self }
        multi method tweak (:$i!) { self }
        multi method tweak (:$m!) { self }
        multi method tweak (:$s!) { self }
        multi method tweak (:$x!) { self }
        multi method tweak (:$p!) { self }
        multi method tweak (:$c!) { self }
        # end tweaks (DO NOT ERASE)
    } # end role

    # begin tweaks (DO NOT ERASE)

    multi method tweak (:single(:$q)!) { self.truly($q,':q'); self.mixin( ::q ); }

    multi method tweak (:double(:$qq)!) { self.truly($qq, ':qq'); self.mixin( ::qq ); }

    multi method tweak (:backslash(:$b)!)   { self.mixin($b ?? ::b1 !! ::b0) }
    multi method tweak (:scalar(:$s)!)      { self.mixin($s ?? ::s1 !! ::s0) }
    multi method tweak (:array(:$a)!)       { self.mixin($a ?? ::a1 !! ::a0) }
    multi method tweak (:hash(:$h)!)        { self.mixin($h ?? ::h1 !! ::h0) }
    multi method tweak (:function(:$f)!)    { self.mixin($f ?? ::f1 !! ::f0) }
    multi method tweak (:closure(:$c)!)     { self.mixin($c ?? ::c1 !! ::c0) }

    multi method tweak (:exec(:$x)!)        { self.mixin($x ?? ::x1 !! ::x0) }
    multi method tweak (:words(:$w)!)       { self.mixin($w ?? ::w1 !! ::w0) }
    multi method tweak (:quotewords(:$ww)!) { self.mixin($ww ?? ::ww1 !! ::ww0) }

    multi method tweak (:heredoc(:$to)!) { self.truly($to, ':to'); self.cursor_herelang; }

    multi method tweak (:$regex!) {
        return %*LANG<Regex>;
    }

    multi method tweak (:$trans!) {
        return %*LANG<Trans>;
    }

    multi method tweak (*%x) {
        my @k = keys(%x);
        self.panic("Unrecognized quote modifier: " ~ join('',@k));
    }
    # end tweaks (DO NOT ERASE)


} # end grammar

###########################
# Captures and Signatures #
###########################

token capterm {
    '\\'
    [
    | '(' <capture>? ')'
    | <?before \S> <termish>
    ]
}

rule capture {
    :my $*INVOCANT_OK = 1;
    <EXPR>
}

rule param_sep { [','|':'|';'|';;'] }

token signature ($lexsig = 0) {
    # XXX incorrectly scopes &infix:<x> parameters to outside following block
    :my $*IN_DECL = 1;
    :my $*zone = 'posreq';
    :my $startpos = self.pos;
    <.ws>
    [
    | <?before '-->' | ')' | ']' | '{' | ':'\s >
    | [ <parameter> || <.panic: "Malformed parameter"> ]
    ] +% <param_sep>
    <.ws>
    { $*IN_DECL = 0; }
    [ '-->' <.ws> <typename> ]?
    {{
        $*LEFTSIGIL = '@';
        if $lexsig {
            $*CURLEX.<$?SIGNATURE> ~= '(' ~ substr($*ORIG, $startpos, $¢.pos - $startpos) ~ ')';
            $*CURLEX.<!NEEDSIG>:delete;
        }
    }}
}

token type_constraint {
    <typename>
    <.ws>
}

rule p5statement_prefix:do      {<sym> <block> }
rule p5statement_prefix:eval    {<sym> <block> }

#########
# Terms #
#########

# start playing with the setting stubber

token p5term:sym<undef> {
    <sym> »
    <O(|%term)>
}

token p5term:sym<continue>
    { <sym> » <O(|%term)> }

token p5circumfix:sigil
    { :dba('contextualizer') <sigil=p5sigil> '(' ~ ')' <semilist> { $*LEFTSIGIL ||= $<sigil>.Str } <O(|%term)> }

token p5circumfix:sym<( )>
    { :dba('parenthesized expression') '(' ~ ')' <semilist> <O(|%term)> }

token p5circumfix:sym<[ ]>
    { :dba('array composer') '[' ~ ']' <semilist> <O(|%term)> }

#############
# Operators #
#############

token PRE {
    :dba('prefix or meta-prefix')
    [
    | <prefix=p5prefix>
        { $<O> = $<prefix><O>; $<sym> = $<prefix><sym> }
    ]
    # XXX assuming no precedence change
    
    <.ws>
}

token infixish ($in_meta = $*IN_META) {
    :my $*IN_META = $in_meta;
    <!stdstopper>
    <!infixstopper>
    :dba('infix or meta-infix')
    <infix=p5infix>
    { $<O> = $<infix>.<O>; $<sym> = $<infix>.<sym>; }
}

token p5dotty:sym«->» {
    <sym> <dottyop>
<O(|%methodcall)> }

token dottyopish {
    <term=dottyop>
}

token dottyop {
    :dba('dotty method or postfix')
    [
    | <methodop>
    | <!alpha> <postcircumfix=p5postcircumfix> { $<O> = $<postcircumfix><O>; $<sym> = $<postcircumfix><sym>; }
    ]
}

# Note, this rule mustn't do anything irreversible because it's used
# as a lookahead by the quote interpolator.

token POST {
    <!stdstopper>

    # last whitespace didn't end here
    <!{ @*MEMOS[$¢.pos]<ws> }>

    :dba('postfix')
    [
    | <dotty=p5dotty>  { $<O> = $<dotty><O>;  $<sym> = $<dotty><sym>;  $<~CAPS> = $<dotty><~CAPS>; }
    | <postop> { $<O> = $<postop><O>; $<sym> = $<postop><sym>; $<~CAPS> = $<postop><~CAPS>; }
    ]
}

token p5postcircumfix:sym<( )>
    { :dba('argument list') '(' ~ ')' <semiarglist> <O(|%methodcall)> }

token p5postcircumfix:sym<[ ]>
    { :dba('subscript') '[' ~ ']' <semilist>  <O(|%methodcall)> }

token p5postcircumfix:sym<{ }>
    { :dba('subscript') '{' ~ '}' <semilist> <O(|%methodcall)> }

token postop {
    | <postfix=p5postfix>         { $<O> := $<postfix><O>; $<sym> := $<postfix><sym>; }
    | <postcircumfix=p5postcircumfix>   { $<O> := $<postcircumfix><O>; $<sym> := $<postcircumfix><sym>; }
}

token methodop {
    [
    | <longname>
    | <?before '$' | '@' | '&' > <variable> { $*VAR = $<variable> }
    ]

    :dba('method arguments')
    [
    | <?[\\(]> <args>
    ]?
}

token semiarglist {
    <arglist> +% ';'
    <.ws>
}

token arglist {
    :my $inv_ok = $*INVOCANT_OK;
    :my StrPos $*endargs = 0;
    :my $*GOAL ::= 'endargs';
    :my $*QSIGIL ::= '';
    <.ws>
    :dba('argument list')
    [
    | <?stdstopper>
    | <EXPR(item %listop)> {{
            my $delims = $<EXPR><delims>;
            for @$delims {
                if ($_.<sym> // '') eq ':' {
                    if $inv_ok {
                        $*INVOCANT_IS = $<EXPR><list>[0];
                    }
                }
            }
        }}
    ]
}

token p5circumfix:sym<{ }> {
    <?before '{' >
    <pblock>
<O(|%term)> }

## methodcall

token p5postfix:sym['->'] ()
    { '->' }

## autoincrement
token p5postfix:sym<++>
    { <sym> <O(|%autoincrement)> }

token p5postfix:sym«--»
    { <sym> <O(|%autoincrement)> }

token p5prefix:sym<++>
    { <sym> <O(|%autoincrement)> }

token p5prefix:sym«--»
    { <sym> <O(|%autoincrement)> }

## exponentiation
token p5infix:sym<**>
    { <sym> <O(|%exponentiation)> }

## symbolic unary
token p5prefix:sym<!>
    { <sym> <O(|%symbolic_unary)> }

token p5prefix:sym<+>
    { <sym> <O(|%symbolic_unary)> }

token p5prefix:sym<->
    { <sym> <O(|%symbolic_unary)> }

token p5prefix:sym<~>
    { <sym> <O(|%symbolic_unary)> }


## binding
token p5infix:sym<!~>
    { <sym> <O(|%binding)> }

token p5infix:sym<=~>
    { <sym> <O(|%binding)> }


## multiplicative
token p5infix:sym<*>
    { <sym> <O(|%multiplicative)> }

token p5infix:sym</>
    { <sym> <O(|%multiplicative)> }

token p5infix:sym<%>
    { <sym> <O(|%multiplicative)> }

token p5infix:sym« << »
    { <sym> <O(|%multiplicative)> }

token p5infix:sym« >> »
    { <sym> <O(|%multiplicative)> }

token p5infix:sym<x>
    { <sym> <O(|%multiplicative)> }


## additive
token p5infix:sym<.> ()
    { <sym> <O(|%additive)> }

token p5infix:sym<+>
    { <sym> <O(|%additive)> }

token p5infix:sym<->
    { <sym> <O(|%additive)> }

## bitwise and (all)
token p5infix:sym<&>
    { <sym> <O(|%bitwise_and)> }

token p5infix:sym<also>
    { <sym> <O(|%bitwise_and)> }


## bitwise or (any)
token p5infix:sym<|>
    { <sym> <O(|%bitwise_or)> }

token p5infix:sym<^>
    { <sym> <O(|%bitwise_or)> }


## named unary examples
# (need \s* to win LTM battle with listops)
token p5prefix:sleep
    { <sym> » <?before \s*> <O(|%named_unary)> }

token p5prefix:abs
    { <sym> » <?before \s*> <O(|%named_unary)> }

token p5prefix:let
    { <sym> » <?before \s*> <O(|%named_unary)> }

token p5prefix:temp
    { <sym> » <?before \s*> <O(|%named_unary)> }

## comparisons
token p5infix:sym« <=> »
    { <sym> <?{ $<O><returns> = "Order"; }> <O(|%comparison)> }

token p5infix:cmp
    { <sym> <?{ $<O><returns> = "Order"; }> <O(|%comparison)> }


token p5infix:sym« < »
    { <sym> <O(|%comparison)> }

token p5infix:sym« <= »
    { <sym> <O(|%comparison)> }

token p5infix:sym« > »
    { <sym> <O(|%comparison)> }

token p5infix:sym« >= »
    { <sym> <O(|%comparison)> }

token p5infix:sym<eq>
    { <sym> <O(|%equality)> }

token p5infix:sym<ne>
    { <sym> <O(|%equality)> }

token p5infix:sym<lt>
    { <sym> <O(|%comparison)> }

token p5infix:sym<le>
    { <sym> <O(|%comparison)> }

token p5infix:sym<gt>
    { <sym> <O(|%comparison)> }

token p5infix:sym<ge>
    { <sym> <O(|%comparison)> }

## equality
token p5infix:sym<==>
    { <sym> <!before '=' > <O(|%equality)> }

token p5infix:sym<!=>
    { <sym> <?before \s> <O(|%equality)> }

## tight and
token p5infix:sym<&&>
    { <sym> <O(|%tight_and)> }


## tight or
token p5infix:sym<||>
    { <sym> <O(|%tight_or)> }

token p5infix:sym<^^>
    { <sym> <O(|%tight_or)> }

token p5infix:sym<//>
    { <sym> <O(|%tight_or)> }

## range
token p5infix:sym<..>
    { <sym> <O(|%range)> }

token p5infix:sym<...>
    { <sym> <O(|%range)> }

## conditional
token p5infix:sym<? :> {
    :my $*GOAL ::= ':';
    '?'
    <.ws>
    <EXPR(item %assignment)>
    [ ':' ||
        [
        || <?before '='> <.panic: "Assignment not allowed within ?:">
        || <?before '!!'> <.panic: "Please use : rather than !!">
        || <?before <infixish>>    # Note: a tight infix would have parsed right
            <.panic: "Precedence too loose within ?:; use ?(): instead ">
        || <.panic: "Found ? but no :; possible precedence problem">
        ]
    ]
    { $<O><_reducecheck> = 'raise_middle'; }
<O(|%conditional)> }

method raise_middle {
    self.<middle> = self.<infix><EXPR>;
    self;
}

token p5infix:sym<=> ()
    { <sym> <O(|%assignment)> }

## list item separator
token p5infix:sym<,>
    { <sym> { $<O><fiddly> = 0; } <O(|%comma)> }

token p5infix:sym« => »
    { <sym> { $<O><fiddly> = 0; } <O(|%comma)> }

# force identifier(), identifier.(), etc. to be a function call always
token p5term:identifier
{
    :my $name;
    :my $pos;
    <identifier> <?before [<unsp>|'(']? >
    { $name = $<identifier>.Str; $pos = $¢.pos; }
    <args( $¢.is_name($name) )>
    { self.add_mystery($name,$pos,substr($*ORIG,$pos,1)) unless $<args><invocant>; }
<O(|%term)>  }

token p5term:opfunc
{
    <category> <colonpair>+ <args>
<O(|%term)>  }

token args ($istype = 0) {
    :my $listopish = 0;
    :my $*GOAL ::= '';
    :my $*INVOCANT_OK = 1;
    :my $*INVOCANT_IS;
    [
#    | :dba('argument list') '.(' ~ ')' <semiarglist>
    | :dba('argument list') '(' ~ ')' <semiarglist>
    | :dba('argument list') <.unsp> '(' ~ ')' <semiarglist>
    |  { $listopish = 1 } [<?before \s> <!{ $istype }> <.ws> <!infixstopper> <arglist>]?
    ]
    { $<invocant> = $*INVOCANT_IS; }

    :dba('extra arglist after (...):')
    [
    || <?{ $listopish }>
    || ':' <?before \s> <moreargs=arglist>    # either switch to listopiness
    || {{ $<O> = {}; }}   # or allow adverbs (XXX needs hoisting?)
    ]
}

# names containing :: may or may not be function calls
# bare identifier without parens also handled here if no other rule parses it
token p5term:name
{
    :my $name;
    :my $pos;
    <longname>
    {
        $name = $<longname>.Str;
        $pos = $¢.pos;
    }
    [
    ||  <?{
            $¢.is_name($<longname>.Str) or substr($<longname>.Str,0,2) eq '::'
        }>
        # parametric type?
        <.unsp>? [ <?before '['> <postcircumfix=p5postcircumfix> ]?
        :dba('type parameter')
        [
            '::'
            <?before [ '«' | '<' | '{' | '<<' ] > <postcircumfix=p5postcircumfix>
        ]?

    # unrecognized names are assumed to be post-declared listops.
    || <args> { self.add_mystery($name,$pos,'termish') unless $<args><invocant>; }
    ]
<O(|%term)> }

## loose and
token p5infix:sym<and>
    { <sym> <O(|%loose_and)> }

token p5infix:sym<andthen>
    { <sym> <O(|%loose_and)> }

## loose or
token p5infix:sym<or>
    { <sym> <O(|%loose_or)> }

token p5infix:sym<orelse>
    { <sym> <O(|%loose_or)> }

token p5infix:sym<xor>
    { <sym> <O(|%loose_or)> }

## expression terminator
# Note: must always be called as <?terminator> or <?before ...<p5terminator>...>

token p5terminator:sym<;>
    { ';' <O(|%terminator)> }

token p5terminator:sym<if>
    { 'if' » <.nofun> <O(|%terminator)> }

token p5terminator:sym<unless>
    { 'unless' » <.nofun> <O(|%terminator)> }

token p5terminator:sym<while>
    { 'while' » <.nofun> <O(|%terminator)> }

token p5terminator:sym<until>
    { 'until' » <.nofun> <O(|%terminator)> }

token p5terminator:sym<for>
    { 'for' » <.nofun> <O(|%terminator)> }

token p5terminator:sym<given>
    { 'given' » <.nofun> <O(|%terminator)> }

token p5terminator:sym<when>
    { 'when' » <.nofun> <O(|%terminator)> }

token p5terminator:sym<)>
    { <sym> <O(|%terminator)> }

token p5terminator:sym<]>
    { ']' <O(|%terminator)> }

token p5terminator:sym<}>
    { '}' <O(|%terminator)> }

token p5terminator:sym<:>
    { ':' <?{ $*GOAL eq ':' }> <O(|%terminator)> }

regex infixstopper {
    :dba('infix stopper')
    [
    | <?before <stopper> >
    | <?before ':' > <?{ $*GOAL eq ':' }>
    | <?{ $*GOAL eq 'endargs' and @*MEMOS[$¢.pos]<endargs> }>
    ]
}

# overridden in subgrammars
token stopper { <!> }

# hopefully we can include these tokens in any outer LTM matcher
regex stdstopper {
    :temp @*STUB = return self if @*MEMOS[self.pos]<endstmt> :exists;
    :dba('standard stopper')
    [
    | <?terminator>
    | <?unitstopper>
    | $                                 # unlikely, check last (normal LTM behavior)
    ]
    { @*MEMOS[$¢.pos]<endstmt> ||= 1; }
}


## vim: expandtab sw=4 ft=perl6

grammar Regex is STD {

    # begin tweaks (DO NOT ERASE)
    multi method tweak (:global(:$g)!) { self }
    multi method tweak (:ignorecase(:$i)!) { self }
    # end tweaks (DO NOT ERASE)

    token category:p5metachar { <sym> }
    proto token p5metachar { <...> }

    token category:p5backslash { <sym> }
    proto token p5backslash { <...> }

    token category:p5assertion { <sym> }
    proto token p5assertion { <...> }

    token category:p5quantifier { <sym> }
    proto token p5quantifier { <...> }

    token category:p5mod_internal { <sym> }
    proto token p5mod_internal { <...> }

    proto token p5regex_infix { <...> }

    # suppress fancy end-of-line checking
    token codeblock {
        :my $*GOAL ::= '}';
        '{' :: [ :lang($¢.cursor_fresh(%*LANG<MAIN>)) <statementlist> ]
        [ '}' || <.panic: "Unable to parse statement list; couldn't find right brace"> ]
    }

    token ws {
        <?{ $*RX<s> }>
        || [ <?before \s | '#'> <.nextsame> ]?   # still get all the pod goodness, hopefully
    }

    rule nibbler {
        :temp $*ignorecase;
        <EXPR>
    }

    token termish {
        <.ws>  # XXX assuming old /x here?
        <term=quant_atom_list>
    }
    token quant_atom_list {
        <quantified_atom>+
    }
    token infixish {
        <!infixstopper>
        <!stdstopper>
        <regex_infix=p5regex_infix>
        {
            $<O> = $<regex_infix><O>;
            $<sym> = $<regex_infix><sym>;
        }
    }
    regex infixstopper {
        :dba('infix stopper')
        <?before <stopper> >
    }

    token p5regex_infix:sym<|> { <sym> <O(|%tight_or)>  }

    token quantified_atom {
        <!stopper>
        <!p5regex_infix>
        <atom>
        [ <.ws> <quantifier=p5quantifier>
#            <?{ $<atom>.max_width }>
#                || <.panic: "Cannot quantify zero-width atom">
        ]?
        <.ws>
    }

    token atom {
        [
        | \w
        | <metachar=p5metachar>
        | '\\' :: .
        ]
    }

    # sequence stoppers
    token p5metachar:sym<|>   { '|'  :: <fail> }
    token p5metachar:sym<)>   { ')'  :: <fail> }

    token p5metachar:quant { <quantifier=p5quantifier> <.panic: "quantifier quantifies nothing"> }

    # "normal" metachars

    token p5metachar:sym<[ ]> {
        <before '['> <quibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:q))> # XXX parse as q[] for now
    }

    token p5metachar:sym«(? )» {
        '(?' {} <assertion=p5assertion>
        [ ')' || <.panic: "Perl 5 regex assertion not terminated by parenthesis"> ]
    }

    token p5metachar:sym<( )> {
        '(' {} [:lang(self.unbalanced(')')) <nibbler>]?
        [ ')' || <.panic: "Unable to parse Perl 5 regex; couldn't find right parenthesis"> ]
        { $/<sym> := <( )> }
    }

    token p5metachar:sym<\\> { <sym> <backslash=p5backslash> }
    token p5metachar:sym<.>  { <sym> }
    token p5metachar:sym<^>  { <sym> }
    token p5metachar:sym<$>  {
        '$' <?before \W | $>
    }

    token p5metachar:var {
        <?before <sigil=p5sigil>\w>
        <.panic: "Cannot interpolate variable in Perl 5 regex">
    }

    token p5backslash:A { <sym> }
    token p5backslash:a { <sym> }
    token p5backslash:b { :i <sym> }
    token p5backslash:c { :i <sym>
        <[ ?.._ ]> || <.panic: "Unrecognized \\c character">
    }
    token p5backslash:d { :i <sym> }
    token p5backslash:e { :i <sym> }
    token p5backslash:f { :i <sym> }
    token p5backslash:h { :i <sym> }
    token p5backslash:l { :i <sym> }
    token p5backslash:n { :i <sym> }
    token p5backslash:o { :dba('octal character') '0' [ <octint> | '{' ~ '}' <octints> ] }
    token p5backslash:p { :i <sym> '{' <[\w:]>+ '}' }
    token p5backslash:Q { <sym> }
    token p5backslash:r { :i <sym> }
    token p5backslash:s { :i <sym> }
    token p5backslash:t { :i <sym> }
    token p5backslash:u { :i <sym> }
    token p5backslash:v { :i <sym> }
    token p5backslash:w { :i <sym> }
    token p5backslash:x { :i :dba('hex character') <sym> [ <hexint> | '{' ~ '}' <hexints> ] }
    token p5backslash:z { :i <sym> }
    token p5backslash:misc { $<litchar>=(\W) | $<number>=(\d+) }
    token p5backslash:oops { <.panic: "Unrecognized Perl 5 regex backslash sequence"> }

    token p5assertion:sym<?> { <sym> <codeblock> }
    token p5assertion:sym<{ }> { <codeblock> }

    token p5assertion:sym«<» { <sym> <?before '=' | '!'> <assertion=p5assertion> }
    token p5assertion:sym<=> { <sym> [ <?before ')'> | <rx> ] }
    token p5assertion:sym<!> { <sym> [ <?before ')'> | <rx> ] }
    token p5assertion:sym«>» { <sym> <rx> }

    token rx {
        # [:lang(self.unbalanced(')')) <nibbler>]
        <nibbler>
        [ <?before ')'> || <.panic: "Unable to parse Perl 5 regex; couldn't find right parenthesis"> ]
    }

    #token p5assertion:identifier { <longname> [               # is qq right here?
    #                                | <?before ')' >
    #                                | <.ws> <nibbler>
    #                               ]
    #                               [ ':' <rx> ]?
    #}
    token p5mod { <[imox]>* }
    token p5mods { <on=p5mod> [ '-' <off=p5mod> ]? }
    token p5assertion:mod { <mods=p5mods> [               # is qq right here?
                                   | ':' <rx>?
                                   | <?before ')' >
                                   ]
    }

    token p5assertion:bogus { <.panic: "Unrecognized Perl 5 regex assertion"> }

    token p5quantifier:sym<*>  { <sym> <quantmod> }
    token p5quantifier:sym<+>  { <sym> <quantmod> }
    token p5quantifier:sym<?>  { <sym> <quantmod> }
    token p5quantifier:sym<{ }> { '{' \d+ [','\d*]? '}' <quantmod> }

    token quantmod { [ '?' | '+' ]? }

} # end grammar

