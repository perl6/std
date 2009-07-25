grammar STD:ver<6.0.0.alpha>:auth<http://perl.org>;

use DEBUG;

# per parse
my $ACTIONS is context;         # class or object which defines reduce actions
my $SETTINGNAME is context;     # name of core setting
my $TMP_PREFIX is context;      # where to put tmp files
my $ORIG is context;            # the original program string
my @ORIG is context;            # same thing as individual chars
my @MEMOS is context;           # per-position info such as ws and line number
my $HIGHWATER is context;      # where we were last looking for things
my $HIGHMESS is context;       # current parse failure message
my $HIGHEXPECT is context;     # things we were looking for at the bleeding edge

# symbol table management
my $CORE is context;            # the CORE scope
my $SETTING is context;         # the SETTING scope
my $GLOBAL is context;          # the GLOBAL scope
my $PROCESS is context;         # the PROCESS scope
my $UNIT is context;            # the UNIT scope
my $CURPAD is context<rw>;      # current lexical scope
my $CURPKG is context;          # current package scope
my $PKGNAME is context = "";    # full package name of package assoc with current lexical scope
my @PKGS is context<rw> = ();   # stack of outer lexical packages

my %MYSTERY is context<rw>;     # names we assume may be post-declared functions

# tree attributes, marked as propagating up (u) down (d) or up-and-down (u/d)
my %LANG is context;            # (d) braided languages: MAIN, Q, Regex, etc

my $IN_DECL is context<rw>;     # (d) a declarator is looking for a name to declare
my $SCOPE is context = "";      # (d) which scope declarator we're under
my $MULTINESS is context;       # (d) which multi declarator we're under
my $PKGDECL is context;         # (d) current package declarator
my $DECLARAND is context<rw>;   # (u/d) new object associated with declaration

my $GOAL is context = "(eof)";  # (d) which special terminator we're most wanting
my $IN_REDUCE is context<rw>;   # (d) attempting to parse an [op] construct
my $IN_META is context<rw>;     # (d) parsing a metaoperator like [..]
my $QUASIMODO is context<rw>;   # (d) don't carp about quasi variables
my $LEFTSIGIL is context<rw>;   # (u) sigil of LHS for item vs list assignment
my $QSIGIL is context<rw>;      # (d) sigil of current interpolation

my $INVOCANT_OK is context<rw>; # (d) parsing a list that allows an invocant
my $INVOCANT_IS is context<rw>; # (u) invocant of args match

my $BORG is context;            # (u/d) who to blame if we're missing a block

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

Note that rules automatically get an implicit {*} at their return, so
for the TOP rule the implicit action name is also simply "TOP".

Another nod toward preprocessing is that blocks that contain nested braces
are delimited by double braces so that the preprocessor does not need to
understand Perl 6 code.

This grammar relies on transitive longest-token semantics, though
initially we made a feeble attempt to order rules so a procedural
interpretation of alternation could usually produce a correct parse.
(This is becoming less true over time.)

=end comment overview

method TOP ($STOP = undef) {
    if defined $STOP {
        my $GOAL is context = $STOP;
        self.unitstop($STOP).comp_unit;
    }
    else {
        self.comp_unit;
    }
}

method initparse ($text, :$rule = 'TOP', :$tmp_prefix = '', :$setting = 'CORE', :$actions = '') {
    temp $*TMP_PREFIX = $tmp_prefix;
    temp $*SETTINGNAME = $setting;
    temp $*ACTIONS = $actions;
    temp $*DEBUG = $DEBUG;
    temp @*MEMOS;

    # various bits of info useful for error messages
    temp $*HIGHWATER = 0;
    temp $*HIGHMESS = '';
    temp $*HIGHEXPECT = {};
    temp $*LAST_NIBBLE = { firstline => 0, lastline => 0 };
    temp $*LAST_NIBBLE_MULTILINE = { firstline => 0, lastline => 0 };
    temp $*GOAL = "(eof)";
    $*ORIG = $text ~ "\n;";           # original string

    my $result = self.new($text)."$rule"();

    # XXX here attach stuff that will enable :cont

    $result;
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
constant %multiplicative  = (:dba('multiplicative')  , :prec<u=>, :assoc<left>);
constant %additive        = (:dba('additive')        , :prec<t=>, :assoc<left>);
constant %replication     = (:dba('replication')     , :prec<s=>, :assoc<left>);
constant %concatenation   = (:dba('concatenation')   , :prec<r=>, :assoc<list>);
constant %junctive_and    = (:dba('junctive and')    , :prec<q=>, :assoc<list>);
constant %junctive_or     = (:dba('junctive or')     , :prec<p=>, :assoc<list>);
constant %named_unary     = (:dba('named unary')     , :prec<o=>, :assoc<unary>, :uassoc<left>);
constant %structural      = (:dba('structural infix'), :prec<n=>, :assoc<non>, :diffy);
constant %chaining        = (:dba('chaining')        , :prec<m=>, :assoc<chain>, :diffy, :iffy);
constant %tight_and       = (:dba('tight and')       , :prec<l=>, :assoc<list>);
constant %tight_or        = (:dba('tight or')        , :prec<k=>, :assoc<list>);
constant %conditional     = (:dba('conditional')     , :prec<j=>, :assoc<right>, :fiddly);
constant %item_assignment = (:dba('item assignment') , :prec<i=>, :assoc<right>);
constant %loose_unary     = (:dba('loose unary')     , :prec<h=>, :assoc<unary>, :uassoc<left>);
constant %comma           = (:dba('comma')           , :prec<g=>, :assoc<list>, :nextterm<nulltermish>, :fiddly);
constant %list_infix      = (:dba('list infix')      , :prec<f=>, :assoc<list>);
constant %list_assignment = (:dba('list assignment') , :prec<i=>, :assoc<right>, :sub<e=>, :fiddly);
constant %list_prefix     = (:dba('list prefix')     , :prec<e=>, :assoc<unary>, :uassoc<left>);
constant %loose_and       = (:dba('loose and')       , :prec<d=>, :assoc<list>);
constant %loose_or        = (:dba('loose or')        , :prec<c=>, :assoc<list>);
constant %sequencer       = (:dba('sequencer')       , :prec<b=>, :assoc<list>, :nextterm<statement>, :fiddly);
constant %LOOSEST         = (:dba('LOOSEST')         , :prec<a=!>);
constant %terminator      = (:dba('terminator')      , :prec<a=>, :assoc<list>);

# "epsilon" tighter than terminator
#constant $LOOSEST = %LOOSEST<prec>;
constant $LOOSEST = "a=!"; # XXX preceding line is busted
constant $item_assignment_prec = 'i=';
constant $methodcall_prec = 'y=';


role PrecOp {

    # This is hopefully called on a match to mix in operator info by type.
    method coerce (Match $m) {
        # $m but= ::?CLASS;
        my $var = self.WHAT ~ '::o';
        my $d = %::($var); 
        if not $d<transparent> {
            for keys(%$d) { $m<O>{$_} //= $d.{$_} };
            $m.deb("coercing to " ~ self) if $*DEBUG +& DEBUG::EXPR;
        }
        $m<O><kind> = self.WHAT;
        return $m;
    }

} # end role

class Transparent does PrecOp {
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
class Structural does PrecOp {
    our %o = %structural;
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
class Sequencer does PrecOp {
    our %o = %sequencer;
} # end class
class Terminator does PrecOp {
    our %o = %terminator;
} # end class

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

token category:noun { <sym> }
proto token noun { <...> }

token category:value { <sym> }
proto token value { <...> }

token category:term { <sym> }
proto token term { <...> }

token category:number { <sym> }
proto token number { <...> }

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

token category:trait_mod { <sym> }
proto token trait_mod (:$endsym is context = 'spacey') { <...> }

token category:type_declarator { <sym> }
proto token type_declarator (:$endsym is context = 'spacey') { <...> }

token category:scope_declarator { <sym> }
proto token scope_declarator (:$endsym is context = 'nofun') { <...> }

token category:package_declarator { <sym> }
proto token package_declarator (:$endsym is context = 'spacey') { <...> }

token category:multi_declarator { <sym> }
proto token multi_declarator (:$endsym is context = 'spacey') { <...> }

token category:routine_declarator { <sym> }
proto token routine_declarator (:$endsym is context = 'endid') { <...> }

token category:regex_declarator { <sym> }
proto token regex_declarator (:$endsym is context = 'spacey') { <...> }

token category:statement_prefix { <sym> }
proto rule  statement_prefix () { <...> }

token category:statement_control { <sym> }
proto rule  statement_control (:$endsym is context = 'spacey') { <...> }

token category:statement_mod_cond { <sym> }
proto rule  statement_mod_cond (:$endsym is context = 'nofun') { <...> }

token category:statement_mod_loop { <sym> }
proto rule  statement_mod_loop (:$endsym is context = 'nofun') { <...> }

token category:infix_prefix_meta_operator { <sym> }
proto token infix_prefix_meta_operator is binary { <...> }

token category:infix_postfix_meta_operator { <sym> }
proto token infix_postfix_meta_operator ($op) is binary { <...> }

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
token endid { <?before <-[ \- \' \w ]> > }
token spacey { <?before <[ \s \# ]> > }
token nofun { <!before '(' | '.(' | '\\' | '\'' | '-' > }

##################
# Lexer routines #
##################

token ws {
    :my @stub = return self if @*MEMOS[self.pos]<ws> :exists;
    :my $startpos = self.pos;

    :dba('whitespace')
    [
        | \h+ <![#\s\\]> { @*MEMOS[$¢.pos]<ws> = $startpos; }   # common case
        | <?before \w> <?after \w> :::
            { @*MEMOS[$startpos]<ws> = undef; }
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
            @*MEMOS[$¢.pos]<ws> = undef;
        }
        else {
            @*MEMOS[$¢.pos]<ws> = $startpos;
            @*MEMOS[$¢.pos]<endstmt> = @*MEMOS[$startpos]<endstmt>
                if @*MEMOS[$startpos]<endstmt> :exists;
        }
    }}
}

token unsp {
    \\ <?before [\s|'#'] >
    :dba('unspace')
    [
    | <.vws>                     {*}                             #= vwhite
    | <.unv>                  {*}                                #= unv
    | $ { $¢.moreinput }
    ]*
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
   | \h+                 {*}                                    #= hwhite
   | <?before '='> ^^ <.pod_comment>  {*}                    #= pod
   | \h* '#' [
         |  <?opener>
            [ <!after ^^ . > || <.panic: "Can't use embedded comments in column 1"> ]
            <.quibble($¢.cursor_fresh( %*LANG<Q> ))>   {*}                               #= embedded
         | {} \N*            {*}                                 #= end
         ]
    ]
}

token ident {
    <.alpha> \w*
}

token apostrophe {
    <[ ' \- ]>
}

token identifier {
    <.ident> [ <.apostrophe> <.ident> ]*
}

# XXX We need to parse the pod eventually to support $= variables.

token pod_comment {
    ^^ '=' <.unsp>?
    [
    | 'begin' \h+ <identifier> ::
        [
        ||  .*? "\n=" <.unsp>? 'end' \h+ $<identifier> » \N*          {*} #= tagged
        ||  <?{ $<identifier>.Str eq 'END'}> .*                           {*} #= end
        || { my $id = $<identifier>.Str; self.panic("=begin $id without matching =end $id"); }
        ]
    | 'begin' » :: \h* [ $$ || '#' || <.panic: "Unrecognized token after =begin"> ]
        [ .*?  "\n=" <.unsp>? 'end' » \N* || { self.panic("=begin without matching =end"); } ]   {*}       #= anon
        
    | 'for' » :: \h* [ <identifier> || $$ || '#' || <.panic: "Unrecognized token after =for"> ]
        [.*?  ^^ \h* $$ || .*]
    | :: 
        [ <?before .*? ^^ '=cut' » > <.panic: "Obsolete pod format, please use =begin/=end instead"> ]?
        [<alpha>||\s||<.panic: "Illegal pod directive">]
        \N*                                           {*}       #= misc
    ]
}

###################
# Top-level rules #
###################

# Note: we only check for the stopper.  We don't check for ^ because
# we might be embedded in something else.
rule comp_unit {
    :my $begin_compunit is context = 1;
    :my $endargs        is context<rw> = -1;
    :my %LANG is context;
    :my $PKGDECL is context = "";
    :my $PKGNAME is context = "GLOBAL";
    :my @PKGS is context<rw> = ();
    :my $IN_DECL is context<rw>;
    :my $DECLARAND is context<rw>;
    :my $QSIGIL is context<rw> = '';
    :my $IN_META is context<rw> = 0;
    :my $QUASIMODO is context<rw>;
    :my $SCOPE is context = "";
    :my $LEFTSIGIL is context<rw>;
    :my %MYSTERY is context<rw> = ();
    :my $INVOCANT_OK is context<rw>;
    :my $INVOCANT_IS is context<rw>;
    :my $CURPAD is context<rw>;
    :my $MULTINESS is context = '';

    :my $CURPKG is context;
    {{

        %*LANG<MAIN>    = ::STD ;
        %*LANG<Q>       = ::STD::Q ;
        %*LANG<Quasi>   = ::STD::Quasi ;
        %*LANG<Regex>   = ::STD::Regex ;
        %*LANG<Trans>   = ::STD::Trans ;
        %*LANG<P5Regex> = ::STD::P5Regex ;

        @*WORRIES = ();
        self.load_setting($*SETTINGNAME);
        self.newpad;
        $*UNIT = $*CURPAD;
        self.finishpad(1);
    }}
    <statementlist>
    [ <?unitstopper> || <.panic: "Confused"> ]
#    { $<CORE> = $*CORE; }
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
        my $p = %*MYSTERY{$_}.<pad>;
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
            $m ~= "\t$_ used at " ~ %post_types{$_}.<line> ~ "\n";
        }
    }
    if %unk_types {
        my @tmp = sort keys(%unk_types);
        $m ~= "Undeclared name" ~ ('s' x (@tmp != 1)) ~ ":\n";
        for @tmp {
            $m ~= "\t$_ used at " ~ %unk_types{$_}.<line> ~ "\n";
        }
    }
    if %unk_routines {
        my @tmp = sort keys(%unk_routines);
        $m ~= "Undeclared routine" ~ ('s' x (@tmp != 1)) ~ ":\n";
        for @tmp {
            $m ~= "\t$_ used at " ~ %unk_routines{$_}.<line> ~ "\n";
        }
    }
    $m;
}

# Note: because of the possibility of placeholders we can't determine arity of
# the block syntactically, so this must be determined via semantic analysis.
# Also, pblocks used in an if/unless statement do not treat $_ as a placeholder,
# while most other blocks treat $_ as equivalent to $^x.  Therefore the first
# possible place to check arity is not here but in the rule that calls this
# rule.  (Could also be done in a later pass.)

token pblock ($CURPAD is context<rw> = $*CURPAD) {
    :dba('parameterized block')
    [<?before <.lambda> | '{' > ||
        {{
            if $*BORG and $*BORG.<block> {
                if $*BORG.<name> {
                    my $m = "Function '" ~ $BORG.<name> ~ "' needs parens to avoid gobbling block" ~ $*BORG.<culprit>.locmess;
                    $*BORG.<block>.panic($m ~ "\nMissing block (apparently gobbled by '" ~ $BORG.<name> ~ "')");
                }
                else {
                    my $m = "Expression needs parens to avoid gobbling block" ~ $*BORG.<culprit>.locmess;
                    $*BORG.<block>.panic($m ~ "\nMissing block (apparently gobbled by expression)");
                }
            }
            elsif %*MYSTERY {
                $¢.panic("Missing block (apparently gobbled by undeclared routine?)");
            }
            else {
                $¢.panic("Missing block");
            }
        }}
    ]
    [
    | <lambda>
        <.newpad>
        <signature>
        <blockoid>
    | <?before '{'>
        <.newpad>
        <blockoid>
    ]
}

token lambda { '->' | '<->' }

# Look for an expression followed by a required lambda.
token xblock {
    :my $GOAL is context = '{';
    :my $BORG is context = {};
    <EXPR>
    { $BORG.<culprit> //= $<EXPR>.cursor(self.pos) }
    <.ws>
    <pblock>
}

token block ($CURPAD is context<rw> = $*CURPAD) {
    :dba('scoped block')
    [ <?before '{' > || <.panic: "Missing block"> ]
    <.newpad>
    <blockoid>
}

token blockoid {
    # temporize braided languages
    :temp %*LANG;

    <.finishpad>
    [
    | '{' ~ '}' <statementlist>
    | <?terminator> <.panic: 'Missing block'>
    | <?> <.panic: "Malformed block">
    ]

    [
    | <?before \h* $$>  # (usual case without comments)
        { @*MEMOS[$¢.pos]<endstmt> = 2; } {*}                    #= endstmt simple 
    | \h* <?before <[\\,:]>> {*}                         #= normal 
    | <.unv>? $$
        { @*MEMOS[$¢.pos]<endstmt> = 2; } {*}                    #= endstmt complex
    | {} <.unsp>? { @*MEMOS[$¢.pos]<endargs> = 1; } {*}             #= endargs
    ]
}

token regex_block {
    # temporize braided languages
    :temp %*LANG;

    :my $lang = %*LANG<Regex>;
    :my $GOAL is context = '}';

    [ <quotepair> <.ws>
        {
            my $kv = $<quotepair>[*-1];
            $lang = $lang.tweak($kv.<k>, $kv.<v>)
                or self.panic("Unrecognized adverb :" ~ $kv.<k> ~ '(' ~ $kv.<v> ~ ')');
        }
    ]*

    '{'
    <nibble( $¢.cursor_fresh($lang).unbalanced('}') )>
    [ '}' || <.panic: "Unable to parse regex; couldn't find right brace"> ]

    [
    | <?before \h* $$>  # (usual case without comments)
        { @*MEMOS[$¢.pos]<endstmt> = 2; } {*}                    #= endstmt simple 
    | \h* <?before <[\\,:]>> {*}                         #= normal 
    | <.unv>? $$
        { @*MEMOS[$¢.pos]<endstmt> = 2; } {*}                    #= endstmt complex
    | {} <.unsp>? { @*MEMOS[$¢.pos]<endargs> = 1; }   {*}           #= endargs
    ]
}

# statement semantics
rule statementlist {
    :my $INVOCANT_OK is context<rw> = 0;
    :dba('statement list')

    [
    | $
    | <?before <[\)\]\}]> >
    | [<statement><eat_terminator> ]*
    ]
}

# embedded semis, context-dependent semantics
rule semilist {
    :my $INVOCANT_OK is context<rw> = 0;
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
      <.panic("Illegal redeclaration of '$label'")>
    ]?

    # add label as a pseudo type
    {{ $¢.add_my_name($label); }}

}

token statement {
    :my $endargs is context = -1;
    :my $QSIGIL is context<rw> = 0;
    <!before <[\)\]\}]> >

    # this could either be a statement that follows a declaration
    # or a statement that is within the block of a code declaration
    <!!{ $¢ = %*LANG<MAIN>.bless($¢); }>

    [
    | <label> <statement>                        {*}            #= label
    | <statement_control>                        {*}            #= control
    | <EXPR> {*}                                                #= expr
        :dba('statement end')
        [
        || <?{ (@*MEMOS[$¢.pos]<endstmt> // 0) == 2 }>   # no mod after end-line curly
        ||
            :dba('statement modifier')
            <.ws>
            [
            | <statement_mod_loop> {*}                              #= mod loop
                {{
                    my $sp = $<EXPR><statement_prefix>;
                    if $sp and $sp<sym> eq 'do' {
                       my $s = $<statement_mod_loop>[0]<sym>;
                       $¢.obs("do...$s" ,"repeat...$s");
                    }
                }}
            | <statement_mod_cond> {*}                              #= mod cond
                :dba('statement modifier loop')
                [
                || <?{ (@*MEMOS[$¢.pos]<endstmt> // 0) == 2 }>
                || <.ws> <statement_mod_loop>? {*}                  #= mod condloop
                ]
            ]?
        ]
        {*}                                                     #= modexpr
    | <?before ';'> {*}                                         #= null
    ]

    # Is there more on same line after a block?
    [ <?{ (@*MEMOS[@*MEMOS[$¢.pos]<ws>//$¢.pos]<endargs>//0) == 1 }>
        \h*
        <!before ';' | ')' | ']' | '}' >
        <!infixstopper>
        <.panic: "Statements must be separated with semicolon">
    ]?
}

token eat_terminator {
    [
    || ';'
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

token statement_control:need {
    :my $longname;
    <sym>:s
    [
    |<version>
    |<module_name>
        {{
            my $SCOPE is context = 'use';
            $longname = $<module_name>[*-1]<longname>.Str;
            $¢.do_need($longname);
        }}
    ] ** ','
}

token statement_control:use {
    :my $longname;
    :my $SCOPE is context = 'use';
    <sym> <.ws>
    [
    | <version>
    | <module_name>
        {{
            $longname = $<module_name><longname>.Str;
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


token statement_control:no {
    <sym> <.ws>
    <module_name>[<.spacey><arglist>]?
    <.ws>
}


token statement_control:if {
    <sym> :s
    <xblock>
    [
        [ <!before 'else'\s*'if'> || <.panic: "Please use 'elsif'"> ]
        'elsif'<?spacey> <elsif=xblock>       {*}                #= elsif
    ]*
    [
        'else'<?spacey> <else=pblock>       {*}             #= else
    ]?
}


token statement_control:unless {
    <sym> :s
    <xblock>
    [ <!before 'else'> || <.panic: "unless does not take \"else\" in Perl 6; please rewrite using \"if\""> ]
}


token statement_control:while {
    <sym> :s
    [ <?before '(' ['my'? '$'\w+ '=']? '<' '$'?\w+ '>' ')'>   #'
        <.panic: "This appears to be Perl 5 code"> ]?
    <xblock>
}


token statement_control:until {
    <sym> :s
    <xblock>
}


token statement_control:repeat {
    <sym> :s
    [
        | ('while'|'until')
          <xblock>
        | <block>                      {*}                      #= block wu
          ('while'|'until') <EXPR>         {*}                      #= expr wu
    ]
}

token statement_control:loop {
    <sym> :s
    $<eee> = (
        '('
            <e1=EXPR>? ';'   {*}                            #= e1
            <e2=EXPR>? ';'   {*}                            #= e2
            <e3=EXPR>?       {*}                            #= e3
        ')'                      {*}                            #= eee
    )?
    <block>                     {*}                             #= block
}


token statement_control:for {
    <sym> :s
    [ <?before 'my'? '$'\w+ '(' >
        <.panic: "This appears to be Perl 5 code"> ]?
    [ <?before '(' <.EXPR>? ';' <.EXPR>? ';' <.EXPR>? ')' >
        <.obs('C-style "for (;;)" loop', '"loop (;;)"')> ]?
    <xblock>
}

token statement_control:given {
    <sym> :s
    <xblock>
}
token statement_control:when {
    <sym> :s
    <xblock>
}
rule statement_control:default {<sym> <block> }

token statement_prefix:BEGIN   { <sym> <blorst> }
token statement_prefix:CHECK   { <sym> <blorst> }
token statement_prefix:INIT    { <sym> <blorst> }
token statement_prefix:START   { <sym> <blorst> }
token statement_prefix:ENTER   { <sym> <blorst> }
token statement_prefix:FIRST   { <sym> <blorst> }

rule statement_control:END     {<sym> <block> }
rule statement_control:LEAVE   {<sym> <block> }
rule statement_control:KEEP    {<sym> <block> }
rule statement_control:UNDO    {<sym> <block> }
rule statement_control:NEXT    {<sym> <block> }
rule statement_control:LAST    {<sym> <block> }
rule statement_control:PRE     {<sym> <block> }
rule statement_control:POST    {<sym> <block> }
rule statement_control:CATCH   {<sym> <block> }
rule statement_control:CONTROL {<sym> <block> }
rule statement_control:TEMP    {<sym> <block> }

#######################
# statement modifiers #
#######################

rule modifier_expr { <EXPR> }

rule statement_mod_cond:if     {<sym> <modifier_expr> }
rule statement_mod_cond:unless {<sym> <modifier_expr> }
rule statement_mod_cond:when   {<sym> <modifier_expr> }

rule statement_mod_loop:while {<sym> <modifier_expr> }
rule statement_mod_loop:until {<sym> <modifier_expr> }

rule statement_mod_loop:for   {<sym> <modifier_expr> }
rule statement_mod_loop:given {<sym> <modifier_expr> }

################
# module names #
################

token def_module_name {
    <longname>
    [ :dba('generic role')
        <?before '['>
        <?{ ($*PKGDECL//'') eq 'role' }>
        <.newpad>
        '[' ~ ']' <signature>
        { $*IN_DECL = 0; }
        <.finishpad>
    ]?
}

token module_name:normal {
    <longname>
    [ <?before '['> :dba('generic role') '[' ~ ']' <arglist> ]?
}

token module_name:deprecated { 'v6-alpha' }

token vnum {
    \d+ | '*'
}

token version:sym<v> {
    'v' <?before \d+> :: <vnum> ** '.' '+'?
}

###############
# Declarators #
###############

token constant_declarator {
    :my $IN_DECL is context<rw> = 1;
    :my $DECLARAND is context<rw>;
    <?{ $*SCOPE eq 'constant' }>
    <identifier> <.ws> <?before '='|'is'\s>
    { $*IN_DECL = 0; self.add_name($<identifier>.Str) }

    <trait>*
}

token variable_declarator {
    :my $IN_DECL is context<rw> = 1;
    :my $DECLARAND is context<rw>;
    <variable>
    { $*IN_DECL = 0; self.add_variable($<variable>.Str) }
    [   # Is it a shaped array or hash declaration?
      #  <?{ $<sigil> eq '@' | '%' }>
        <.unsp>?
        $<shape> = [
        | '(' ~ ')' <signature>
        | :dba('shape definition') '[' ~ ']' <semilist>
        | :dba('shape definition') '{' ~ '}' <semilist>
        | <?before '<'> <postcircumfix>
        ]*
    ]?
    <.ws>

    <trait>*
}

rule scoped {
    :dba('scoped declarator')
    [
    | <declarator>
    | <regex_declarator>
    | <package_declarator>
    | <typename>+
        {
            my $t = $<typename>;
            @$t > 1 and $¢.panic("Multiple prefix constraints not yet supported")
        }
        <multi_declarator>
    | <multi_declarator>
    ]
    || <?before <[A..Z]>><longname>{{
            my $t = $<longname>.Str;
            if not $¢.is_known($t) {
                $¢.panic("In \"$*SCOPE\" declaration, typename $t must be predeclared (or marked as declarative with :: prefix)");
            }
        }}
        <!> # drop through
    || <.panic: "Malformed $*SCOPE">
}


token scope_declarator:my        { <sym> { $*SCOPE = $<sym> } <scoped> }
token scope_declarator:our       { <sym> { $*SCOPE = $<sym> } <scoped> }
token scope_declarator:state     { <sym> { $*SCOPE = $<sym> } <scoped> }
token scope_declarator:constant  { <sym> { $*SCOPE = $<sym> } <scoped> }
token scope_declarator:has       { <sym> { $*SCOPE = $<sym> } <scoped> }
token scope_declarator:augment   { <sym> { $*SCOPE = $<sym> } <scoped> }
token scope_declarator:supersede { <sym> { $*SCOPE = $<sym> } <scoped> }


token package_declarator:class {
    :my $PKGDECL is context = 'class';
    <sym> <package_def>
}

token package_declarator:grammar {
    :my $PKGDECL is context = 'grammar';
    <sym> <package_def>
}

token package_declarator:module {
    :my $PKGDECL is context = 'module';
    <sym> <package_def>
}

token package_declarator:package {
    :my $PKGDECL is context = 'package';
    <sym> <package_def>
}

token package_declarator:role {
    :my $PKGDECL is context = 'role';
    <sym> <package_def>
}

token package_declarator:knowhow {
    :my $PKGDECL is context = 'knowhow';
    <sym> <package_def>
}

token package_declarator:slang {
    :my $PKGDECL is context = 'slang';
    <sym> <package_def>
}

token package_declarator:require {   # here because of declarational aspects
    <sym> <.ws>
    [
    || <module_name> <EXPR>?
    || <EXPR>
    ]
}

token package_declarator:trusts {
    <sym> <.ws>
    <module_name>
}

token package_declarator:does {
    <sym> <.ws>
    <typename>
}

rule package_def {
    :my $longname;
    :my $IN_DECL is context<rw> = 1;
    :my $DECLARAND is context<rw>;
    [
        [
            <def_module_name>{
                $longname = $<def_module_name>[0]<longname>;
                $¢.add_name($longname.Str);
            }
        ]?
        <trait>*
        [
        || <?before '{'>
            {{
                # figure out the actual full package name (nested in outer package)
                my $pkg = $*PKGNAME || "GLOBAL";
                my $shortname;
                if $longname {
                     $shortname = $longname.<name>.Str;
                }
                else {
                    $shortname = '_anon_';
                }
                $*PKGNAME = $pkg ~ '::' ~ $shortname;
                my $newpkg = $*CURPKG.{$shortname ~ '::'};
                $newpkg.<PARENT::> = $*CURPKG;
                $*CURPKG = $newpkg;
                push @PKGS, $pkg;
                say "adding $newpkg " ~ $*PKGNAME if $*DEBUG +& DEBUG::symtab;
            }}

            <block>
            {{
                $*PKGNAME = pop(@PKGS);
                $*CURPKG = $*CURPKG.<PARENT::>;
            }}
            {*}                                                     #= block
        || <?before ';'>
            [
            || <?{ $*begin_compunit }>
                {{
                    $longname orelse $¢.panic("Compilation unit cannot be anonymous");
                    my $shortname = $longname.<name>.Str;
                    $*PKGNAME = $shortname;
                    my $newpkg = $*CURPKG.{$shortname ~ '::'};
                    $newpkg.<PARENT::> = $*CURPKG;
                    $*CURPKG = $newpkg;
                    $*begin_compunit = 0;
                }}
                {*}                                                     #= semi
            || <.panic: "Too late for semicolon form of " ~ $*PKGDECL ~ " definition">
            ]
        || <.panic: "Unable to parse " ~ $*PKGDECL ~ " definition">
        ]
    ] || <.panic: "Malformed $*PKGDECL">
}

token declarator {
    [
    | <constant_declarator>
    | <variable_declarator>
    | '(' ~ ')' <signature> <trait>*
    | <routine_declarator>
    | <regex_declarator>
    | <type_declarator>
    ]
}

token multi_declarator:multi {
    :my $MULTINESS is context = 'multi';
    <sym> <.ws> [ <declarator> || <routine_def> || <.panic: 'Malformed multi'> ]
}
token multi_declarator:proto {
    :my $MULTINESS is context = 'proto';
    <sym> <.ws> [ <declarator> || <routine_def> || <.panic: 'Malformed proto'> ]
}
token multi_declarator:only {
    :my $MULTINESS is context = 'only';
    <sym> <.ws> [ <declarator> || <routine_def> || <.panic: 'Malformed only'> ]
}
token multi_declarator:null {
    :my $MULTINESS is context = '';
    <declarator>
}

token routine_declarator:sub       { <sym> <routine_def> }
token routine_declarator:method    { <sym> <method_def> }
token routine_declarator:submethod { <sym> <method_def> }
token routine_declarator:macro     { <sym> <macro_def> }

token regex_declarator:regex { <sym>       <regex_def> }
token regex_declarator:token { <sym>       <regex_def> }
token regex_declarator:rule  { <sym>       <regex_def> }

rule multisig {
    :dba('signature')
    [
        ':'?'(' ~ ')' <signature>
    ]
    ** '|'
}

rule routine_def ($CURPAD is context<rw> = $*CURPAD) {
    :my $IN_DECL is context<rw> = 1;
    :my $DECLARAND is context<rw>;
    [
        [ '&'<deflongname>? | <deflongname> ]?
        <.newpad>
        [ <multisig> | <trait> ]*
        <!{
            $*IN_DECL = 0;
        }>
        <blockoid>:!s
    ] || <.panic: "Malformed routine">
}

rule method_def ($CURPAD is context<rw> = $*CURPAD) {
    :my $IN_DECL is context<rw> = 1;
    :my $DECLARAND is context<rw>;
    <.newpad>
    [
        [
        | <[ ! ^ ]>?<longname> [ <multisig> | <trait> ]*
        | <multisig> <trait>*
        | <sigil> '.'
            :dba('subscript signature')
            [
            | '(' ~ ')' <signature>
            | '[' ~ ']' <signature>
            | '{' ~ '}' <signature>
            | <?before '<'> <postcircumfix>
            ]
            <trait>*
        | <?>
        ]
        <blockoid>:!s
    ] || <.panic: "Malformed method">
}

rule regex_def ($CURPAD is context<rw> = $*CURPAD) {
    :my $IN_DECL is context<rw> = 1;
    :my $DECLARAND is context<rw>;
    [
        [ '&'<deflongname>? | <deflongname> ]?
        <.newpad>
        [ [ ':'?'(' <signature> ')'] | <trait> ]*
        { $*IN_DECL = 0; }
        <.finishpad>
        <regex_block>:!s
    ] || <.panic: "Malformed regex">
}

rule macro_def ($CURPAD is context<rw> = $*CURPAD) {
    :my $IN_DECL is context<rw> = 1;
    :my $DECLARAND is context<rw>;
    [
        [ '&'<deflongname>? | <deflongname> ]?
        <.newpad>
        [ <multisig> | <trait> ]*
        <!{
            $*IN_DECL = 0;
        }>
        <blockoid>:!s
    ] || <.panic: "Malformed macro">
}

rule trait {
    :my $IN_DECL is context<rw> = 0;
    [
    | <trait_mod>
    | <colonpair>
    ]
}

token trait_mod:is {
    <sym>:s <longname><circumfix>?  # e.g. context<rw> and Array[Int]
    {{
        if $*DECLARAND {
            my $traitname = $<longname>.Str;
            # XXX eventually will use multiple dispatch
            $*DECLARAND{$traitname} = self.gettrait($traitname, $<circumfix>);
        }
    }}
}
token trait_mod:hides {
    <sym>:s <module_name>
}
token trait_mod:does {
    :my $PKGDECL is context = 'role';
    <sym>:s <module_name>
}
token trait_mod:will {
    <sym>:s <identifier> <block>
}

token trait_mod:of      { <sym>:s <typename> }
token trait_mod:as      { <sym>:s <typename> }
token trait_mod:returns { <sym>:s <typename> }
token trait_mod:handles { <sym>:s <noun> }

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
    | <noun=termish>
        {
            $¢.<PRE>  = $<noun><PRE>:delete;
            $¢.<POST> = $<noun><POST>:delete;
            $¢.<~CAPS> = $<noun><~CAPS>;
        }
    | <?>
    ]
}

token termish {
    :my $SCOPE is context<rw> = "our";
    :my $VAR is context<rw>;
    :dba('prefix or noun')
    [
    | <PRE> [ <!{ my $p = $<PRE>; my @p = @$p; @p[*-1]<O><noun> and $<noun> = pop @$p }> <PRE> ]*
        [ <?{ $<noun> }> || <noun> ]
    | <noun>
    ]

    # also queue up any postfixes
    :dba('postfix')
    [
    || <?{ $*QSIGIL }>
        [
        || <?{ $*QSIGIL eq '$' }> [ <POST>+! <?after <[ \] } > ) ]> > ]?
        ||                          <POST>+! <?after <[ \] } > ) ]> > 
        || { $VAR = 0; }
        ]
    || <!{ $*QSIGIL }>
        <POST>*
    ]
    {
        self.check_variable($VAR) if $VAR;
        $¢.<~CAPS> = $<noun><~CAPS>;
    }
}

token noun:fatarrow           { <fatarrow> }
token noun:variable           { <variable> { $*VAR = $<variable> } }
token noun:package_declarator { <package_declarator> }
token noun:scope_declarator   { <scope_declarator> }
token noun:multi_declarator   { <?before 'multi'|'proto'|'only'> <multi_declarator> }
token noun:routine_declarator { <routine_declarator> }
token noun:regex_declarator   { <regex_declarator> }
token noun:type_declarator    { <type_declarator> }
token noun:circumfix          { <circumfix> }
token noun:dotty              { <dotty> }
token noun:value              { <value> }
token noun:capterm            { <capterm> }
token noun:sigterm            { <sigterm> }
token noun:term               { <term> }
token noun:statement_prefix   { <statement_prefix> }
token noun:colonpair          { [ <colonpair> <.ws> ]+ }

token fatarrow {
    <key=identifier> \h* '=>' <.ws> <val=EXPR(item %item_assignment)>
}

token colonpair {
    :my $key;
    :my $value;

    ':'
    :dba('colon pair')
    [
    | '!' <identifier>
        { $key = $<identifier>.Str; $value = 0; }
        {*}                                                     #= false
    | $<num> = [\d+] <identifier>
    | <identifier>
        { $key = $<identifier>.Str; }
        [
        || <.unsp>? <circumfix> { $value = $<circumfix>; }
        || { $value = 1; }
        ]
        {*}                                                     #= value
    | :dba('signature') '(' ~ ')' <fakesignature>
    | <circumfix>
        { $key = ""; $value = $<circumfix>; }
        {*}                                                     #= structural
    | $<var> = (<sigil> {} <twigil>? <desigilname>)
        { $key = $<var><desigilname>.Str; $value = $<var>; }
        {*}                                                     #= varname
    ]
    { $<k> = $key; $<v> = $value; }
}

token quotepair {
    :my $key;
    :my $value;

    ':'
    :dba('colon pair (restricted)')
    [
    | '!' <identifier>
        { $key = $<identifier>.Str; $value = 0; }
        {*}                                                     #= false
    | <identifier>
        { $key = $<identifier>.Str; }
        [
        || <.unsp>? <?before '('> <circumfix> { $value = $<circumfix>; }
        || { $value = 1; }
        ]
        {*}                                                     #= value
    | $<n>=(\d+) $<id>=(<[a..z]>+)
        { $key = $<id>.Str; $value = $<n>.Str; }
        {*}                                                     #= nth
    ]
    { $<k> = $key; $<v> = $value; }
}

# Most of these special variable rules are there simply to catch old p5 brainos

token special_variable:sym<$¢> { <sym> }

token special_variable:sym<$!> { <sym> <!before \w> }

token special_variable:sym<$!{ }> {
    ( '$!{' :: (.*?) '}' )
    <.obs($0.Str ~ " variable", 'smart match against $!')>
}

token special_variable:sym<$/> {
    <sym>
    # XXX assuming nobody ever wants to assign $/ directly anymore...
    [ <?before \h* '=' <![=]> >
        <.obs('$/ variable as input record separator',
             "filehandle's :irs attribute")>
    ]?
}

token special_variable:sym<$~> {
    <sym> :: <?before \s | ',' | '=' | <terminator> >
    <.obs('$~ variable', 'Form module')>
}

token special_variable:sym<$`> {
    <sym> :: <?before \s | ',' | <terminator> >
    <.obs('$` variable', 'explicit pattern before <(')>
}

token special_variable:sym<$@> {
    <sym> ::
    <.obs('$@ variable as eval error', '$!')>
}

token special_variable:sym<$#> {
    <sym> ::
    [
    || (\w+) <.obs("\$#" ~ $0.Str ~ " variable", '@' ~ $0.Str ~ '.end')>
    || <.obs('$# variable', '.fmt')>
    ]
}
token special_variable:sym<$$> {
    <sym> <!alpha> :: <?before \s | ',' | <terminator> >
    <.obs('$$ variable', '$*PID')>
}
token special_variable:sym<$%> {
    <sym> ::
    <.obs('$% variable', 'Form module')>
}

# Note: this works because placeholders are restricted to lowercase
token special_variable:sym<$^X> {
    <sigil> '^' $<letter> = [<[A..Z]>] \W
    <.obscaret($<sigil>.Str ~ '^' ~ $<letter>.Str, $<sigil>.Str, $<letter>.Str)>
}

token special_variable:sym<$^> {
    <sym> :: <?before \s | ',' | '=' | <terminator> >
    <.obs('$^ variable', 'Form module')>
}

token special_variable:sym<$&> {
    <sym> :: <?before \s | ',' | <terminator> >
    <.obs('$& variable', '$/ or $()')>
}

token special_variable:sym<$*> {
    <sym> :: <?before \s | ',' | '=' | <terminator> >
    <.obs('$* variable', '^^ and $$')>
}

token special_variable:sym<$)> {
    <sym> :: <?before \s | ',' | <terminator> >
    <.obs('$) variable', '$*EGID')>
}

token special_variable:sym<$-> {
    <sym> :: <?before \s | ',' | '=' | <terminator> >
    <.obs('$- variable', 'Form module')>
}

token special_variable:sym<$=> {
    <sym> :: <?before \s | ',' | '=' | <terminator> >
    <.obs('$= variable', 'Form module')>
}

token special_variable:sym<@+> {
    <sym> :: <?before \s | ',' | <terminator> >
    <.obs('@+ variable', '.to method')>
}

token special_variable:sym<%+> {
    <sym> :: <?before \s | ',' | <terminator> >
    <.obs('%+ variable', '.to method')>
}

token special_variable:sym<$+[ ]> {
    '$+['
    <.obs('@+ variable', '.to method')>
}

token special_variable:sym<@+[ ]> {
    '@+['
    <.obs('@+ variable', '.to method')>
}

token special_variable:sym<@+{ }> {
    '@+{'
    <.obs('%+ variable', '.to method')>
}

token special_variable:sym<@-> {
    <sym> :: <?before \s | ',' | <terminator> >
    <.obs('@- variable', '.from method')>
}

token special_variable:sym<%-> {
    <sym> :: <?before \s | ',' | <terminator> >
    <.obs('%- variable', '.from method')>
}

token special_variable:sym<$-[ ]> {
    '$-['
    <.obs('@- variable', '.from method')>
}

token special_variable:sym<@-[ ]> {
    '@-['
    <.obs('@- variable', '.from method')>
}

token special_variable:sym<%-{ }> {
    '@-{'
    <.obs('%- variable', '.from method')>
}

token special_variable:sym<$+> {
    <sym> :: <?before \s | ',' | <terminator> >
    <.obs('$+ variable', 'Form module')>
}

token special_variable:sym<${^ }> {
    ( <sigil> '{^' :: (.*?) '}' )
    <.obscaret($0.Str, $<sigil>.Str, $0.{0}.Str)>
}

# XXX should eventually rely on multi instead of nested cases here...
method obscaret (Str $var, Str $sigil, Str $name) {
    my $repl;
    given $sigil {
        when '$' {
            given $name {
                when 'MATCH'         { $repl = '$/' }
                when 'PREMATCH'      { $repl = 'an explicit pattern before <(' }
                when 'POSTMATCH'     { $repl = 'an explicit pattern after )>' }
                when 'ENCODING'      { $repl = '$?ENCODING' }
                when 'UNICODE'       { $repl = '$?UNICODE' }  # XXX ???
                when 'TAINT'         { $repl = '$*TAINT' }
                when 'OPEN'          { $repl = 'filehandle introspection' }
                when 'N'             { $repl = '$-1' } # XXX ???
                when 'L'             { $repl = 'Form module' }
                when 'A'             { $repl = 'Form module' }
                when 'E'             { $repl = '$!.extended_os_error' }
                when 'C'             { $repl = 'COMPILING namespace' }
                when 'D'             { $repl = '$*DEBUGGING' }
                when 'F'             { $repl = '$*SYSTEM_FD_MAX' }
                when 'H'             { $repl = '$?FOO variables' }
                when 'I'             { $repl = '$*INPLACE' } # XXX ???
                when 'O'             { $repl = '$?OS or $*OS' }
                when 'P'             { $repl = 'whatever debugger Perl 6 comes with' }
                when 'R'             { $repl = 'an explicit result variable' }
                when 'S'             { $repl = 'the context function' } # XXX ???
                when 'T'             { $repl = '$*BASETIME' }
                when 'V'             { $repl = '$*PERL_VERSION' }
                when 'W'             { $repl = '$*WARNING' }
                when 'X'             { $repl = '$*EXECUTABLE_NAME' }
                when *               { $repl = "a global form such as $sigil*$name" }
            }
        }
        when '%' {
            given $name {
                when 'H'             { $repl = '$?FOO variables' }
                when *               { $repl = "a global form such as $sigil*$name" }
            }
        }
        when * { $repl = "a global form such as $sigil*$name" }
    };
    return self.obs("$var variable", $repl);
}

token special_variable:sym<::{ }> {
    '::' <?before '{'>
}

token special_variable:sym<${ }> {
    ( <[$@%]> '{' :: (.*?) '}' )
    <.obs("" ~ $0.Str ~ " variable", "\{" ~ $<sigil>.Str ~ "}(" ~ $0.{0}.Str ~ ")")>
}

token special_variable:sym<$[> {
    <sym> :: <?before \s | ',' | '=' | <terminator> >
    <.obs('$[ variable', 'user-defined array indices')>
}

token special_variable:sym<$]> {
    <sym> :: <?before \s | ',' | <terminator> >
    <.obs('$] variable', '$*PERL_VERSION')>
}

token special_variable:sym<$\\> {
    <sym> :: <?before \s | ',' | '=' | <terminator> >
    <.obs('$\\ variable', "the filehandle's :ors attribute")>
}

token special_variable:sym<$|> {
    <sym> :: <?before \s | ',' | '=' | <terminator> >
    <.obs('$| variable', ':autoflush on open')>
}

token special_variable:sym<$:> {
    <sym> <?before <[\x20\t\n\],=)}]> >
    <.obs('$: variable', 'Form module')>
}

token special_variable:sym<$;> {
    <sym> :: <?before \s | ',' | '=' | <terminator> >
    <.obs('$; variable', 'real multidimensional hashes')>
}

token special_variable:sym<$'> { #'
    <sym> :: <?before \s | ',' | <terminator> >
    <.obs('$' ~ "'" ~ 'variable', "explicit pattern after )\x3E")>
}

token special_variable:sym<$"> {
    <sym> <!{ $*QSIGIL }>
    :: <?before \s | ',' | '=' | <terminator> >
    <.obs('$" variable', '.join() method')>
}

token special_variable:sym<$,> {
    <sym> :: <?before \s | ',' | <terminator> >
    <.obs('$, variable', ".join() method")>
}

token special_variable:sym['$<'] {
    <sym> :: <!before \s* \w+ \s* '>' >
    <.obs('$< variable', '$*UID')>
}

token special_variable:sym«\$>» {
    <sym> :: <?before \s | ',' | <terminator> >
    <.obs('$> variable', '$*EUID')>
}

token special_variable:sym<$.> {
    <sym> :: <?before \s | ',' | <terminator> >
    <.obs('$. variable', "filehandle's .line method")>
}

token special_variable:sym<$?> {
    <sym> :: <?before \s | ',' | <terminator> >
    <.obs('$? variable as child error', '$!')>
}

# desigilname should only follow a sigil/twigil

token desigilname {
    [
    | <?before '$' > <variable> { $*VAR = $<variable> }
    | <longname>
    ]
}

token variable {
    :my $IN_META is context<rw> = 0;
    :my $sigil = '';
    :my $twigil = '';
    :my $name;
    <?before <sigil> {
        $sigil = $<sigil>.Str;
        $*LEFTSIGIL ||= $sigil;
    }> {}
    [
    || <sigil> <twigil>? <?before '::' [ '{' | '<' | '(' ]> <longname> # XXX
    || '&'
        [
        | <twigil>? <sublongname> { $name = $<sublongname>.Str } {*}                                   #= subnoun
        | '[' ~ ']' <infixish(1)>
        ]
    || '$::' <name>? # XXX
    || '$:' <name> # XXX
    || [
        | <sigil> <twigil>? <desigilname> { $name = $<desigilname>.Str } {*}                                    #= desigilname
        | <special_variable> {*}                                    #= special
        | <sigil> $<index>=[\d+] {*}                                #= $0
        # Note: $() can also parse as contextualizer in an expression; should have same effect
        | <sigil> <?before '<' | '('> <postcircumfix> {*}           #= $()
        | <sigil> <?{ $*IN_DECL }> {*}                              #= anondecl
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

    { my $t = $<twigil>; $twigil = $t.[0].Str if @$t; }
    [ <?{ $twigil eq '.' }>
        [<.unsp> | '\\' | <?> ] <?before '('> <postcircumfix> {*}          #= methcall
    ]?
}



# Note, don't reduce on a bare sigil unless you don't want a twigil or
# you otherwise don't care what the longest token is.

token sigil:sym<$>  { <sym> }
token sigil:sym<@@> { <sym> }
token sigil:sym<@>  { <sym> }
token sigil:sym<%>  { <sym> }
token sigil:sym<&>  { <sym> }

token twigil:sym<.> { <sym> }
token twigil:sym<!> { <sym> }
token twigil:sym<^> { <sym> }
token twigil:sym<:> { <sym> <!before ':'> }
token twigil:sym<*> { <sym> }
token twigil:sym<+> { <sym> <!!worry: "The + twigil is deprecated, use the * twigil instead"> }
token twigil:sym<?> { <sym> }
token twigil:sym<=> { <sym> }
token twigil:sym<~> { <sym> }

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
    :my $QSIGIL is context<rw> = '';
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

token value:quote   { <quote> }
token value:number  { <number> }
token value:version { <version> }

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
    <.unsp>? [ <?before '['> <postcircumfix> ]?
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

token number:rational { <nu=integer>'/'<de=integer> }
token number:complex { <re=numish>'+'<im=numish>'\\'?'i' | <im=numish>'\\'?'i' }
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
    <!!before ['.' <?before \s | ',' | '=' | <terminator> > <.panic: "Decimal point must be followed by digit">]? >
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

token rad_number {
    ':' $<radix> = [\d+] <.unsp>?      # XXX optional dot here?
    {}           # don't recurse in lexer
    :dba('number in radix notation')
    [
    || '<'
            $<intpart> = [ <[ 0..9 a..z A..Z ]>+ [ _ <[ 0..9 a..z A..Z ]>+ ]* ]
            $<fracpart> = [ '.' <[ 0..9 a..z A..Z ]>+ [ _ <[ 0..9 a..z A..Z ]>+ ]* ]?
            [ '*' <base=radint> '**' <exp=radint> ]?
       '>'
#      { make radcalc($<radix>, $<intpart>, $<fracpart>, $<base>, $<exp>) }
    || <?before '['> <circumfix>
    || <?before '('> <circumfix>
    ]
}

token octints { [<.ws><octint><.ws>] ** ',' }

token octint {
    <[ 0..7 ]>+ [ _ <[ 0..7 ]>+ ]*
}

token hexints { [<.ws><hexint><.ws>] ** ',' }

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
    token stopper { ^^ {*} $<ws>=(\h*?) $*DELIM \h* <.unv>?? $$ \v? }
} # end role

# XXX be sure to temporize @herestub_queue on reentry to new line of heredocs

method heredoc () {
    temp $*CTX = self.callm if $*DEBUG +& DEBUG::trace_call;
    return if self.peek;
    my $here = self;
    while my $herestub = shift @herestub_queue {
        my $DELIM is context = $herestub.delim;
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
                or self.panic("Unrecognized adverb :" ~ $kv.<k> ~ '(' ~ $kv.<v> ~ ')');
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

    $start <nibble($lang)> $stop

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

    $start <left=nibble($lang)> $stop 
    [ <?{ $start ne $stop }>
        <.ws>
        [ <infixish> || <panic: "Missing assignment operator"> ]
        [ <?{ $<infixish>.Str eq '=' || $<infixish>.<infix_postfix_meta_operator> }> || <.panic: "Malformed assignment operator"> ]
        <.ws>
        <right=EXPR(item %item_assignment)>
    || 
        { $lang = $lang2.unbalanced($stop); }
        <right=nibble($lang)> $stop
    ]
}

token tribble ($l, $lang2 = $l) {
    :my ($lang, $start, $stop);
    <babble($l)>
    { my $B = $<babble><B>; ($lang,$start,$stop) = @$B; }

    $start <left=nibble($lang)> $stop 
    [ <?{ $start ne $stop }>
        <.ws> <quibble($lang2)>
    || 
        { $lang = $lang2.unbalanced($stop); }
        <right=nibble($lang)> $stop
    ]
}

token quasiquibble ($l) {
    :my ($lang, $start, $stop);
    :my $QUASIMODO is context = 0; # :COMPILING sets true
    <babble($l)>
    { my $B = $<babble><B>; ($lang,$start,$stop) = @$B; }

    [
    || <?{ $start eq '{' }> [ :lang($lang) <block> ]
    || $start [ :lang($lang) <statementlist> ] $stop
    ]
}

# note: polymorphic over many quote languages, we hope
token nibbler {
    :my $text = '';
    :my $from = self.pos;
    :my $to = $from;
    :my @nibbles = ();
    :my $multiline = 0;
    :my $nibble;
    { $<_from> = self.pos; }
    [ <!before <stopper> >
        [
        || <starter> <nibbler> <stopper>
                        {{
                            push @nibbles, $¢.cursor_singleton(TEXT => $text, _from => $from, _pos => $to );

                            my $n = $<nibbler>[*-1]<nibbles>;
                            my @n = @$n;

                            push @nibbles, $<starter>;
                            push @nibbles, @n;
                            push @nibbles, $<stopper>;

                            $text = '';
                            $to = $from = $¢.pos;
                        }}
        || <escape>     {{
                            push @nibbles, $¢.cursor_singleton(TEXT => $text, _from => $from, _pos => $to ), $<escape>[*-1];
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
        push @nibbles, $¢.cursor_singleton(TEXT => $text, _from => $from, _pos => $to );
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

token quote:sym<' '>   { "'" <nibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:q).unbalanced("'"))> "'" }
token quote:sym<" ">   { '"' <nibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:qq).unbalanced('"'))> '"' }

token circumfix:sym<« »>   { '«' <nibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:qq).tweak(:ww).balanced('«','»'))> '»' }
token circumfix:sym«<< >>» { '<<' <nibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:qq).tweak(:ww).balanced('<<','>>'))> '>>' }
token circumfix:sym«< >»   { '<'
                              [ <?before 'STDIN>' > <.obs('<STDIN>', '$' ~ '*IN.lines')> ]?  # XXX fake out gimme5
                              [ <?before '>' > <.obs('<>', 'lines() or ()')> ]?
                              <nibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:q).tweak(:w).balanced('<','>'))> '>' }

token quote:sym<//>   {
    '/'\s*'/' <.panic: "Null regex not allowed">
}

token quote:sym</ />   {
    '/' <nibble( $¢.cursor_fresh( %*LANG<Regex> ).unbalanced("/") )> [ '/' || <.panic: "Unable to parse regex; couldn't find final '/'"> ]
    <.old_rx_mods>?
}

# handle composite forms like qww
token quote:qq {
    :my $qm;
    'qq'
    [
    | <quote_mod> » <!before '('> { $qm = $<quote_mod>.Str } <.ws> <quibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:qq).tweak($qm => 1))>
    | » <!before '('> <.ws> <quibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:qq))>
    ]
}
token quote:q {
    :my $qm;
    'q'
    [
    | <quote_mod> » <!before '('> { $qm = $<quote_mod>.Str } <quibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:q).tweak($qm => 1))>
    | » <!before '('> <.ws> <quibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:q))>
    ]
}

token quote:Q {
    :my $qm;
    'Q'
    [
    | <quote_mod> » <!before '('> { $qm = $<quote_mod>.Str } <quibble($¢.cursor_fresh( %*LANG<Q> ).tweak($qm => 1))>
    | » <!before '('> <.ws> <quibble($¢.cursor_fresh( %*LANG<Q> ))>
    ]
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

token quote:rx {
    <sym> » <!before '('>
    <quibble( $¢.cursor_fresh( %*LANG<Regex> ) )>
    <!old_rx_mods>
}

token quote:m  {
    <sym> » <!before '('>
    <quibble( $¢.cursor_fresh( %*LANG<Regex> ) )>
    <!old_rx_mods>
}

token quote:mm {
    <sym> » <!before '('>
    <quibble( $¢.cursor_fresh( %*LANG<Regex> ).tweak(:s))>
    <!old_rx_mods>
}

token quote:s {
    <sym> » <!before '('>
    <pat=sibble( $¢.cursor_fresh( %*LANG<Regex> ), $¢.cursor_fresh( %*LANG<Q> ).tweak(:qq))>
    <!old_rx_mods>
}

token quote:ss {
    <sym> » <!before '('>
    <pat=sibble( $¢.cursor_fresh( %*LANG<Regex> ).tweak(:s), $¢.cursor_fresh( %*LANG<Q> ).tweak(:qq))>
    <!old_rx_mods>
}
token quote:tr {
    <sym> » <!before '('> <pat=tribble( $¢.cursor_fresh( %*LANG<Q> ).tweak(:q))>
    <!old_tr_mods>
}

token old_rx_mods {
    <!after \s>
    (< i g s m x c e >+) 
    {{
        given $0.Str {
            $_ ~~ /i/ and $¢.worryobs('/i',':i');
            $_ ~~ /g/ and $¢.worryobs('/g',':g');
            $_ ~~ /s/ and $¢.worryobs('/s','^^ and $$ anchors');
            $_ ~~ /m/ and $¢.worryobs('/m','. or \N');
            $_ ~~ /x/ and $¢.worryobs('/x','normal default whitespace');
            $_ ~~ /c/ and $¢.worryobs('/c',':c or :p');
            $_ ~~ /e/ and $¢.worryobs('/e','interpolated {...} or s{} = ... form');
            $¢.obs('suffix regex modifiers','prefix adverbs');
        }
    }}
}

token old_tr_mods {
    (< c d s ] >+) 
    {{
        given $0.Str {
            $_ ~~ /c/ and $¢.worryobs('/c',':c');
            $_ ~~ /d/ and $¢.worryobs('/g',':d');
            $_ ~~ /s/ and $¢.worryobs('/s',':s');
            $¢.obs('suffix transliteration modifiers','prefix adverbs');
        }
    }}
}

token quote:quasi {
    <sym> » <!before '('> <quasiquibble($¢.cursor_fresh( %*LANG<Quasi> ))>
}

# XXX should eventually be derived from current Unicode tables.
constant %open2close = (
"\x0028" => "\x0029",
"\x003C" => "\x003E",
"\x005B" => "\x005D",
"\x007B" => "\x007D",
"\x00AB" => "\x00BB",
"\x0F3A" => "\x0F3B",
"\x0F3C" => "\x0F3D",
"\x169B" => "\x169C",
"\x2018" => "\x2019",
"\x201A" => "\x2019",
"\x201B" => "\x2019",
"\x201C" => "\x201D",
"\x201E" => "\x201D",
"\x201F" => "\x201D",
"\x2039" => "\x203A",
"\x2045" => "\x2046",
"\x207D" => "\x207E",
"\x208D" => "\x208E",
"\x2208" => "\x220B",
"\x2209" => "\x220C",
"\x220A" => "\x220D",
"\x2215" => "\x29F5",
"\x223C" => "\x223D",
"\x2243" => "\x22CD",
"\x2252" => "\x2253",
"\x2254" => "\x2255",
"\x2264" => "\x2265",
"\x2266" => "\x2267",
"\x2268" => "\x2269",
"\x226A" => "\x226B",
"\x226E" => "\x226F",
"\x2270" => "\x2271",
"\x2272" => "\x2273",
"\x2274" => "\x2275",
"\x2276" => "\x2277",
"\x2278" => "\x2279",
"\x227A" => "\x227B",
"\x227C" => "\x227D",
"\x227E" => "\x227F",
"\x2280" => "\x2281",
"\x2282" => "\x2283",
"\x2284" => "\x2285",
"\x2286" => "\x2287",
"\x2288" => "\x2289",
"\x228A" => "\x228B",
"\x228F" => "\x2290",
"\x2291" => "\x2292",
"\x2298" => "\x29B8",
"\x22A2" => "\x22A3",
"\x22A6" => "\x2ADE",
"\x22A8" => "\x2AE4",
"\x22A9" => "\x2AE3",
"\x22AB" => "\x2AE5",
"\x22B0" => "\x22B1",
"\x22B2" => "\x22B3",
"\x22B4" => "\x22B5",
"\x22B6" => "\x22B7",
"\x22C9" => "\x22CA",
"\x22CB" => "\x22CC",
"\x22D0" => "\x22D1",
"\x22D6" => "\x22D7",
"\x22D8" => "\x22D9",
"\x22DA" => "\x22DB",
"\x22DC" => "\x22DD",
"\x22DE" => "\x22DF",
"\x22E0" => "\x22E1",
"\x22E2" => "\x22E3",
"\x22E4" => "\x22E5",
"\x22E6" => "\x22E7",
"\x22E8" => "\x22E9",
"\x22EA" => "\x22EB",
"\x22EC" => "\x22ED",
"\x22F0" => "\x22F1",
"\x22F2" => "\x22FA",
"\x22F3" => "\x22FB",
"\x22F4" => "\x22FC",
"\x22F6" => "\x22FD",
"\x22F7" => "\x22FE",
"\x2308" => "\x2309",
"\x230A" => "\x230B",
"\x2329" => "\x232A",
"\x23B4" => "\x23B5",
"\x2768" => "\x2769",
"\x276A" => "\x276B",
"\x276C" => "\x276D",
"\x276E" => "\x276F",
"\x2770" => "\x2771",
"\x2772" => "\x2773",
"\x2774" => "\x2775",
"\x27C3" => "\x27C4",
"\x27C5" => "\x27C6",
"\x27D5" => "\x27D6",
"\x27DD" => "\x27DE",
"\x27E2" => "\x27E3",
"\x27E4" => "\x27E5",
"\x27E6" => "\x27E7",
"\x27E8" => "\x27E9",
"\x27EA" => "\x27EB",
"\x2983" => "\x2984",
"\x2985" => "\x2986",
"\x2987" => "\x2988",
"\x2989" => "\x298A",
"\x298B" => "\x298C",
"\x298D" => "\x298E",
"\x298F" => "\x2990",
"\x2991" => "\x2992",
"\x2993" => "\x2994",
"\x2995" => "\x2996",
"\x2997" => "\x2998",
"\x29C0" => "\x29C1",
"\x29C4" => "\x29C5",
"\x29CF" => "\x29D0",
"\x29D1" => "\x29D2",
"\x29D4" => "\x29D5",
"\x29D8" => "\x29D9",
"\x29DA" => "\x29DB",
"\x29F8" => "\x29F9",
"\x29FC" => "\x29FD",
"\x2A2B" => "\x2A2C",
"\x2A2D" => "\x2A2E",
"\x2A34" => "\x2A35",
"\x2A3C" => "\x2A3D",
"\x2A64" => "\x2A65",
"\x2A79" => "\x2A7A",
"\x2A7D" => "\x2A7E",
"\x2A7F" => "\x2A80",
"\x2A81" => "\x2A82",
"\x2A83" => "\x2A84",
"\x2A8B" => "\x2A8C",
"\x2A91" => "\x2A92",
"\x2A93" => "\x2A94",
"\x2A95" => "\x2A96",
"\x2A97" => "\x2A98",
"\x2A99" => "\x2A9A",
"\x2A9B" => "\x2A9C",
"\x2AA1" => "\x2AA2",
"\x2AA6" => "\x2AA7",
"\x2AA8" => "\x2AA9",
"\x2AAA" => "\x2AAB",
"\x2AAC" => "\x2AAD",
"\x2AAF" => "\x2AB0",
"\x2AB3" => "\x2AB4",
"\x2ABB" => "\x2ABC",
"\x2ABD" => "\x2ABE",
"\x2ABF" => "\x2AC0",
"\x2AC1" => "\x2AC2",
"\x2AC3" => "\x2AC4",
"\x2AC5" => "\x2AC6",
"\x2ACD" => "\x2ACE",
"\x2ACF" => "\x2AD0",
"\x2AD1" => "\x2AD2",
"\x2AD3" => "\x2AD4",
"\x2AD5" => "\x2AD6",
"\x2AEC" => "\x2AED",
"\x2AF7" => "\x2AF8",
"\x2AF9" => "\x2AFA",
"\x2E02" => "\x2E03",
"\x2E04" => "\x2E05",
"\x2E09" => "\x2E0A",
"\x2E0C" => "\x2E0D",
"\x2E1C" => "\x2E1D",
"\x2E20" => "\x2E21",
"\x3008" => "\x3009",
"\x300A" => "\x300B",
"\x300C" => "\x300D",
"\x300E" => "\x300F",
"\x3010" => "\x3011",
"\x3014" => "\x3015",
"\x3016" => "\x3017",
"\x3018" => "\x3019",
"\x301A" => "\x301B",
"\x301D" => "\x301E",
"\xFD3E" => "\xFD3F",
"\xFE17" => "\xFE18",
"\xFE35" => "\xFE36",
"\xFE37" => "\xFE38",
"\xFE39" => "\xFE3A",
"\xFE3B" => "\xFE3C",
"\xFE3D" => "\xFE3E",
"\xFE3F" => "\xFE40",
"\xFE41" => "\xFE42",
"\xFE43" => "\xFE44",
"\xFE47" => "\xFE48",
"\xFE59" => "\xFE5A",
"\xFE5B" => "\xFE5C",
"\xFE5D" => "\xFE5E",
"\xFF08" => "\xFF09",
"\xFF1C" => "\xFF1E",
"\xFF3B" => "\xFF3D",
"\xFF5B" => "\xFF5D",
"\xFF5F" => "\xFF60",
"\xFF62" => "\xFF63",
);

token opener {
  <[
\x0028
\x003C
\x005B
\x007B
\x00AB
\x0F3A
\x0F3C
\x169B
\x2018
\x201A
\x201B
\x201C
\x201E
\x201F
\x2039
\x2045
\x207D
\x208D
\x2208
\x2209
\x220A
\x2215
\x223C
\x2243
\x2252
\x2254
\x2264
\x2266
\x2268
\x226A
\x226E
\x2270
\x2272
\x2274
\x2276
\x2278
\x227A
\x227C
\x227E
\x2280
\x2282
\x2284
\x2286
\x2288
\x228A
\x228F
\x2291
\x2298
\x22A2
\x22A6
\x22A8
\x22A9
\x22AB
\x22B0
\x22B2
\x22B4
\x22B6
\x22C9
\x22CB
\x22D0
\x22D6
\x22D8
\x22DA
\x22DC
\x22DE
\x22E0
\x22E2
\x22E4
\x22E6
\x22E8
\x22EA
\x22EC
\x22F0
\x22F2
\x22F3
\x22F4
\x22F6
\x22F7
\x2308
\x230A
\x2329
\x23B4
\x2768
\x276A
\x276C
\x276E
\x2770
\x2772
\x2774
\x27C3
\x27C5
\x27D5
\x27DD
\x27E2
\x27E4
\x27E6
\x27E8
\x27EA
\x2983
\x2985
\x2987
\x2989
\x298B
\x298D
\x298F
\x2991
\x2993
\x2995
\x2997
\x29C0
\x29C4
\x29CF
\x29D1
\x29D4
\x29D8
\x29DA
\x29F8
\x29FC
\x2A2B
\x2A2D
\x2A34
\x2A3C
\x2A64
\x2A79
\x2A7D
\x2A7F
\x2A81
\x2A83
\x2A8B
\x2A91
\x2A93
\x2A95
\x2A97
\x2A99
\x2A9B
\x2AA1
\x2AA6
\x2AA8
\x2AAA
\x2AAC
\x2AAF
\x2AB3
\x2ABB
\x2ABD
\x2ABF
\x2AC1
\x2AC3
\x2AC5
\x2ACD
\x2ACF
\x2AD1
\x2AD3
\x2AD5
\x2AEC
\x2AF7
\x2AF9
\x2E02
\x2E04
\x2E09
\x2E0C
\x2E1C
\x2E20
\x3008
\x300A
\x300C
\x300E
\x3010
\x3014
\x3016
\x3018
\x301A
\x301D
\xFD3E
\xFE17
\xFE35
\xFE37
\xFE39
\xFE3B
\xFE3D
\xFE3F
\xFE41
\xFE43
\xFE47
\xFE59
\xFE5B
\xFE5D
\xFF08
\xFF1C
\xFF3B
\xFF5B
\xFF5F
\xFF62
  ]>
}

# assumes whitespace is eaten already

method peek_delimiters {
    my $pos = self.pos;
    my $startpos = $pos;
    my $char = substr($*ORIG,$pos++,1);
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
    | <[a..z A..Z]><-[ \] , # ]>*?<[a..z A..Z ) ]> <?before \s*<[ \] , # ]>>
    ] || <.panic: "Unrecognized character name">
}

token charnames { [<.ws><charname><.ws>] ** ',' }

token charspec {
    [
    | '[' ~ ']' <charnames>
    | \d+
    | <[ ?..Z \\.._ ]>
    | <?> <.panic: "Unrecognized \\c character">
    ]
}

method truly ($bool,$opt) {
    return self if $bool;
    self.panic("Can't negate $opt adverb");
}

grammar Q is STD {

    role b1 {
        token escape:sym<\\> { <sym> <item=backslash> }
        token backslash:qq { <?before 'q'> { $<quote> = $¢.cursor_fresh(%*LANG<MAIN>).quote(); } }
        token backslash:sym<\\> { <text=sym> }
        token backslash:stopper { <text=stopper> }
        token backslash:a { <sym> }
        token backslash:b { <sym> }
        token backslash:c { <sym> <charspec> }
        token backslash:e { <sym> }
        token backslash:f { <sym> }
        token backslash:n { <sym> }
        token backslash:o { <sym> [ <octint> | '[' ~ ']' <octints> ] }
        token backslash:r { <sym> }
        token backslash:t { <sym> }
        token backslash:x { <sym> [ <hexint> | '[' ~ ']' <hexints> ] }
        token backslash:sym<0> { <sym> }
    } # end role

    role b0 {
        token escape:sym<\\> { <!> }
    } # end role

    role c1 {
        token escape:sym<{ }> { <?before '{'> [ :lang(%*LANG<MAIN>) <block> ] }
    } # end role

    role c0 {
        token escape:sym<{ }> { <!> }
    } # end role

    role s1 {
        token escape:sym<$> {
            :my $QSIGIL is context = '$';
            <?before '$'>
            [ :lang(%*LANG<MAIN>) <EXPR(item %methodcall)> ] || <.panic: "Non-variable \$ must be backslashed">
        }
    } # end role

    role s0 {
        token escape:sym<$> { <!> }
    } # end role

    role a1 {
        token escape:sym<@> {
            :my $QSIGIL is context<rw> = '@';
            <?before '@'>
            [ :lang(%*LANG<MAIN>) <EXPR(item %methodcall)> | <!> ] # trap ABORTBRANCH from variable's ::
        }
    } # end role

    role a0 {
        token escape:sym<@> { <!> }
    } # end role

    role h1 {
        token escape:sym<%> {
            :my $QSIGIL is context<rw> = '%';
            <?before '%'>
            [ :lang(%*LANG<MAIN>) <EXPR(item %methodcall)> | <!> ]
        }
    } # end role

    role h0 {
        token escape:sym<%> { <!> }
    } # end role

    role f1 {
        token escape:sym<&> {
            :my $QSIGIL is context<rw> = '&';
            <?before '&'>
            [ :lang(%*LANG<MAIN>) <EXPR(item %methodcall)> | <!> ]
        }
    } # end role

    role f0 {
        token escape:sym<&> { <!> }
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

        token escape:sym<\\> { <sym> <item=backslash> }

        token backslash:qq { <?before 'q'> { $<quote> = $¢.cursor_fresh(%*LANG<MAIN>).quote(); } }
        token backslash:sym<\\> { <text=sym> }
        token backslash:stopper { <text=stopper> }

        # in single quotes, keep backslash on random character by default
        token backslash:misc { {} (.) { $<text> = "\\" ~ $0.Str; } }

        # begin tweaks (DO NOT ERASE)
        multi method tweak (:single(:$q)!) { self.panic("Too late for :q") }
        multi method tweak (:double(:$qq)!) { self.panic("Too late for :qq") }
        # end tweaks (DO NOT ERASE)

    } # end role

    role qq does b1 does c1 does s1 does a1 does h1 does f1 {
        token stopper { \" }
        # in double quotes, omit backslash on random \W backslash by default
        token backslash:misc { {} [ (\W) { $<text> = $0.Str; } | $<x>=(\w) <.panic("Unrecognized backslash sequence: '\\" ~ $<x>.Str ~ "'")> ] }

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

grammar Quasi is STD {
    token term:unquote {
        :my $QUASIMODO is context = 0;
        <starter><starter><starter> <EXPR> <stopper><stopper><stopper>
    }

    # begin tweaks (DO NOT ERASE)
    multi method tweak (:$ast!) { self; } # XXX some transformer operating on the normal AST?
    multi method tweak (:$lang!) { self.cursor_fresh( $lang ); }
    multi method tweak (:$unquote!) { self; } # XXX needs to override unquote
    multi method tweak (:$COMPILING!) { $*QUASIMODO = 1; self; } # XXX needs to lazify the lexical lookups somehow

    multi method tweak (*%x) {
        my @k = keys(%x);
        self.panic("Unrecognized quasiquote modifier: " ~ join('',@k));
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
    :my $INVOCANT_OK is context<rw> = 1;
    <EXPR>
}

token sigterm {
    ':(' ~ ')' <fakesignature>
}

rule param_sep { [','|':'|';'|';;'] }

token fakesignature($CURPAD is context<rw> = $*CURPAD) {
    <.newpad>
    <signature>
}

token signature {
    # XXX incorrectly scopes &infix:<x> parameters to outside following block
    :my $IN_DECL is context<rw> = 1;
    :my $zone is context<rw> = 'posreq';
    :my $startpos = self.pos;
    <.ws>
    [
    | <?before '-->' | ')' | ']' | '{' | ':'\s >
    | <parameter>
    ] ** <param_sep>
    <.ws>
    { $*IN_DECL = 0; }
    [ '-->' <.ws> <typename> ]?
    {{ $*LEFTSIGIL = '@'; $*CURPAD.{'$?GOTSIG'} ~= '(' ~ substr($*ORIG, $startpos, $¢.pos - $startpos) ~ ')'; }}
}

token type_declarator:subset {
    <sym> :s
    [
        <longname> { $¢.add_name($<longname>.Str); }
        [ of <.ws> <typename> ]?
        <trait>*
        [where <EXPR(item %chaining)> ]?    # (EXPR can parse multiple where clauses)
    ] || <.panic: "Malformed subset">
}

token type_declarator:enum {
    <sym> <.ws>
    <longname> <.ws> <trait>* <?before <[ < ( « ]> > <noun> <.ws>
        { $¢.add_name($<longname>.Str); $¢.add_enum($<longname>.Str, $<noun>.Str); }
}

token type_constraint {
    [
    | <value>
    | <typename>
    | where <.ws> <EXPR(item %chaining)>
    ]
    <.ws>
}

rule post_constraint {
    [
    | '[' ~ ']' <signature>
    | '(' ~ ')' <signature>
    | where <EXPR(item %chaining)>
    ]
}

token named_param {
    :my $GOAL is context = ')';
    ':'
    [
    | <name=identifier> '(' <.ws>
        [ <named_param> | <param_var> <.ws> ]
        [ ')' || <.panic: "Unable to parse named parameter; couldn't find right parenthesis"> ]
    | <param_var>
    ]
}

token param_var {
    [
    | '[' ~ ']' <signature>
    | '(' ~ ')' <signature>
    | <sigil> [<?before <.twigil>\w> <twigil>]?
        [
            # Is it a longname declaration?
        || <?{ $<sigil>.Str eq '&' }> <?ident> {}
            <name=sublongname>

        ||  # Is it a shaped array or hash declaration?
            <?{ $<sigil>.Str eq '@' || $<sigil>.Str eq '%' }>
            <name=identifier>?
            <?before <[ \< \( \[ \{ ]> >
            <postcircumfix>

            # ordinary parameter name
        || <name=identifier>
        || $<name> = [<[/!]>]

            # bare sigil?
        ]?
        {{
            my $vname = $<sigil>.Str;
            my $t = $<twigil>;
            my $twigil = '';
            $twigil = $t.[0].Str if @$t;
            $vname ~= $twigil;
            my $n = try { $<name>[0].Str } // '';
            $vname ~= $n;
            given $twigil {
                when '' {
                    self.add_my_variable($vname) if $n ne '';
                }
                when '.' {
                }
                when '!' {
                }
                default {
                    self.worry("Illegal to use $twigil twigil in signature");
                }
            }
        }}
    ]
}

token parameter {
    :my $kind;
    :my $quant = '';
    :my $q;


    [
    | <type_constraint>+
        {
            my $t = $<type_constraint>;
            @$t > 1 and $¢.panic("Multiple prefix constraints not yet supported")
        }
        [
        | '*' <param_var>   { $quant = '*'; $kind = '*'; }
        | '|' <param_var>   { $quant = '|'; $kind = '*'; }
        | '\\' <param_var>  { $quant = '\\'; $kind = '!'; }
        |   [
            | <param_var>   { $quant = ''; $kind = '!'; }
            | <named_param> { $quant = ''; $kind = '*'; }
            ]
            [
            | '?'           { $quant = '?'; $kind = '?' }
            | '!'           { $quant = '!'; $kind //= '!' }
            | <?>
            ]
        | <?> { $quant = ''; $kind = '!' }
        ]

    | '*' <param_var>   { $quant = '*'; $kind = '*'; }
    | '|' <param_var>   { $quant = '|'; $kind = '*'; }
    | '\\' <param_var>  { $quant = '\\'; $kind = '!'; }
    |   [
        | <param_var>   { $quant = ''; $kind = '!'; }
        | <named_param> { $quant = ''; $kind = '*'; }
        ]
        [
        | '?'           { $quant = '?'; $kind = '?' }
        | '!'           { $quant = '!'; $kind //= '!' }
        | <?>
        ]
    ]

    <trait>*

    <post_constraint>*

    [
        <default_value> {{
            given $quant {
              when '!' { $¢.panic("Can't put a default on a required parameter") }
              when '*' { $¢.panic("Can't put a default on a slurpy parameter") }
              when '|' { $¢.panic("Can't put a default on an slurpy capture parameter") }
              when '\\' { $¢.panic("Can't put a default on a capture parameter") }
            }
            $kind = '?' if $kind eq '!';
        }}
    ]?

    {
        $<quant> = $quant;
        $<kind> = $kind;
    }

    # enforce zone constraints
    {{
        given $kind {
            when '!' {
                given $*zone {
                    when 'posopt' {
$¢.panic("Can't put required parameter after optional parameters");
                    }
                    when 'var' {
$¢.panic("Can't put required parameter after variadic parameters");
                    }
                }
            }
            when '?' {
                given $*zone {
                    when 'posreq' { $*zone = 'posopt' }
                    when 'var' {
$¢.panic("Can't put optional positional parameter after variadic parameters");
                    }
                }
            }
            when '*' {
                $*zone = 'var';
            }
        }
    }}
}

rule default_value {
    '=' <EXPR(item %item_assignment)>
}

token statement_prefix:try     { <sym> <blorst> }
token statement_prefix:gather  { <sym> <blorst> }
token statement_prefix:contend { <sym> <blorst> }
token statement_prefix:async   { <sym> <blorst> }
token statement_prefix:maybe   { <sym> <blorst> }
token statement_prefix:lazy    { <sym> <blorst> }
token statement_prefix:do      { <sym> <blorst> }

token statement_prefix:lift    {
    :my $QUASIMODO is context = 1;
    <sym> <blorst>
}

token blorst {
    <?before \s> <.ws>
    [
    | <block>
    | <statement>
    ]
}

#########
# Terms #
#########

# start playing with the setting stubber
token term:YOU_ARE_HERE ( --> Term) {
    <sym> <.you_are_here>
}

token term:new ( --> Term) {
    'new' \h+ <longname> \h* <!before ':'> <.obs("C++ constructor syntax", "method call syntax")>
}

token term:sym<::?IDENT> ( --> Term) {
    $<sym> = [ '::?' <identifier> ] »
}

token term:sym<undef> ( --> Term) {
    <sym> »
    [ <?before \h*'$/' >
        <.obs('$/ variable as input record separator',
             "the filehandle's .slurp method")>
    ]?
    [ <?before \h*<sigil><twigil>?\w >
        <.obs('undef as a verb', 'undefine function')>
    ]?
}

token term:sym<continue> ( --> Term)
    { <sym> » }

token term:sym<self> ( --> Term)
    { <sym> » }

token term:sym<defer> ( --> Term)
    { <sym> » }

token term:rand ( --> Term) {
    <sym> »
    [ <?before '('? \h* [\d|'$']> <.obs('rand(N)', 'N.rand or (1..N).pick')> ]?
    [ <?before '()'> <.obs('rand()', 'rand')> ]?
}

token term:e ( --> Term)
    { <sym> » }

token term:i ( --> Term)
    { <sym> » }

token term:pi ( --> Term)
    { <sym> » }

token term:Inf ( --> Term)
    { <sym> » }

token term:NaN ( --> Term)
    { <sym> » }

token term:sym<*> ( --> Term)
    { <sym> }

token term:sym<**> ( --> Term)
    { <sym> }

token infix:lambda ( --> Term) {
    <?before '{' | '->' > {{
        my $line = $¢.lineof($¢.pos);
        for 'if', 'unless', 'while', 'until', 'for', 'loop', 'given', 'when' {
            my $m = %*MYSTERY{$_};
            next unless $m;
            if $line - ($m.<line>//-123) < 5 {
                $¢.panic("$_() interpreted as function call at line " ~ $m.<line> ~
                "; please use whitespace instead of parens\nUnexpected block in infix position (two terms in a row)");
            }
        }
        return () if $*IN_REDUCE;
        $¢.panic("Unexpected block in infix position (two terms in a row, or previous statement missing semicolon?)");
    }}
}

token circumfix:sigil ( --> Term)
    { :dba('contextualizer') <sigil> '(' ~ ')' <semilist> { $*LEFTSIGIL ||= $<sigil>.Str } }

token circumfix:sym<( )> ( --> Term)
    { :dba('parenthesized expression') '(' ~ ')' <semilist> }

token circumfix:sym<[ ]> ( --> Term)
    { :dba('array composer') '[' ~ ']' <semilist> }

#############
# Operators #
#############

token PRE {
    :dba('prefix or meta-prefix')
    [
    | <prefix>
        { $<O> = $<prefix><O>; $<sym> = $<prefix><sym> }
                                                    {*}         #= prefix
    | <prefix_circumfix_meta_operator>
        { $<O> = $<prefix_circumfix_meta_operator><O>; $<sym> = $<prefix_circumfix_meta_operator>.Str }
                                                    {*}         #= precircum
    ]
    # XXX assuming no precedence change
    
    <prefix_postfix_meta_operator>*                 {*}         #= prepost
    <.ws>
}

token infixish ($in_meta = $*IN_META) {
    :my $infix;
    :my $IN_META is context<rw> = $in_meta;
    <!stdstopper>
    <!infixstopper>
    :dba('infix or meta-infix')
    [
    | <colonpair> {
            $<fake> = 1;
            $<sym> = ':';
            %<O><prec> = %item_assignment<prec>;  # actual test is non-inclusive!
            %<O><assoc> = 'unary';
            %<O><kind> = 'ADVERB';
        }
    | '[' ~ ']' <infixish(1)> { $<O> = $<infixish><O>; $<sym> = $<infixish><sym>; }
    | <infix_circumfix_meta_operator>
        { $<O> = $<infix_circumfix_meta_operator><O>;
          $<sym> = $<infix_circumfix_meta_operator><sym>; }
    | <infix_prefix_meta_operator>
        { $<O> = $<infix_prefix_meta_operator><O>;
          $<sym> = $<infix_prefix_meta_operator><sym>; }
    | <infix> <!before '='>
           { $<O> = $<infix>.<O>; $<sym> = $<infix>.<sym>; }
    | <infix> <?before '='> <?{ $infix = $<infix>; }> <infix_postfix_meta_operator($infix)>
           { $<O> = $<infix_postfix_meta_operator>.<O>; $<sym> = $<infix_postfix_meta_operator>.<sym>; }
    ]
}

# doing fancy as one rule simplifies LTM
token dotty:sym<.*> ( --> Methodcall) {
    ('.' [ <[+*?=]> | '^' '!'? ]) :: <.unspacey> <dottyop>
    { $<sym> = $0.Str; }
}

token dotty:sym<.> ( --> Methodcall) {
    <sym> <dottyop>
}

token privop ( --> Methodcall) {
    '!' <methodop>
}

token dottyopish {
    <noun=dottyop>
}

token dottyop {
    :dba('dotty method or postfix')
    [
    | <methodop>
    | <colonpair>
    | <!alpha> <postop> { $<O> = $<postop><O>; $<sym> = $<postop><sym>; }  # only non-alpha postfixes have dotty form
    ]
}

# Note, this rule mustn't do anything irreversible because it's used
# as a lookahead by the quote interpolator.

token POST {
    <!stdstopper>

    # last whitespace didn't end here
    <!{ @*MEMOS[$¢.pos]<ws> }>

    [ <.unsp> | '\\' ]?

    [ ['.' <.unsp>?]? <postfix_prefix_meta_operator> <.unsp>? ]*

    :dba('postfix')
    [
    | <dotty>  { $<O> = $<dotty><O>;  $<sym> = $<dotty><sym>;  $<~CAPS> = $<dotty><~CAPS>; }
    | <privop> { $<O> = $<privop><O>; $<sym> = $<privop><sym>; $<~CAPS> = $<privop><~CAPS>; }
    | <postop> { $<O> = $<postop><O>; $<sym> = $<postop><sym>; $<~CAPS> = $<postop><~CAPS>; }
    ]
    { $*LEFTSIGIL = '@'; }
}

method can_meta ($op, $meta) {
    !$op<O><fiddly> ||
        self.panic("Can't " ~ $meta ~ " " ~ $op<sym> ~ " because " ~ $op<O><dba> ~ " operators are too fiddly");
    self;
}

regex prefix_circumfix_meta_operator:reduce (--> List_prefix) {
    :my $IN_REDUCE is context<rw> = 1;
    <?before '['\S+']'>
    $<s> = (
        '['
        [
        || <op=infixish(1)> <?before ']'>
        || \\<op=infixish(1)> <?before ']'>
        || <!>
        ]
    )
    [ <?> || <.worry: "Malformed reduce"> <!> ] ']' ['«'|<?>]

    <.can_meta($<s><op>, "reduce")>

    [
    || <!{ $<s><op><O><diffy> }>
    || <?{ $<s><op><O><assoc> eq 'chain' }>
    || <.panic("Can't reduce " ~ $<s><op><sym> ~ " because " ~ $<s><op><O><dba> ~ " operators are diffy and not chaining")>
    ]

    { $<O> = $<s><op><O>; $<O><prec>:delete; $<O><assoc> = 'unary'; $<O><uassoc> = 'left'; }
    { $<sym> = $<s>.Str; }

    [ <?before '('> || <?before \s+ [ <?stdstopper> { $<O><noun> = 1 } ]? > || { $<O><noun> = 1 } ]
}

token prefix_postfix_meta_operator:sym< « >    { <sym> | '<<' }

token postfix_prefix_meta_operator:sym< » >    {
    [ <sym> | '>>' ]
    # require >>.( on interpolated hypercall so infix:«$s»($a,$b) {...} dwims
    [<!{ $*QSIGIL }> || <!before '('> ]
}

token infix_prefix_meta_operator:sym<!> ( --> Transparent) {
    <sym> <!before '!'> {} <infixish(1)>

    [
    || <?{ $<infixish>.Str eq '=' }>
       { $¢ = ::Chaining.coerce($¢); }
       
    || <.can_meta($<infixish>, "negate")>    
       <?{ $<infixish><O><iffy> }>
       <?{ $<O> = $<infixish><O>; }>
        
    || <.panic("Can't negate " ~ $<infixish>.Str ~ " because " ~ $<infixish><O><dba> ~ " operators are not iffy enough")>
    ]
}

token infix_prefix_meta_operator:sym<R> ( --> Transparent) {
    <sym> {} <infixish(1)>
    <.can_meta($<infixish>, "reverse")>
    <?{ $<O> = $<infixish><O>; }>
}

#method lex1 (Str $s) {
#    self.<O>{$s}++ and self.panic("Nested $s metaoperators not allowed");
#    self;
#}

token infix_prefix_meta_operator:sym<X> ( --> List_infix) {
    <sym> {}
    [ <infixish(1)>
        [X <.panic: "Old form of XopX found">]?
        <.can_meta($<infixish>[0], "cross")>
        <?{ $<O> = $<infixish>[0]<O>; $<O><prec>:delete; $<sym> ~= $<infixish>[0].Str }>
    ]?
}

token infix_circumfix_meta_operator:sym<« »> ( --> Transparent) {
    [
    | '«'
    | '»'
    ]
    {} <infixish(1)> [ '«' | '»' || <.panic: "Missing « or »"> ]
    <.can_meta($<infixish>, "hyper")>
    <?{ $<O> := $<infixish><O>; }>
}

token infix_circumfix_meta_operator:sym«<< >>» ( --> Transparent) {
    [
    | '<<'
    | '>>'
    ]
    {} <infixish(1)> [ '<<' | '>>' || <.panic("Missing << or >>")> ]
    <.can_meta($<infixish>, "hyper")>
    <?{ $<O> := $<infixish><O>; }>
}

token infix_postfix_meta_operator:sym<=> ($op --> Item_assignment) {
    '='
    <.can_meta($op, "make assignment out of")>
    [ <!{ $op<O><diffy> }> || <.panic("Can't make assignment out of " ~ $op<sym> ~ " because " ~ $op<O><dba> ~ " operators are diffy")> ]
    { $<O> = $op<O>; $<sym> = $op<sym> ~ '='; $<O><dba> = "assignment"; $<O><prec>:delete; $<O><iffy> = 0; }
}

token postcircumfix:sym<( )> ( --> Methodcall)
    { :dba('argument list') '(' ~ ')' <semiarglist> }

token postcircumfix:sym<[ ]> ( --> Methodcall)
    { :dba('subscript') '[' ~ ']' <semilist> { $<semilist>.Str ~~ /^\s*\-[1]\s*$/ and $¢.obs("[-1] subscript to access final element","[*-1]") } }

token postcircumfix:sym<{ }> ( --> Methodcall)
    { :dba('subscript') '{' ~ '}' <semilist> }

token postcircumfix:sym«< >» ( --> Methodcall)
    { '<' <nibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:q).tweak(:w).balanced('<','>'))> [ '>' || <.panic: "Unable to parse quote-words subscript; couldn't find right angle quote"> ] }

token postcircumfix:sym«<< >>» ( --> Methodcall)
    { '<<' <nibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:qq).tweak(:ww).balanced('<<','>>'))> [ '>>' || <.panic: "Unable to parse quote-words subscript; couldn't find right double-angle quote"> ] }

token postcircumfix:sym<« »> ( --> Methodcall)
    { '«' <nibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:qq).tweak(:ww).balanced('«','»'))> [ '»' || <.panic: "Unable to parse quote-words subscript; couldn't find right double-angle quote"> ] }

token postop {
    | <postfix>         { $<O> := $<postfix><O>; $<sym> := $<postfix><sym>; }
    | <postcircumfix>   { $<O> := $<postcircumfix><O>; $<sym> := $<postcircumfix><sym>; }
}

token methodop {
    [
    | <longname>
    | <?before '$' | '@' | '&' > <variable> { $*VAR = $<variable> }
    | <?before <[ ' " ]> >
        [ <!{$*QSIGIL}> || <!before '"' <-["]>*? \s > ] # dwim on "$foo."
        <quote>
        { my $t = $<quote><nibble>.Str; $t ~~ /\W/ or $t ~~ /^(WHO|WHAT|WHERE|WHEN|WHY|HOW)$/ or $¢.worry("Useless use of quotes") }
    ] <.unsp>? 

    :dba('method arguments')
    [
    | ':' <?before \s> <!{ $*QSIGIL }> <arglist>
    | <?[\\(]> <args>
    ]?
}

token semiarglist {
    <arglist> ** ';'
    <.ws>
}

token arglist {
    :my $inv_ok = $*INVOCANT_OK;
    :my StrPos $endargs is context<rw> = 0;
    :my $GOAL is context = 'endargs';
    :my $QSIGIL is context<rw> = '';
    <.ws>
    :dba('argument list')
    [
    | <?stdstopper>
    | <EXPR(item %list_infix)> {{
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

token term:lambda ( --> Term) {
    <?before <.lambda> >
    <pblock>
    {{
        if $*BORG {
            $*BORG.<block> = $<pblock>;
        }
    }}
}

token circumfix:sym<{ }> ( --> Term) {
    <?before '{' >
    <pblock>
    {{
        if $*BORG {
            $*BORG.<block> = $<pblock>;
        }
    }}
}

## methodcall

token postfix:sym<i> ( --> Methodcall)
    { <sym> » }

token infix:sym<.> ()
    { '.' <[\]\)\},:\s\$"']> <.obs('. to concatenate strings', '~')> }

token postfix:sym['->'] ()
    { '->' <.obs('-> to call a method', '.')> }

## autoincrement
token postfix:sym<++> ( --> Autoincrement)
    { <sym> }

token postfix:sym<--> ( --> Autoincrement)
    { <sym> }

token prefix:sym<++> ( --> Autoincrement)
    { <sym> }

token prefix:sym<--> ( --> Autoincrement)
    { <sym> }

## exponentiation
token infix:sym<**> ( --> Exponentiation)
    { <sym> }

## symbolic unary
token prefix:sym<!> ( --> Symbolic_unary)
    { <sym> }

token prefix:sym<+> ( --> Symbolic_unary)
    { <sym> }

token prefix:sym<-> ( --> Symbolic_unary)
    { <sym> }

token prefix:sym<~~> ( --> Symbolic_unary)
    { <sym> <.badinfix> }

token prefix:sym<~> ( --> Symbolic_unary)
    { <sym> }

token prefix:sym<??> ( --> Symbolic_unary)
    { <sym> <.badinfix> }

token prefix:sym<?> ( --> Symbolic_unary)
    { <sym> }

token prefix:sym<~^> ( --> Symbolic_unary)
    { <sym> }

token prefix:sym<+^> ( --> Symbolic_unary)
    { <sym> }

token prefix:sym<?^> ( --> Symbolic_unary)
    { <sym> }

token prefix:sym<^^> ( --> Symbolic_unary)
    { <sym> <.badinfix> }

token prefix:sym<^> ( --> Symbolic_unary)
    { <sym> }

token prefix:sym<||> ( --> Symbolic_unary)
    { <sym> <.badinfix> }

token prefix:sym<|> ( --> Symbolic_unary)
    { <sym> }


## multiplicative
token infix:sym<*> ( --> Multiplicative)
    { <sym> }

token infix:sym</> ( --> Multiplicative)
    { <sym> }

token infix:sym<div> ( --> Multiplicative)
    { <sym> }

token infix:sym<%> ( --> Multiplicative)
    { <sym> <?{ $<O><iffy> = 1; }> }   # Allow !% operator

token infix:sym<mod> ( --> Multiplicative)
    { <sym> }

token infix:sym<+&> ( --> Multiplicative)
    { <sym> }

token infix:sym« +< » ( --> Multiplicative)
    { <sym> <!before '<'> }

token infix:sym« << » ( --> Multiplicative)
    { <sym> \s <.obs('<< to do left shift', '+< or ~<')> }

token infix:sym« >> » ( --> Multiplicative)
    { <sym> \s <.obs('>> to do right shift', '+> or ~>')> }

token infix:sym« +> » ( --> Multiplicative)
    { <sym> <!before '>'> }

token infix:sym<~&> ( --> Multiplicative)
    { <sym> }

token infix:sym<?&> ( --> Multiplicative)
    { <sym> }

token infix:sym« ~< » ( --> Multiplicative)
    { <sym> <!before '<'> }

token infix:sym« ~> » ( --> Multiplicative)
    { <sym> <!before '>'> }


## additive
token infix:sym<+> ( --> Additive)
    { <sym> }

token infix:sym<-> ( --> Additive)
    { <sym> }

token infix:sym<+|> ( --> Additive)
    { <sym> }

token infix:sym<+^> ( --> Additive)
    { <sym> }

token infix:sym<~|> ( --> Additive)
    { <sym> }

token infix:sym<~^> ( --> Additive)
    { <sym> }

token infix:sym<?|> ( --> Additive)
    { <sym> }

token infix:sym<?^> ( --> Additive)
    { <sym> }

## replication
# Note: no word boundary check after x, relies on longest token for x2 xx2 etc
token infix:sym<x> ( --> Replication)
    { <sym> }

token infix:sym<xx> ( --> Replication)
    { <sym> }

## concatenation
token infix:sym<~> ( --> Concatenation)
    { <sym> }


## junctive and (all)
token infix:sym<&> ( --> Junctive_and)
    { <sym> }

token infix:sym<also> ( --> Junctive_and)
    { <sym> }


## junctive or (any)
token infix:sym<|> ( --> Junctive_or)
    { <sym> }

token infix:sym<^> ( --> Junctive_or)
    { <sym> }


## named unary examples
# (need \s* to win LTM battle with listops)
token prefix:sleep ( --> Named_unary)
    { <sym> » <?before \s*> }

token prefix:abs ( --> Named_unary)
    { <sym> » <?before \s*> }

token prefix:let ( --> Named_unary)
    { <sym> » <?before \s*> }

token prefix:temp ( --> Named_unary)
    { <sym> » <?before \s*> }


## structural infix
token infix:sym« <=> » ( --> Structural)
    { <sym> <?{ $<O><returns> = "Order"; }> }

token infix:cmp ( --> Structural)
    { <sym> <?{ $<O><returns> = "Order"; }> }

token infix:leg ( --> Structural)
    { <sym> <?{ $<O><returns> = "Order"; }> }

token infix:but ( --> Structural)
    { <sym> }

token infix:does ( --> Structural)
    { <sym> }

token infix:sym<..> ( --> Structural)
    { <sym> [<!{ $*IN_META }> <?before ')' | ']'> <.panic: "Please use ..* for indefinite range">]? }

token infix:sym<^..> ( --> Structural)
    { <sym> }

token infix:sym<..^> ( --> Structural)
    { <sym> }

token infix:sym<^..^> ( --> Structural)
    { <sym> }


## chaining binary
token infix:sym<==> ( --> Chaining)
    { <sym> <!before '=' > }

token infix:sym<!=> ( --> Chaining)
    { <sym> <?before \s> }

token infix:sym« < » ( --> Chaining)
    { <sym> }

token infix:sym« <= » ( --> Chaining)
    { <sym> }

token infix:sym« > » ( --> Chaining)
    { <sym> }

token infix:sym« >= » ( --> Chaining)
    { <sym> }

token infix:sym<~~> ( --> Chaining)
    { <sym> }

# XXX should move to inside meta !
token infix:sym<!~> ( --> Chaining)
    { <sym> \s <.obs('!~ to do negated pattern matching', '!~~')> }

token infix:sym<=~> ( --> Chaining)
    { <sym> <.obs('=~ to do pattern matching', '~~')> }

token infix:sym<eq> ( --> Chaining)
    { <sym> }

token infix:sym<ne> ( --> Chaining)
    { <sym> }

token infix:sym<lt> ( --> Chaining)
    { <sym> }

token infix:sym<le> ( --> Chaining)
    { <sym> }

token infix:sym<gt> ( --> Chaining)
    { <sym> }

token infix:sym<ge> ( --> Chaining)
    { <sym> }

token infix:sym<=:=> ( --> Chaining)
    { <sym> }

token infix:sym<===> ( --> Chaining)
    { <sym> }

token infix:sym<eqv> ( --> Chaining)
    { <sym> }

token infix:sym<before> ( --> Chaining)
    { <sym> }

token infix:sym<after> ( --> Chaining)
    { <sym> }


## tight and
token infix:sym<&&> ( --> Tight_and)
    { <sym> }


## tight or
token infix:sym<||> ( --> Tight_or)
    { <sym> }

token infix:sym<^^> ( --> Tight_or)
    { <sym> }

token infix:sym<//> ( --> Tight_or)
    { <sym> }

token infix:sym<min> ( --> Tight_or)
    { <sym> }

token infix:sym<max> ( --> Tight_or)
    { <sym> }


## conditional
token infix:sym<?? !!> ( --> Conditional) {
    :my $GOAL is context = '!!';
    '??'
    <.ws>
    <EXPR(item %item_assignment)>
    [ '!!' ||
        [
        || <?before '='> <.panic: "Assignment not allowed within ??!!">
        || <?before '::'> <.panic: "Please use !! rather than ::">
        || <?before <infixish>>    # Note: a tight infix would have parsed right
            <.panic: "Precedence too loose within ??!!; use ??()!! instead ">
        || <.panic: "Found ?? but no !!; possible precedence problem">
        ]
    ]
}

token infix:sym<?> ( --> Conditional)
    { <sym> <.obs('?: for the conditional operator', '??!!')> }

token infix:sym<ff> ( --> Conditional)
    { <sym> }

token infix:sym<^ff> ( --> Conditional)
    { <sym> }

token infix:sym<ff^> ( --> Conditional)
    { <sym> }

token infix:sym<^ff^> ( --> Conditional)
    { <sym> }

token infix:sym<fff> ( --> Conditional)
    { <sym> }

token infix:sym<^fff> ( --> Conditional)
    { <sym> }

token infix:sym<fff^> ( --> Conditional)
    { <sym> }

token infix:sym<^fff^> ( --> Conditional)
    { <sym> }

## assignment
# There is no "--> type" because assignment may be coerced to either
# item assignment or list assignment at "make" time.

token infix:sym<=> ()
{
    <sym>
    { $¢ = $*LEFTSIGIL eq '$' 
        ?? ::Item_assignment.coerce($¢)
        !! ::List_assignment.coerce($¢);
    }
}

token infix:sym<:=> ( --> Item_assignment)
    { <sym> }

token infix:sym<::=> ( --> Item_assignment)
    { <sym> }

token infix:sym<.=> ( --> Item_assignment) {
    <sym> <.ws>
    [ <?before \w+';' | 'new'|'sort'|'subst'|'trans'|'reverse'|'uniq'|'map'|'samecase'|'substr'|'flip'|'fmt' > || <worryobs('.= as append operator', '~=')> ]
    { $<O><nextterm> = 'dottyopish' }
}

token infix:sym« => » ( --> Item_assignment)
    { <sym> { $<O><fiddly> = 0; } }

# Note, other assignment ops generated by infix_postfix_meta_operator rule

## loose unary
token prefix:sym<true> ( --> Loose_unary)
    { <sym> » }

token prefix:sym<not> ( --> Loose_unary)
    { <sym> » }

## list item separator
token infix:sym<,> ( --> Comma)
    { <sym> { $<O><fiddly> = 0; } }

token infix:sym<:> ( --> Comma)
    { <sym> <?before \s | <terminator> >
        { $¢.panic("Illegal use of colon as invocant marker") unless $*INVOCANT_OK--; }
    }

token infix:sym« p5=> » ( --> Comma)
    { <sym> }

token infix:sym<Z> ( --> List_infix)
    { <sym> }

token infix:sym<minmax> ( --> List_infix)
    { <sym> }

token infix:sym<...> ( --> List_infix)
    { <sym> }

token term:sym<...> ( --> List_prefix)
    { <sym> <args>? }

token term:sym<???> ( --> List_prefix)
    { <sym> <args>? }

token term:sym<!!!> ( --> List_prefix)
    { <sym> <args>? }

# force identifier(), identifier.(), etc. to be a function call always
token term:identifier ( --> Term )
{
    :my $name;
    :my $pos;
    <identifier> <?before [<unsp>|'(']? >
    { $name = $<identifier>.Str; $pos = $¢.pos; }
    <args( $¢.is_name($name) )>
    { self.add_mystery($name,$pos) unless $<args><invocant>; }
    {{
        if $*BORG and $*BORG.<block> {
            if not $*BORG.<name> {
                $*BORG.<culprit> = $<identifier>.cursor($pos);
                $*BORG.<name> = $name;
            }
        }
    }}
}

token term:opfunc ( --> Term )
{
    <category> <colonpair>+ <args>
}

token args ($istype = 0) {
    :my $listopish = 0;
    :my $GOAL is context = '';
    :my $INVOCANT_OK is context<rw> = 1;
    :my $INVOCANT_IS is context<rw>;
    [
#    | :dba('argument list') '.(' ~ ')' <semiarglist> {*}             #= func args
    | :dba('argument list') '(' ~ ')' <semiarglist> {*}              #= func args
    | :dba('argument list') <.unsp> '(' ~ ')' <semiarglist> {*} #= func args
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
token term:name ( --> Term)
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
        <.unsp>? [ <?before '['> <postcircumfix> ]?
        :dba('type parameter')
        [
            '::'
            <?before [ '«' | '<' | '{' | '<<' ] > <postcircumfix>
            {*}                                                 #= packagevar 
        ]?
        {*}                                                     #= typename

    # unrecognized names are assumed to be post-declared listops.
    || <args> { self.add_mystery($name,$pos) unless $<args><invocant>; }
        {{
            if $*BORG and $*BORG.<block> {
                if not $*BORG.<name> {
                    $*BORG.<culprit> = $<longname>.cursor($pos);
                    $*BORG.<name> //= $name;
                }
            }
        }}
    ]
}

## loose and
token infix:sym<and> ( --> Loose_and)
    { <sym> }

token infix:sym<andthen> ( --> Loose_and)
    { <sym> }

## loose or
token infix:sym<or> ( --> Loose_or)
    { <sym> }

token infix:sym<orelse> ( --> Loose_or)
    { <sym> }

token infix:sym<xor> ( --> Loose_or)
    { <sym> }

## sequencer
token infix:sym« <== » ( --> Sequencer)
    { <sym> }

token infix:sym« ==> » ( --> Sequencer)
    { <sym> {*} }              #'

token infix:sym« <<== » ( --> Sequencer)
    { <sym> }

token infix:sym« ==>> » ( --> Sequencer)
    { <sym> {*} }              #'

## expression terminator
# Note: must always be called as <?terminator> or <?before ...<terminator>...>

token terminator:sym<;> ( --> Terminator)
    { ';' }

token terminator:sym<if> ( --> Terminator)
    { 'if' » <.nofun> }

token terminator:sym<unless> ( --> Terminator)
    { 'unless' » <.nofun> }

token terminator:sym<while> ( --> Terminator)
    { 'while' » <.nofun> }

token terminator:sym<until> ( --> Terminator)
    { 'until' » <.nofun> }

token terminator:sym<for> ( --> Terminator)
    { 'for' » <.nofun> }

token terminator:sym<given> ( --> Terminator)
    { 'given' » <.nofun> }

token terminator:sym<when> ( --> Terminator)
    { 'when' » <.nofun> }

token terminator:sym« --> » ( --> Terminator)
    { '-->' {*} }              #'

token terminator:sym<)> ( --> Terminator)
    { <sym> }

token terminator:sym<]> ( --> Terminator)
    { ']' }

token terminator:sym<}> ( --> Terminator)
    { '}' }

token terminator:sym<!!> ( --> Terminator)
    { '!!' <?{ $*GOAL eq '!!' }> }

# disallow &[] and such as infix
# token infix:sigil ( --> Term )
#     { <sigil><-[&]> <.worry: "Sigiled form not allowed where infix expected"> <!> }

regex infixstopper {
    :dba('infix stopper')
    [
    | <?before <stopper> >
    | <?before '!!' > <?{ $*GOAL eq '!!' }>
    | <?before '{' | <lambda> > <?{ ($*GOAL eq '{' or $*GOAL eq 'endargs') and @*MEMOS[$¢.pos]<ws> }>
    | <?{ $*GOAL eq 'endargs' and @*MEMOS[$¢.pos]<endargs> }>
    ]
}

# overridden in subgrammars
token stopper { <!> }

# hopefully we can include these tokens in any outer LTM matcher
regex stdstopper {
    :my @stub = return self if @*MEMOS[self.pos]<endstmt> :exists;
    :dba('standard stopper')
    [
    | <?terminator>
    | <?unitstopper>
    | $                                 # unlikely, check last (normal LTM behavior)
    ]
    { @*MEMOS[$¢.pos]<endstmt> ||= 1; }
}

##############################
# Operator Precedence Parser #
##############################

method EXPR ($preclvl) {
    temp $*CTX = self.callm if $*DEBUG +& DEBUG::trace_call;
    if self.peek {
        return self._AUTOLEXpeek('EXPR');
    }
    my $preclim = $preclvl ?? $preclvl.<prec> // $LOOSEST !! $LOOSEST;
    my $LEFTSIGIL is context<rw> = '';
    my @termstack;
    my @opstack;
    my $termish = 'termish';

    push @opstack, { 'O' => item %terminator, 'sym' => '' };         # (just a sentinel value)

    my $here = self;
    my $S = $here.pos;
    self.deb("In EXPR, at $S") if $*DEBUG +& DEBUG::EXPR;

    my &reduce := -> {
        self.deb("entering reduce, termstack == ", +@termstack, " opstack == ", +@opstack) if $*DEBUG +& DEBUG::EXPR;
        my $op = pop @opstack;
        my $sym = $op<sym>;
        given $op<O><assoc> // 'unary' {
            when 'chain' {
                self.deb("reducing chain") if $*DEBUG +& DEBUG::EXPR;
                my @chain;
                push @chain, pop(@termstack);
                push @chain, $op;
                while @opstack {
                    last if $op<O><prec> ne @opstack[*-1]<O><prec>;
                    push @chain, pop(@termstack);
                    push @chain, pop(@opstack);
                }
                push @chain, pop(@termstack);
                my $endpos = @chain[0]<_pos>;
                @chain = reverse @chain if @chain > 1;
                my $startpos = @chain[0]<_from>;
                my $nop = $op.cursor_fresh();
                $nop<chain> = [@chain];
                $nop<_arity> = 'CHAIN';
                $nop<_from> = $startpos;
                $nop<_pos> = $endpos;
                $nop<~CAPS> = \@chain;
                push @termstack, $nop._REDUCE($startpos, 'CHAIN');
                @termstack[*-1].<PRE>:delete;
                @termstack[*-1].<POST> :delete;
            }
            when 'list' {
                self.deb("reducing list") if $*DEBUG +& DEBUG::EXPR;
                my @list;
                my @delims = $op;
                push @list, pop(@termstack);
                while @opstack {
                    self.deb($sym ~ " vs " ~ @opstack[*-1]<sym>) if $*DEBUG +& DEBUG::EXPR;
                    last if $sym ne @opstack[*-1]<sym>;
                    if @termstack and defined @termstack[0] {
                        push @list, pop(@termstack);
                    }
                    else {
                        self.worry("Missing term in " ~ $sym ~ " list");
                    }
                    push @delims, pop(@opstack);
                }
                if @termstack and defined @termstack[0] {
                    push @list, pop(@termstack);
                }
                else {
                    self.worry("Missing final term in '" ~ $sym ~ "' list");
                }
                my $endpos = @list[0]<_pos>;
                @list = reverse @list if @list > 1;
                my $startpos = @list[0]<_from>;
                @delims = reverse @delims if @delims > 1;
                my $nop = $op.cursor_fresh();
                $nop<sym> = $sym;
                $nop<O> = $op<O>;
                $nop<list> = [@list];
                $nop<delims> = [@delims];
                $nop<_arity> = 'LIST';
                $nop<_from> = $startpos;
                $nop<_pos> = $endpos;
                if @list {
                    my @caps;
                    push @caps, @list[0].caps if @list[0];
                    for 0..@delims-1 {
                        my $d = @delims[$_];
                        my $l = @list[$_+1];
                        push @caps, $d;
                        push @caps, $l;  # nullterm?
                    }
                    $nop<~CAPS> = \@caps;
                }
                push @termstack, $nop._REDUCE($startpos, 'LIST');
                @termstack[*-1].<PRE>:delete;
                @termstack[*-1].<POST>:delete;
            }
            when 'unary' {
                self.deb("reducing") if $*DEBUG +& DEBUG::EXPR;
                self.deb("Termstack size: ", +@termstack) if $*DEBUG +& DEBUG::EXPR;

                self.deb($op.dump) if $*DEBUG +& DEBUG::EXPR;
                my $nop = $op.cursor_fresh();
                my $arg = pop @termstack;
                $op<arg> = $arg;
                my $a = $op<~CAPS>;
                $op<_arity> = 'UNARY';
                if $arg<_from> < $op<_from> { # postfix
                    $op<_from> = $arg<_from>;   # extend from to include arg
#                    warn "OOPS ", $arg.Str, "\n" if @acaps > 1;
                    unshift @$a, $arg;
                    push @termstack, $op._REDUCE($op<_from>, 'POSTFIX');
                    @termstack[*-1].<PRE>:delete;
                    @termstack[*-1].<POST>:delete;
                }
                elsif $arg<_pos> > $op<_pos> {   # prefix
                    $op<_pos> = $arg<_pos>;     # extend pos to include arg
#                    warn "OOPS ", $arg.Str, "\n" if @acaps > 1;
                    push @$a, $arg;
                    push @termstack, $op._REDUCE($op<_from>, 'PREFIX');
                    @termstack[*-1].<PRE>:delete;
                    @termstack[*-1].<POST>:delete;
                }
            }
            default {
                self.deb("reducing") if $*DEBUG +& DEBUG::EXPR;
                self.deb("Termstack size: ", +@termstack) if $*DEBUG +& DEBUG::EXPR;

                my $right = pop @termstack;
                my $left = pop @termstack;
                $op<right> = $right;
                $op<left> = $left;
                $op<_from> = $left<_from>;
                $op<_pos> = $right<_pos>;
                $op<_arity> = 'BINARY';

                my $a = $op<~CAPS>;
                unshift @$a, $left;
                push @$a, $right;

                self.deb($op.dump) if $*DEBUG +& DEBUG::EXPR;
                push @termstack, $op._REDUCE($op<_from>, 'INFIX');
                @termstack[*-1].<PRE>:delete;
                @termstack[*-1].<POST>:delete;
            }
        }
    };

  TERM:
    loop {
        self.deb("In loop, at ", $here.pos) if $*DEBUG +& DEBUG::EXPR;
        my $oldpos = $here.pos;
        $here = $here.cursor_fresh();
        $*LEFTSIGIL = @opstack[*-1]<O><prec> gt $item_assignment_prec ?? '@' !! '';
        my @t = $here.$termish;

        if not @t or not $here = @t[0] or ($here.pos == $oldpos and $termish eq 'termish') {
            return ();
            # $here.panic("Failed to parse a required term");
        }
        $termish = 'termish';
        my $PRE = $here.<PRE>:delete // [];
        my $POST = $here.<POST>:delete // [];
        my @PRE = @$PRE;
        my @POST = reverse @$POST;

        # interleave prefix and postfix, pretend they're infixish
        my $M = $here;

        # note that we push loose stuff onto opstack before tight stuff
        while @PRE and @POST {
            my $postO = @POST[0]<O>;
            my $preO = @PRE[0]<O>;
            if $postO<prec> lt $preO<prec> {
                push @opstack, shift @POST;
            }
            elsif $postO<prec> gt $preO<prec> {
                push @opstack, shift @PRE;
            }
            elsif $postO<uassoc> eq 'left' {
                push @opstack, shift @POST;
            }
            elsif $postO<uassoc> eq 'right' {
                push @opstack, shift @PRE;
            }
            else {
                $here.panic('"' ~ @PRE[0]<sym> ~ '" and "' ~ @POST[0]<sym> ~ '" are not associative');
            }
        }
        push @opstack, @PRE,@POST;

        push @termstack, $here.<noun>;
        @termstack[*-1].<POST>:delete;
        self.deb("after push: " ~ (0+@termstack)) if $*DEBUG +& DEBUG::EXPR;

        last TERM if $preclim eq $methodcall_prec; # in interpolation, probably

        loop {     # while we see adverbs
            $oldpos = $here.pos;
            last TERM if (@*MEMOS[$oldpos]<endstmt> // 0) == 2;
            $here = $here.cursor_fresh.ws;
            my @infix = $here.cursor_fresh.infixish();
            last TERM unless @infix;
            my $infix = @infix[0];
            last TERM unless $infix.pos > $oldpos;
            
            if not $infix<sym> {
                die $infix.dump if $*DEBUG +& DEBUG::EXPR;
            }

            my $inO = $infix<O>;
            my Str $inprec = $inO<prec>;
            if not defined $inprec {
                self.deb("No prec given in infix!") if $*DEBUG +& DEBUG::EXPR;
                die $infix.dump if $*DEBUG +& DEBUG::EXPR;
                $inprec = %terminator<prec>;
            }

            if $inprec le $preclim {
                if $preclim ne $LOOSEST {
                    my $dba = $preclvl.<dba>;
                    my $h = $*HIGHEXPECT;
                    my $k = 'infix or meta-infix';
                    $h.{$k}:delete;
                    $h.{"infix or meta-infix (with precedence tighter than $dba)"} = 1;
                }
                last TERM;
            }

            $here = $infix.cursor_fresh.ws();

            # substitute precedence for listops
            $inO<prec> = $inO<sub> if $inO<sub>;

            # Does new infix (or terminator) force any reductions?
            while @opstack[*-1]<O><prec> gt $inprec {
                &reduce();
            }

            # Not much point in reducing the sentinels...
            last if $inprec lt $LOOSEST;

        if $infix<fake> {
            push @opstack, $infix;
            &reduce();
            next;  # not really an infix, so keep trying
        }

            # Equal precedence, so use associativity to decide.
            if @opstack[*-1]<O><prec> eq $inprec {
                given $inO<assoc> {
                    when 'non'   { $here.panic('"' ~ $infix.Str ~ '" is not associative') }
                    when 'left'  { &reduce() }   # reduce immediately
                    when 'right' { }            # just shift
                    when 'chain' { }            # just shift
                    when 'unary' { }            # just shift
                    when 'list'  {              # if op differs reduce else shift
                       # &reduce() if $infix<sym> !eqv @opstack[*-1]<sym>;
                    }
                    default { $here.panic('Unknown associativity "' ~ $_ ~ '" for "' ~ $infix<sym> ~ '"') }
                }
            }

            $termish = $inO<nextterm> if $inO<nextterm>;
            push @opstack, $infix;              # The Shift
            last;
        }
    }
    &reduce() while +@opstack > 1;
    if @termstack {
        +@termstack == 1 or $here.panic("Internal operator parser error, termstack == " ~ (+@termstack));
        @termstack[0]<_from> = self.pos;
        @termstack[0]<_pos> = $here.pos;
    }
    self._MATCHIFYr($S, "EXPR", @termstack);
}

##########
## Regex #
##########

grammar Regex is STD {

    # begin tweaks (DO NOT ERASE)
    multi method tweak (:Perl5(:$P5)!) { self.cursor_fresh( %*LANG<Q> ).mixin( ::q ).mixin( ::p5 ) }
    multi method tweak (:overlap(:$ov)!) { self }
    multi method tweak (:exhaustive(:$ex)!) { self }
    multi method tweak (:continue(:$c)!) { self }
    multi method tweak (:pos(:$p)!) { self }
    multi method tweak (:sigspace(:$s)!) { self }
    multi method tweak (:ratchet(:$r)!) { self }
    multi method tweak (:global(:$g)!) { self }
    multi method tweak (:ignorecase(:$i)!) { self }
    multi method tweak (:ignoreaccent(:$a)!) { self }
    multi method tweak (:samecase(:$ii)!) { self }
    multi method tweak (:sameaccent(:$aa)!) { self }
    multi method tweak (:$nth!) { self }
    multi method tweak (:st(:$nd)!) { self }
    multi method tweak (:rd(:$th)!) { self }
    multi method tweak (:$x!) { self }
    multi method tweak (:$bytes!) { self }
    multi method tweak (:$codes!) { self }
    multi method tweak (:$graphs!) { self }
    multi method tweak (:$chars!) { self }
    multi method tweak (:$rw!) { self }
    multi method tweak (:$keepall!) { self }
    multi method tweak (:$panic!) { self }
    # end tweaks (DO NOT ERASE)

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
        <?{ $*sigspace }>
        || [ <?before \s | '#'> <.nextsame> ]?   # still get all the pod goodness, hopefully
    }

    token normspace {
        <?before \s | '#'> [ :lang($¢.cursor_fresh(%*LANG<MAIN>)) <.ws> ]
    }

    # suppress fancy end-of-line checking
    token codeblock {
        :my $GOAL is context = '}';
        '{' :: [ :lang($¢.cursor_fresh(%*LANG<MAIN>)) <statementlist> ]
        [ '}' || <.panic: "Unable to parse statement list; couldn't find right brace"> ]
    }

    rule nibbler {
        :my $sigspace    is context<rw> = $*sigspace    // 0;
        :my $ratchet     is context<rw> = $*ratchet     // 0;
        :my $ignorecase is context<rw> = $*ignorecase // 0;
        :my $ignoreaccent    is context<rw> = $*ignoreaccent    // 0;
        [ \s* < || | && & > ]?
        <EXPR>
    }

    token termish {
        <.ws>
        [
        || <noun=quantified_atom>+
        || <?before <stopper> | <[&|~]>  >  <.panic: "Null pattern not allowed">
        || <?before <[ \] \) \> ]> > <.panic: "Unmatched closing bracket">
        || <.panic: "Unrecognized regex metacharacter">
        ]
    }
    token infixish {
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
    token rxinfix:sym<~> ( --> Additive ) { <sym> }

    token quantified_atom {
        <!stopper>
        <!rxinfix>
        <atom>
        <.ws>
        [ <quantifier> <.ws> ]?
#            <?{ $<atom>.max_width }>
#                || <.panic: "Can't quantify zero-width atom">
    }

    token atom {
        :dba('regex atom')
        [
        | \w
        | <metachar> ::
        ]
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
        <normspace>
    }

    token metachar:sym<{ }> {
        <?before '{'>
        <codeblock>
        {{ $/<sym> := <{ }> }}
    }

    token metachar:mod {
        <mod_internal>
        { $/<sym> := $<mod_internal><sym> }
    }

    token metachar:sym<:> {
        <sym>
    }

    token metachar:sym<::> {
        <sym>
    }

    token metachar:sym<:::> {
        <sym>
    }

    token metachar:sym<[ ]> {
        '[' {} [:lang(self.unbalanced(']')) <nibbler>]
        [ ']' || <.panic: "Unable to parse regex; couldn't find right bracket"> ]
        { $/<sym> := <[ ]> }
    }

    token metachar:sym<( )> {
        '(' {} [:lang(self.unbalanced(')')) <nibbler>]
        [ ')' || <.panic: "Unable to parse regex; couldn't find right parenthesis"> ]
        { $/<sym> := <( )> }
    }

    token metachar:sym« <( » { '<(' }
    token metachar:sym« )> » { ')>' }

    token metachar:sym« << » { '<<' }
    token metachar:sym« >> » { '>>' }
    token metachar:sym< « > { '«' }
    token metachar:sym< » > { '»' }

    token metachar:qw {
        <?before '<' \s >  # (note required whitespace)
        <circumfix>
    }

    token metachar:sym«< >» {
        '<' <unsp>? {} <assertion>
        [ '>' || <.panic: "regex assertion not terminated by angle bracket"> ]
    }

    token metachar:sym<\\> { <sym> <backslash> }
    token metachar:sym<.>  { <sym> }
    token metachar:sym<^^> { <sym> }
    token metachar:sym<^>  { <sym> }
    token metachar:sym<$$> {
        <sym>
        [ (\w+) <.obs("\$\$" ~ $0.Str ~ " to deref var inside a regex", "\$(\$" ~ $0.Str ~ ")")> ]?
    }
    token metachar:sym<$>  {
        '$'
        <?before
        | \s
        | '|'
        | '&'
        | ')'
        | ']'
        | '>'
        | $
        | <.stopper>
        >
    }

    token metachar:sym<' '> { <?before "'"> [:lang($¢.cursor_fresh(%*LANG<MAIN>)) <quote>] }
    token metachar:sym<" "> { <?before '"'> [:lang($¢.cursor_fresh(%*LANG<MAIN>)) <quote>] }

    token metachar:var {
        <!before '$$'>
        <?before <sigil>>
        [:lang($¢.cursor_fresh(%*LANG<MAIN>)) <variable> <.ws> <.check_variable($<variable>)> ]
        $<binding> = ( <.ws> '=' <.ws> <quantified_atom> )?
        { $<sym> = $<variable>.Str; }
    }

    token backslash:unspace { <?before \s> <.SUPER::ws> }

    token backslash:sym<0> { '0' <!before <[0..7]> > }

    token backslash:A { <sym> <.obs('\\A as beginning-of-string matcher', '^')> }
    token backslash:a { <sym> <.panic: "\\a is allowed only in strings, not regexes"> }
    token backslash:b { :i <sym> }
    token backslash:c { :i <sym> <charspec> }
    token backslash:d { :i <sym> }
    token backslash:e { :i <sym> }
    token backslash:f { :i <sym> }
    token backslash:h { :i <sym> }
    token backslash:n { :i <sym> }
    token backslash:o { :i <sym> [ <octint> | '[' ~ ']' <octints> ] }
    token backslash:Q { <sym> <.obs('\\Q as quotemeta', 'quotes or literal variable match')> }
    token backslash:r { :i <sym> }
    token backslash:s { :i <sym> }
    token backslash:t { :i <sym> }
    token backslash:v { :i <sym> }
    token backslash:w { :i <sym> }
    token backslash:x { :i <sym> [ <hexint> | '[' ~ ']' <hexints> ] }
    token backslash:z { <sym> <.obs('\\z as end-of-string matcher', '$')> }
    token backslash:Z { <sym> <.obs('\\Z as end-of-string matcher', '\\n?$')> }
    token backslash:misc { $<litchar>=(\W) }
    token backslash:oops { <.panic: "Unrecognized regex backslash sequence"> }

    token assertion:sym<...> { <sym> }
    token assertion:sym<???> { <sym> }
    token assertion:sym<!!!> { <sym> }

    token assertion:sym<?> { <sym> [ <?before '>'> | <assertion> ] }
    token assertion:sym<!> { <sym> [ <?before '>'> | <assertion> ] }
    token assertion:sym<*> { <sym> [ <?before '>'> | <.ws> <nibbler> ] }

    token assertion:sym<{ }> { <codeblock> }

    token assertion:variable {
        <?before <sigil>>  # note: semantics must be determined per-sigil
        [:lang($¢.cursor_fresh(%*LANG<MAIN>).unbalanced('>')) <variable=EXPR(item %LOOSEST)>]
    }

    token assertion:method {
        '.' [
            | <?before <alpha> > <assertion>
            | [ :lang($¢.cursor_fresh(%*LANG<MAIN>).unbalanced('>')) <dottyop> ]
            ]
    }

    token assertion:name { [ :lang($¢.cursor_fresh(%*LANG<MAIN>).unbalanced('>')) <longname> ]
                                    [
                                    | <?before '>' >
                                    | <.ws> <nibbler>
                                    | '=' <assertion>
                                    | ':' <.ws>
                                        [ :lang($¢.cursor_fresh(%*LANG<MAIN>).unbalanced('>')) <arglist> ]
                                    | '(' {}
                                        [ :lang($¢.cursor_fresh(%*LANG<MAIN>)) <arglist> ]
                                        [ ')' || <.panic: "Assertion call missing right parenthesis"> ]
                                    ]?
    }

    token assertion:sym<[> { <?before '['> <cclass_elem>+ }
    token assertion:sym<+> { <?before '+'> <cclass_elem>+ }
    token assertion:sym<-> { <?before '-'> <cclass_elem>+ }
    token assertion:sym<.> { <sym> }
    token assertion:sym<,> { <sym> }
    token assertion:sym<~~> { <sym> [ <?before '>'> | \d+ | <desigilname> ] }

    token assertion:bogus { <.panic: "Unrecognized regex assertion"> }

    token sign { '+' | '-' | <?> }
    token cclass_elem {
        :dba('character class element')
        <sign>
        <.normspace>?
        [
        | <name>
        | <before '['> <quibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:q))> # XXX parse as q[] for now
        ]
        <.normspace>?
    }

    token mod_arg { :dba('modifier argument') '(' ~ ')' <semilist> }

    token mod_internal:sym<:my>    { ':' <?before ['my'|'state'|'our'|'constant'] \s > [:lang($¢.cursor_fresh(%*LANG<MAIN>)) <statement> <eat_terminator> ] }
    token mod_internal:sym<:temp>    { ':' <?before ['temp'|'let'] \s > [:lang($¢.cursor_fresh(%*LANG<MAIN>)) <statement> <eat_terminator> ] }

    # XXX needs some generalization

    token mod_internal:sym<:i>    { $<sym>=[':i'|':ignorecase'] » { $*ignorecase = 1 } }
    token mod_internal:sym<:!i>   { $<sym>=[':!i'|':!ignorecase'] » { $*ignorecase = 0 } }
    token mod_internal:sym<:i( )> { $<sym>=[':i'|':ignorecase'] <mod_arg> { $*ignorecase = eval $<mod_arg>.Str } }
    token mod_internal:sym<:0i>   { ':' (\d+) ['i'|'ignorecase'] { $*ignorecase = $0 } }

    token mod_internal:sym<:a>    { $<sym>=[':a'|':ignoreaccent'] » { $*ignoreaccent = 1 } }
    token mod_internal:sym<:!a>   { $<sym>=[':!a'|':!ignoreaccent'] » { $*ignoreaccent = 0 } }
    token mod_internal:sym<:a( )> { $<sym>=[':a'|':ignoreaccent'] <mod_arg> { $*ignoreaccent = eval $<mod_arg>.Str } }
    token mod_internal:sym<:0a>   { ':' (\d+) ['a'|'ignoreaccent'] { $*ignoreaccent = $0 } }

    token mod_internal:sym<:s>    { ':s' 'igspace'? » { $*sigspace = 1 } }
    token mod_internal:sym<:!s>   { ':!s' 'igspace'? » { $*sigspace = 0 } }
    token mod_internal:sym<:s( )> { ':s' 'igspace'? <mod_arg> { $*sigspace = eval $<mod_arg>.Str } }
    token mod_internal:sym<:0s>   { ':' (\d+) 's' 'igspace'? » { $*sigspace = $0 } }

    token mod_internal:sym<:r>    { ':r' 'atchet'? » { $*ratchet = 1 } }
    token mod_internal:sym<:!r>   { ':!r' 'atchet'? » { $*ratchet = 0 } }
    token mod_internal:sym<:r( )> { ':r' 'atchet'? » <mod_arg> { $*ratchet = eval $<mod_arg>.Str } }
    token mod_internal:sym<:0r>   { ':' (\d+) 'r' 'atchet'? » { $*ratchet = $0 } }
 
    token mod_internal:sym<:Perl5>    { [':Perl5' | ':P5'] [ :lang( $¢.cursor_fresh( %*LANG<P5Regex> ).unbalanced($*GOAL) ) <nibbler> ] }

    token mod_internal:adv {
        <?before ':' <.identifier> > [ :lang($¢.cursor_fresh(%*LANG<MAIN>)) <quotepair> ] { $/<sym> := «: $<quotepair><key>» }
    }

    token mod_internal:oops { ':'\w+ <.panic: "Unrecognized regex modifier"> }

    token quantifier:sym<*>  { <sym> <quantmod> }
    token quantifier:sym<+>  { <sym> <quantmod> }
    token quantifier:sym<?>  { <sym> <quantmod> }
    token quantifier:sym<**> { <sym> :: <normspace>? <quantmod> <normspace>?
        [
        | \d+ [ '..' [ \d+ | '*' ] ]?
        | <codeblock>
        | <quantified_atom>
        ]
    }

    token quantifier:sym<~~> {
        [
        | '!' <sym>
        | <sym>
        ]
        <normspace> <quantified_atom> }

    token quantmod { ':'? [ '?' | '!' | '+' ]? }

} # end grammar

############
# P5 Regex #
############

grammar P5Regex is STD {

    # begin tweaks (DO NOT ERASE)
    multi method tweak (:global(:$g)!) { self }
    multi method tweak (:ignorecase(:$i)!) { self }
    # end tweaks (DO NOT ERASE)

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

    # suppress fancy end-of-line checking
    token codeblock {
        :my $GOAL is context = '}';
        '{' :: [ :lang($¢.cursor_fresh(%*LANG<MAIN>)) <statementlist> ]
        [ '}' || <.panic: "Unable to parse statement list; couldn't find right brace"> ]
    }

    token ws {
        <?{ $*sigspace }>
        || [ <?before \s | '#'> <.nextsame> ]?   # still get all the pod goodness, hopefully
    }

    rule nibbler {
        :my $ignorecase is context<rw> = $*ignorecase // 0;
        <EXPR>
    }

    token termish {
        <.ws>  # XXX assuming old /x here?
        <noun=quantified_atom>+
    }
    token infixish {
        <!infixstopper>
        <!stdstopper>
        <rxinfix>
        {
            $<O> = $<rxinfix><O>;
            $<sym> = $<rxinfix><sym>;
        }
    }

    token rxinfix:sym<|> ( --> Junctive_or ) { <sym> }

    token quantified_atom {
        <!stopper>
        <!rxinfix>
        <atom>
        [ <.ws> <quantifier>
#            <?{ $<atom>.max_width }>
#                || <.panic: "Can't quantify zero-width atom">
        ]?
        <.ws>
    }

    token atom {
        [
        | \w
        | <metachar>
        | '\\' :: .
        ]
    }

    # sequence stoppers
    token metachar:sym<|>   { '|'  :: <fail> }
    token metachar:sym<)>   { ')'  :: <fail> }

    token metachar:quant { <quantifier> <.panic: "quantifier quantifies nothing"> }

    # "normal" metachars

    token metachar:sym<[ ]> {
        <before '['> <quibble($¢.cursor_fresh( %*LANG<Q> ).tweak(:q))> # XXX parse as q[] for now
    }

    token metachar:sym«(? )» {
        '(?' {} <assertion>
        [ ')' || <.panic: "Perl 5 regex assertion not terminated by parenthesis"> ]
    }

    token metachar:sym<( )> {
        '(' {} [:lang(self.unbalanced(')')) <nibbler>]?
        [ ')' || <.panic: "Unable to parse Perl 5 regex; couldn't find right parenthesis"> ]
        { $/<sym> := <( )> }
    }

    token metachar:sym<\\> { <sym> <backslash> }
    token metachar:sym<.>  { <sym> }
    token metachar:sym<^>  { <sym> }
    token metachar:sym<$>  {
        '$' <?before \W | $>
    }

    token metachar:var {
        <?before <sigil>\w>
        <.panic: "Can't interpolate variable in Perl 5 regex">
    }

    token backslash:A { <sym> }
    token backslash:a { <sym> }
    token backslash:b { :i <sym> }
    token backslash:c { :i <sym>
        <[ ?.._ ]> || <.panic: "Unrecognized \\c character">
    }
    token backslash:d { :i <sym> }
    token backslash:e { :i <sym> }
    token backslash:f { :i <sym> }
    token backslash:h { :i <sym> }
    token backslash:l { :i <sym> }
    token backslash:n { :i <sym> }
    token backslash:o { '0' [ <octint> | '{' ~ '}' <octints> ] }
    token backslash:p { :i <sym> '{' <[\w:]>+ '}' }
    token backslash:Q { <sym> }
    token backslash:r { :i <sym> }
    token backslash:s { :i <sym> }
    token backslash:t { :i <sym> }
    token backslash:u { :i <sym> }
    token backslash:v { :i <sym> }
    token backslash:w { :i <sym> }
    token backslash:x { :i <sym> [ <hexint> | '{' ~ '}' <hexints> ] }
    token backslash:z { :i <sym> }
    token backslash:misc { $<litchar>=(\W) | $<number>=(\d+) }
    token backslash:oops { <.panic: "Unrecognized Perl 5 regex backslash sequence"> }

    token assertion:sym<?> { <sym> <codeblock> }
    token assertion:sym<{ }> { <codeblock> }

    token assertion:sym«<» { <sym> <?before '=' | '!'> <assertion> }
    token assertion:sym<=> { <sym> [ <?before ')'> | <rx> ] }
    token assertion:sym<!> { <sym> [ <?before ')'> | <rx> ] }
    token assertion:sym«>» { <sym> <rx> }

    token rx {
        #[:lang(self.unbalanced(')')) <nibbler>]
        <nibbler>
        [ <?before ')'> || <.panic: "Unable to parse Perl 5 regex; couldn't find right parenthesis"> ]
    }

    #token assertion:identifier { <longname> [               # is qq right here?
    #                                | <?before ')' >
    #                                | <.ws> <nibbler>
    #                               ]
    #                               [ ':' <rx> ]?
    #}
    token p5mod { <[imox]>* }
    token p5mods { <on=p5mod> [ '-' <off=p5mod> ]? }
    token assertion:mod { <p5mods> [               # is qq right here?
                                   | ':' <rx>?
                                   | <?before ')' >
                                   ]
    }

    token assertion:bogus { <.panic: "Unrecognized Perl 5 regex assertion"> }

    token quantifier:sym<*>  { <sym> <quantmod> }
    token quantifier:sym<+>  { <sym> <quantmod> }
    token quantifier:sym<?>  { <sym> <quantmod> }
    token quantifier:sym<{ }> { '{' \d+ [','\d*]? '}' <quantmod> }

    token quantmod { [ '?' | '+' ]? }

} # end grammar

#################
# Symbol tables #
#################

use NAME;
use STASH;

method newpad {
    my $outer = $*CURPAD;
    my $line = self.lineof(self.pos);
    $*CURPAD = STASH.new(
        _file => $*FILE, _line => $line,
        _cf => $*FILE<name> ~ ':' ~ $line,
    );
    $*CURPAD<OUTER::> = $outer if $outer;
    self;
}

method finishpad($siggy = $*CURPAD.{'$?GOTSIG'}//0) {
    my $line = self.lineof(self.pos);
    $*CURPAD<$_> //= NAME.new( name => '$_', file => $*FILE, line => $line );
    $*CURPAD<$/> //= NAME.new( name => '$/', file => $*FILE, line => $line );
    $*CURPAD<$!> //= NAME.new( name => '$!', file => $*FILE, line => $line );
    if not $siggy {
        $*CURPAD<@_> = NAME.new( name => '@_', file => $*FILE, line => $line );
        $*CURPAD<%_> = NAME.new( name => '%_', file => $*FILE, line => $line );
        $*CURPAD<$?GOTSIG> = '';
    }
    self;
}

method is_name ($n, $curpad = $*CURPAD) {
    my $name = $n;
    say "is_name $name" if $*DEBUG +& DEBUG::symtab;

    my $curpkg = $*CURPKG;
    return True if $name ~~ /\:\:\(/;
    my @components = self.canonicalize_name($name);
    if @components > 1 {
        return True if @components[0] eq 'COMPILING::';
        return True if @components[0] eq 'CALLER::';
        return True if @components[0] eq 'CONTEXT::';
        if $curpkg = self.find_top_pkg(@components[0]) {
            say "Found lexical package ", @components[0] if $*DEBUG +& DEBUG::symtab;
            shift @components;
        }
        else {
            say "Looking for GLOBAL::<$name>" if $*DEBUG +& DEBUG::symtab;
            $curpkg = $*GLOBAL;
        }
        while @components > 1 {
            my $pkg = shift @components;
            $curpkg = $curpkg.{$pkg};
            return False unless $curpkg;
            say "Found $pkg okay" if $*DEBUG +& DEBUG::symtab;
        }
    }
    $name = shift(@components)//'';
    say "Looking for $name" if $*DEBUG +& DEBUG::symtab;
    return True if $name eq '';
    my $pad = $curpad;
    while $pad {
        say "Looking in ", $pad.cf if $*DEBUG +& DEBUG::symtab;
        if $pad.{$name} {
            say "Found $name in ", $pad.cf if $*DEBUG +& DEBUG::symtab;
            return True;
        }
        $pad = $pad.<OUTER::>;
    }
    return True if $curpkg.{$name};
    return True if $*GLOBAL.{$name};
    say "$name not found" if $*DEBUG +& DEBUG::symtab;
    return False;
}

method find_stash ($n, $curpad = $*CURPAD) {
    my $name = $n;
    say "find_stash $name" if $*DEBUG +& DEBUG::symtab;

    my $curpkg = $*CURPKG;
    return () if $name ~~ /\:\:\(/;
    my @components = self.canonicalize_name($name);
    if @components > 1 {
        return () if @components[0] eq 'COMPILING::';
        return () if @components[0] eq 'CALLER::';
        return () if @components[0] eq 'CONTEXT::';
        if $curpkg = self.find_top_pkg(@components[0]) {
            say "Found lexical package ", @components[0] if $*DEBUG +& DEBUG::symtab;
            shift @components;
        }
        else {
            say "Looking for GLOBAL::<$name>" if $*DEBUG +& DEBUG::symtab;
            $curpkg = $*GLOBAL;
        }
        while @components > 1 {
            my $pkg = shift @components;
            $curpkg = $curpkg.{$pkg};
            return () unless $curpkg;
            say "Found $pkg okay" if $*DEBUG +& DEBUG::symtab;
        }
    }
    $name = shift(@components)//'';
    return () if $name eq '';

    my $pad = $curpad;
    while $pad {
        return $_ if $_ = $pad.{$name};
        $pad = $pad.<OUTER::>;
    }
    return $_ if $_ = $curpkg.{$name};
    return $_ if $_ = $*GLOBAL.{$name};
    return ();
}

method find_top_pkg ($name) {
    say "find_top_pkg $name" if $*DEBUG +& DEBUG::symtab;
    if $name eq 'OUR::' {
        return $*CURPKG;
    }
    elsif $name eq 'MY::' {
        return $*CURPAD;
    }
    elsif $name eq 'CORE::' {
        return $*CORE;
    }
    elsif $name eq 'SETTING::' {
        return $*SETTING;
    }
    elsif $name eq 'UNIT::' {
        return $*UNIT;
    }
    # everything is somewhere in lexical scope (we hope)
    my $pad = $*CURPAD;
    while $pad {
        return $pad.{$name} if $pad.{$name};
        $pad = $pad.<OUTER::>;
    }
    return 0;
}

method add_name ($name) {
    my $scope = $*SCOPE // 'our';
    say "Adding $*SCOPE $name in $*PKGNAME" if $*DEBUG +& DEBUG::symtab;
    if $scope eq 'augment' or $scope eq 'supersede' {
        self.is_name($name) or self.worry("Can't $scope something that doesn't exist");
    }
    else {
        if $scope eq 'our' or $scope eq 'constant' {
            self.add_our_name($name);
        }
        else {
            self.add_my_name($name);
        }
    }
    self;
}

method add_my_name ($n, $d, $p) {
    my $name = $n;
    say "add_my_name $name" if $*DEBUG +& DEBUG::symtab;
    return self if $name ~~ /\:\:\(/;
    my $curstash = $*CURPAD;
    my @components = self.canonicalize_name($name);
    while @components > 1 {
        my $pkg = shift @components;
        my $newstash = $curstash.{$pkg} //= STASH.new( 'PARENT::' => $curstash );
        say "Adding new package $pkg in ", $curstash.cf if $*DEBUG +& DEBUG::symtab;
        $curstash = $newstash;
    }
    $name = shift @components;
    return self unless defined $name and $name ne '';
    return self if $name eq '$' or $name eq '@' or $name eq '%';

    # This may just be a lexical alias to "our" and such,
    # so reuse $*DECLARAND pointer if it's there.
    my $declaring = $d // NAME.new(
        xpad => $*CURPAD,
        name => $name,
        file => $*FILE, line => self.line,
        mult => ($*MULTINESS||'only'),
    );
    if $curstash.{$name}:exists and $curstash.{$name}<line> {
        say "$name exists, curstash = ", $curstash.cf if $*DEBUG +& DEBUG::symtab;
        my $omult = $curstash.{$name}<mult> // '';
        if $declaring === $curstash.{$name} {}  # already did this, probably enum
        elsif $*SCOPE eq 'use' {}
        elsif $*MULTINESS eq 'multi' and $omult ne 'only' {}
        elsif $omult eq 'proto' {}
        elsif $*PKGDECL eq 'role' {}
        else {
            my $old = $curstash.{$name};
            my $ofile = $old.file // 0;
            my $oline = $old.line // '???';
            my $loc = '';
            if $ofile {
                if $ofile !=== $*FILE {
                    my $oname = $ofile<name>;
                    $loc = " (from $oname line $oline)";
                }
                else {
                    $loc = " (from line $oline)";
                }
            }
            if $name ~~ /^\w/ {
                self.panic("Illegal redeclaration of lexical symbol $name$loc");
            }
            elsif $name ~~ s/^\&// {
                self.panic("Illegal redeclaration of lexical routine $name$loc");
            }
            else {  # XXX eventually check for conformant arrays here
                self.worry("Useless redeclaration of lexical variable $name$loc");
            }
        }
    }
    else {
        $*DECLARAND = $curstash.{$name} = $declaring;
        if $name ~~ /^\w+$/ and $*SCOPE ne 'constant' {
            $curstash.{"&$name"} //= $curstash.{$name};
            $curstash.{$name ~ '::'} //= ($p // STASH.new( 'PARENT::' => $curstash, _file => $*FILE, _line => self.line ));
        }
    }
    self;
}

method add_our_name ($n) {
    my $name = $n;
    say "add_our_name $name " ~ $*PKGNAME if $*DEBUG +& DEBUG::symtab;
    return self if $name ~~ /\:\:\(/;
    my $curstash = $*CURPKG;
    say "curstash $curstash global $GLOBAL ", join ' ', %$*GLOBAL if $*DEBUG +& DEBUG::symtab;
    $name ~~ s/\:ver\<.*?\>//;
    $name ~~ s/\:auth\<.*?\>//;
    my @components = self.canonicalize_name($name);
    if @components > 1 {
        my $c = self.find_top_pkg(@components[0]);
        if $c {
            shift @components;
            $curstash = $c;
        }
    }
    while @components > 1 {
        my $pkg = shift @components;
        my $newstash = $curstash.{$pkg} //= STASH.new('PARENT::' => $curstash );
        $curstash = $newstash;
        say "Adding new package $pkg in $curstash " if $*DEBUG +& DEBUG::symtab;
    }
    $name = shift @components;
    return self unless defined $name and $name ne '';

    my $declaring = $*DECLARAND // NAME.new(
        xpad => $*CURPAD,
        name => $name,
        file => $*FILE, line => self.line,
        mult => ($*MULTINESS||'only'),
    );
    if $curstash.{$name}:exists and $curstash.{$name}<line> {
        my $omult = $curstash.{$name}<mult> // '';
        if $declaring === $curstash.{$name} {} # already did it somehow
        elsif $*SCOPE eq 'use' {}
        elsif $*MULTINESS eq 'multi' and $omult ne 'only' {}
        elsif $omult eq 'proto' {}
        elsif $*PKGDECL eq 'role' {}
        else {
            my $old = $curstash.{$name};
            my $ofile = $old.file // 0;
            my $oline = $old.line // '???';
            my $loc = '';
            if $ofile {
                if $ofile !=== $*FILE {
                    my $oname = $ofile<name>;
                    $loc = " (from $oname line $oline)";
                }
                else {
                    $loc = " (from line $oline)";
                }
            }
            if $name ~~ /^\w/ {
                self.panic("Illegal redeclaration of package symbol $name$loc");
            }
            elsif $name ~~ s/^\&// {
                self.panic("Illegal redeclaration of package routine $name$loc");
            }
            else {  # XXX eventually check for conformant arrays here
                # (redeclaration of identical package vars is not useless)
            }
        }
    }
    else {
        $*DECLARAND = $curstash.{$name} = $declaring;
        if $name ~~ /^\w+$/ {
            $curstash.{"&$name"} //= $declaring;
            $curstash.{$name ~ '::'} //= STASH.new( 'PARENT::' => $curstash, _file => $*FILE, _line => self.line );
        }
    }
    self.add_my_name($n, $declaring, $curstash.{$name ~ '::'}) if $curstash === $*CURPKG;   # the lexical alias
    self;
}

method add_mystery ($name,$pos) {
    if not self.is_known($name) {
        say "add_mystery $name $*CURPAD" if $*DEBUG +& DEBUG::symtab;
        %*MYSTERY{$name}.<pad> = $*CURPAD;
        %*MYSTERY{$name}.<line> ~= self.lineof($pos) ~ ' ';
    }
    else {
        say "$name is known" if $*DEBUG +& DEBUG::symtab;
    }
    self;
}

method load_setting ($setting) {
    @PKGS = ();

    $*SETTING = self.load_pad($setting);
    $*CURPAD = $*SETTING;
    $*CORE = $*SETTING;
    for 1..100 {
        my $outerer = $*CORE.<OUTER::>;
        last unless $outerer;
        $*CORE = $outerer;
    }
    if $*CORE.<OUTER::> {
        warn "Internal error: infinite setting loop";
        $*CORE.<OUTER::>:delete;
    }
    $*GLOBAL = $*CORE.<GLOBAL::> = STASH.new(
        _file => $*FILE, _line => 1,
        _cf => 'GLOBAL',
    );
    $*CURPKG = $*GLOBAL;
}

method is_known ($n, $curpad = $*CURPAD) {
    my $name = $n;
    say "is_known $name" if $*DEBUG +& DEBUG::symtab;
    return True if $*QUASIMODO;
    return True if $*CURPKG.{$name};
    my $curpkg = $*CURPKG;
    my @components = self.canonicalize_name($name);
    if @components > 1 {
        return True if @components[0] eq 'COMPILING::';
        return True if @components[0] eq 'CALLER::';
        return True if @components[0] eq 'CONTEXT::';
        if $curpkg = self.find_top_pkg(@components[0]) {
            say "Found lexical package ", @components[0] if $*DEBUG +& DEBUG::symtab;
            shift @components;
        }
        else {
            say "Looking for GLOBAL::<$name>" if $*DEBUG +& DEBUG::symtab;
            $curpkg = $*GLOBAL;
        }
        while @components > 1 {
            my $pkg = shift @components;
            say "Looking for $pkg in $curpkg ", join ' ', keys(%$curpkg) if $*DEBUG +& DEBUG::symtab;
            $curpkg = $curpkg.{$pkg};
            return False unless $curpkg;
            say "Found $pkg okay, now in $curpkg " if $*DEBUG +& DEBUG::symtab;
        }
    }

    $name = shift(@components)//'';
    say "Final component is $name" if $*DEBUG +& DEBUG::symtab;
    return True if $name eq '';
    if $curpkg.{$name} {
        say "Found" if $*DEBUG +& DEBUG::symtab;
        return True;
    }

    my $pad = $curpad;
    while $pad {
        say "Looking in ", $pad.cf if $*DEBUG +& DEBUG::symtab;
        if $pad.{$name} {
            say "Found $name in ", $pad.cf if $*DEBUG +& DEBUG::symtab;
            return True;
        }
        say $pad.<OUTER::> // "No OUTER" if $*DEBUG +& DEBUG::symtab;
        $pad = $pad.<OUTER::>;
    }
    say "Not Found" if $*DEBUG +& DEBUG::symtab;

    return False;
}


method add_routine ($name) {
    my $vname = '&' ~ $name;
    self.add_name($vname);
    self;
}

method add_variable ($name) {
    if ($*SCOPE//'') eq 'our' {
        self.add_our_variable($name);
    }
    else {
        self.add_my_variable($name);
    }
    self;
}

method add_my_variable ($n) {
    my $name = $n;
    say "add_my_variable $name" if $*DEBUG +& DEBUG::symtab;
    if substr($name, 0, 1) eq '&' {
        self.add_my_name($name);
        if $name ~~ s/\:\(.*// {
            self.add_my_name($name);
        }
        return self;
    }
    self.add_my_name($name);
    self;
}

method add_our_variable ($name) {
    self.add_our_name($name);
    self;
}

method check_variable ($variable) {
    my $name = $variable.Str;
    say "check_variable $name" if $*DEBUG +& DEBUG::symtab;
    my ($sigil, $twigil, $first) = $name ~~ /(\W)(\W?)(.?)/;
    given $twigil {
        when '' {
            my $ok = 0;
            $ok = 1 if $name ~~ /::/;
            $ok ||= $*IN_DECL;
            $ok ||= $sigil eq '&';
            $ok ||= $first lt 'A';
            $ok ||= self.is_known($name);
            if not $ok {
                $variable.worry("Variable $name is not predeclared");
            }
        }
        when '^' {
            my $MULTINESS is context = 'multi';
            my $siggy = $*CURPAD.{'$?GOTSIG'}//'';
            if $siggy { $variable.panic("Placeholder variable $name cannot override existing signature $siggy"); }
            self.add_my_variable($name);
        }
        when ':' {
            my $MULTINESS is context = 'multi';
            self.add_my_variable($name);
        }
        when '~' {
            return %*LANG.{substr($name,2)};
        }
        when '?' {
            if $name ~~ /\:\:/ {
                my ($first) = self.canonicalize_name($name);
                $variable.worry("Unrecognized variable: $name") unless $first ~~ /^(CALLER|CONTEXT|OUTER|MY|SETTING|CORE)$/;
            }
            else {
                # search upward through languages to STD
                my $v = $variable.lookup_compiler_var($name);
                $variable.<value> = $v if $v;
            }
        }
    }
    self;
}

method lookup_compiler_var($name) {

    # see if they did "constant $?FOO = something" earlier
    my $lex = $*CURPAD.{$name};
    if defined $lex {
        if $lex.<thunk>:exists {
            return $lex.<thunk>.();
        }
        else {
            return $lex.<value>;
        }
    }

    given $name {
        when '$?FILE'     { return $*FILE<name>; }
        when '$?LINE'     { return self.lineof(self.pos); }
        when '$?POSITION' { return self.pos; }

        when '$?LANG'    { return item %*LANG; }

        when '$?STASH'    { return $*CURPAD; }

        when '$?PACKAGE'  { return $*CURPKG; }
        when '$?MODULE'   { return $*CURPKG; } #  XXX should scan
        when '$?CLASS'    { return $*CURPKG; } #  XXX should scan
        when '$?ROLE'     { return $*CURPKG; } #  XXX should scan
        when '$?GRAMMAR'  { return $*CURPKG; } #  XXX should scan

        when '$?PACKAGENAME' { return $*PKGNAME; }

        when '$?OS'       { return 'unimpl'; }
        when '$?DISTRO'   { return 'unimpl'; }
        when '$?VM'       { return 'unimpl'; }
        when '$?XVM'      { return 'unimpl'; }
        when '$?PERL'     { return 'unimpl'; }

        when '$?USAGE'    { return 'unimpl'; }

        when '&?ROUTINE'  { return 'unimpl'; }
        when '&?BLOCK'    { return 'unimpl'; }

        when '%?CONFIG'    { return 'unimpl'; }
        when '%?DEEPMAGIC' { return 'unimpl'; }

        # (derived grammars should default to nextsame, terminating here)
        default { self.worry("Unrecognized variable: $name"); return 0; }
    }
}

####################
# Service Routines #
####################

# token panic (Str $s) { <commit> <fail($s)> }

method panic (Str $s) {
    my $m;
    my $here = self;

    # Have we backed off recently?
    my $highvalid = self.pos <= $*HIGHWATER;

    $here = self.cursor($*HIGHWATER) if $highvalid;

    my $first = $here.lineof($*LAST_NIBBLE.<_from>);
    my $last = $here.lineof($*LAST_NIBBLE.<_pos>);
    if $first != $last {
        if $here.lineof($here.pos) == $last {
            $m ~= "\n(Possible runaway string from line $first)";
        }
        else {
            $first = $here.lineof($*LAST_NIBBLE_MULTILINE.<_from>);
            $last = $here.lineof($*LAST_NIBBLE_MULTILINE.<_pos>);
            # the bigger the string (in lines), the further back we suspect it
            if $here.lineof($here.pos) - $last < $last - $first  {
                $m ~= "\n(Possible runaway string from line $first to line $last)";
            }
        }
    }

    $m ~= "\n" ~ $s;

    if $highvalid {
        $m ~= $*HIGHMESS if $*HIGHMESS;
        $*HIGHMESS = $m;
    }
    else {
        # not in backoff, so at "bleeding edge", as it were... therefore probably
        # the exception will be caught and re-panicked later, so remember message
        $*HIGHMESS ~= "\n" ~ $s;
    }

    $m ~= $here.locmess;

    if $highvalid and %$*HIGHEXPECT {
        my @keys = sort keys %$*HIGHEXPECT;
        if @keys > 1 {
            $m ~= "\n    expecting any of:\n\t" ~ join("\n\t", sort keys %$*HIGHEXPECT);
        }
        else {
            $m ~= "\n    expecting @keys" unless @keys[0] eq 'whitespace';
        }
    }
    $m ~~ s|Confused|Confused (two terms in a row?)| if $m ~~ /infix|nofun/;

    if @*WORRIES {
        $m ~= "\nOther potential difficulties:\n  " ~ join( "\n  ", @*WORRIES);
    }
    $m ~= "\n";
    $m ~= self.explain_mystery();

    die $Cursor::RED ~ '===' ~ $Cursor::CLEAR ~ 'SORRY!' ~ $Cursor::RED ~ '===' ~ $Cursor::CLEAR ~ $m;
}

method worry (Str $s) {
    push @*WORRIES, $s ~ self.locmess;
    self;
}

method locmess () {
    my $pos = self.pos;
    my $line = self.lineof($pos);
    if $pos >= @*MEMOS - 2 {
        $pos = @*MEMOS - 3;
        $line = ($line - 1) ~ " (EOF)";
    }
    my $pre = substr($*ORIG, 0, $pos);
    $pre = substr($pre, -40, 40);
    1 while $pre ~~ s!.*\n!!;
    $pre = '<BOL>' if $pre eq '';
    my $post = substr($*ORIG, $pos, 40);
    1 while $post ~~ s!(\n.*)!!;
    $post = '<EOL>' if $post eq '';
    " at " ~ $*FILE<name> ~ " line $line:\n------> " ~ $Cursor::GREEN ~ $pre ~ $Cursor::YELLOW ~ $*PERL6HERE ~ $Cursor::RED ~ 
        "$post$Cursor::CLEAR";
}

method line {
    self.lineof(self.pos);
}

method lineof ($p) {
    return 1 unless defined $p;
    my $line = @*MEMOS[$p]<L>;
    return $line if $line;
    $line = 0;
    my $pos = 0;
    my @text = split(/^/,$*ORIG);   # XXX p5ism, should be ^^
    for @text {
        $line++;
        @*MEMOS[$pos++]<L> = $line
            for 1 .. chars($_);
    }
    @*MEMOS[$pos++]<L> = $line;
    return @*MEMOS[$p]<L> // 0;
}

method SETGOAL { }
method FAILGOAL (Str $stop, Str $name) {
    self.panic("Unable to parse $name; couldn't find final '$stop'");
}

# "when" arg assumes more things will become obsolete after Perl 6 comes out...
method obs (Str $old, Str $new, Str $when = ' in Perl 6') {
    %$*HIGHEXPECT = ();
    self.panic("Obsolete use of $old;$when please use $new instead");
}

method worryobs (Str $old, Str $new, Str $when = ' in Perl 6') {
    self.worry("Possible obsolete use of $old;$when please use $new instead");
    self;
}

# Since most keys are valid prefix operators or terms, this rule is difficult
# to reach ('say »+«' works), but it's okay as a last-ditch default anyway.
token term:sym<miscbad> {
    {} <!{ $*QSIGIL }> <infixish> <.badinfix($<infixish>.Str)>
}

method badinfix (Str $bad = $*sym) {
    self.panic("Preceding operator expects term, but found infix $bad instead");
}

## vim: expandtab sw=4 ft=perl6
