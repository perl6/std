grammar STD::P5 is STD;

use DEBUG;

grammar Regex is STD {

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

    proto token regex_infix { <...> }

    # suppress fancy end-of-line checking
    token codeblock {
        :my $*GOAL ::= '}';
        '{' :: [ :lang($¢.cursor_fresh(%*LANG<MAIN>)) <statementlist> ]
        [ '}' || <.panic: "Unable to parse statement list; couldn't find right brace"> ]
    }

    token ws {
        <?{ $*sigspace }>
        || [ <?before \s | '#'> <.nextsame> ]?   # still get all the pod goodness, hopefully
    }

    rule nibbler {
        :temp $*ignorecase;
        <EXPR>
    }

    token termish {
        <.ws>  # XXX assuming old /x here?
        <noun=quant_atom_list>
    }
    token quant_atom_list {
        <quantified_atom>+
    }
    token infixish {
        <!infixstopper>
        <!stdstopper>
        <regex_infix>
        {
            $<O> = $<regex_infix><O>;
            $<sym> = $<regex_infix><sym>;
        }
    }

    token regex_infix:sym<|> ( --> Junctive_or ) { <sym> }

    token quantified_atom {
        <!stopper>
        <!regex_infix>
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
    token backslash:o { :dba('octal character') '0' [ <octint> | '{' ~ '}' <octints> ] }
    token backslash:p { :i <sym> '{' <[\w:]>+ '}' }
    token backslash:Q { <sym> }
    token backslash:r { :i <sym> }
    token backslash:s { :i <sym> }
    token backslash:t { :i <sym> }
    token backslash:u { :i <sym> }
    token backslash:v { :i <sym> }
    token backslash:w { :i <sym> }
    token backslash:x { :i :dba('hex character') <sym> [ <hexint> | '{' ~ '}' <hexints> ] }
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
        # [:lang(self.unbalanced(')')) <nibbler>]
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

