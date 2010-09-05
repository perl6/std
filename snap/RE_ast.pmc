# RE_ast.pmc
#
# Copyright 2009-2010, Larry Wall
#
# You may copy this software under the terms of the Artistic License,
#     version 2.0 or later.

package main;
use utf8;
use strict; use warnings;
use DEBUG;
use Encode;

# The DFA engine has two priorities; top priority is to generate the correct
# pattern prefixes; second is to generate as much fate as it can.

# [conjectural]
# We use two data structures to represent NFAs.  The NFA description tree
# (NFA::* but not NFA::Node, NFA::Lazy) is statically built; it is a near 1:1
# mapping of the RE_ast structure.  The NFA description tree is used to
# generate the NFA construction tree, which is lazily built.
{
    package NFA::Lazy;
    sub new   { bless [ @_ ], 'NFA::Lazy' }
    sub reify {
        my $self = shift;
        my ($node, $prefix, $continue) = splice @$self;
        bless $self, 'NFA::Node';
        $node->construct($self, $prefix, $continue);
    }
}

{
    package NFA::Node;
    sub reify { }
}

{
    package NFA::seq;
    sub new {
        my ($left, $right) = @_;
        my $literal = $left->{literal};
        my $litlen  = $left->{litlen};
        if ($literal) {
            $literal &&= $right->{literal};
            $litlen  +=  ($right->{litlen} // 0);
        }
        bless { left => $left, right => $right, literal => $literal,
            litlen => $litlen, fates => ($left->{fates} || $right->{fates}) },
            'NFA::seq';
    }

    sub construct {
        my ($self, $node, $pre_fates, $continue) = @_;

        $self->{left}->construct($node, $pre_fates, sub {
                my $mid_fates = shift;
                NFA::Lazy->new($self->{right}, $mid_fates, $continue);
            });
    }
}

#############################################################
# longest token set generator
#############################################################

#    $::DEBUG |= -1;
sub qm { my $s = shift;
    $s = $s->[0] if ref $s eq 'ARRAY';	# only count first token of circumfix or postcircumfix
    my $r = '';
    for (split(//,$s)) {
	if ($_ eq " ") { $r .= '\x20' }
	elsif ($_ eq "\t") { $r .= '\t' }
	elsif ($_ eq "\n") { $r .= '\n' }
	elsif ($_ =~ m/^\w$/) { $r .= $_ }
	elsif ($_ eq '<' | $_ eq '>') { $r .= $_ }
	else { $r .= '\\' . $_ }
    }
    $r;
}

sub here {
    return unless $::DEBUG & DEBUG::longest_token_pattern_generation;
    my $arg = shift;
    my $lvl = 0;
    while (caller($lvl)) { $lvl++ }
    my ($package, $file, $line, $subname, $hasargs) = caller(0);

    my $name = $package;   # . '::' . substr($subname,1);
    if (defined $arg) { 
	$name .= " " . $arg;
    }
    ::deb("\t", ':' x $lvl, ' ', $name, " [", $file, ":", $line, "]") if $::DEBUG & DEBUG::longest_token_pattern_generation;
}

{ package nfa;

# Rules: Don't call $cont more than once with the same fate.  Don't instantiate
# a node more than once with the same fate.
sub node {
    my $id = @::NFANODES;
    #::deb("creating direct node $id") if $::DEBUG & DEBUG::longest_token_pattern_generation;
    push @::NFANODES, [ $id, @_ ];
    $id;
}

sub gnode {
    my $id = @::NFANODES;
    #::deb("creating node $id via " . ref($_[0])) if $::DEBUG & DEBUG::longest_token_pattern_generation;
    push @::NFANODES, [ $id ];
    $_[0]->construct($::NFANODES[$id], $_[1], $_[2]);
    $id;
}

sub rgnode { my ($ob, $n, $f, $c) = @_;
    #::deb("forwarding node " . $n->[0] . " to " . ref($ob)) if $::DEBUG & DEBUG::longest_token_pattern_generation;
    $ob->construct($n, $f, $c);
}

sub nfa::null::construct { my ($self, $node, $fate, $cont) = @_;
    push @$node, $cont ? (undef, undef, $cont->($fate)) : ($fate);
}

sub nfa::imp::construct { my ($self, $node, $fate, $cont) = @_;
    push @$node, $fate;
}

our $NULL = bless({ m => [], nr => 0, l => 1, ll => 0 }, 'nfa::null');
our $IMP  = bless({ m => [], nr => 1, l => 0, ll => 0 }, 'nfa::imp');

# When a non-LTM alternation or quantifier is applied to a subregex, it becomes
# impossible to control where subsequent tokens match, so we can't copy fates.
sub nfa::horizon::construct { my ($self, $node, $fate, $cont) = @_;
    my @fate = @$fate;
    $fate[0] = 1;
    nfa::rgnode($self->{i}, $node, \@fate, $cont);
}
sub horizon { my ($inner) = @_;
    bless({ m => $inner->{m}, nr => $inner->{nr}, l => $inner->{l},
            ll => $inner->{ll}, i => $inner }, 'nfa::horizon');
}

sub method { my ($mp, $inner) = @_;
    bless({ %$inner, m => [ @{ $inner->{m} }, $mp ] }, ref($inner));
}

sub noreturn { $_[0]{nr} }

sub nfa::seq::construct { my ($self, $node, $fate, $cont) = @_;
    nfa::rgnode($self->{fst}, $node, $fate, sub {
            nfa::gnode($self->{snd}, $_[0], $cont) });
}
sub seq { my ($fst, $snd) = @_;
    bless({ m  => [ @{ $fst->{m} }, @{ $snd->{m} } ],
            nr => $fst->{nr} || $snd->{nr}, l => $fst->{l} && $snd->{l},
            ll => ($fst->{l} ? $fst->{ll} + $snd->{ll} : $fst->{ll}),
            fst => $fst, snd => $snd }, 'nfa::seq');
}

sub nfa::star::construct { my ($self, $node, $fate, $cont) = @_;
    my @fate = @$fate;
    $fate[0] = 1;
    push @$node, ($cont ? (undef, undef, $cont->(\@fate)) : (\@fate)),
        undef, nfa::gnode($self->{i}, \@fate, sub { $node->[0] });
}
sub star { my ($in) = @_;
    bless({ m => $in->{m}, nr => 0, l => 0, ll => 0, i => $in },
            'nfa::star');
}

sub nfa::opt::construct { my ($self, $node, $fate, $cont) = @_;
    my @fate = @$fate;
    $fate[0] = 1;
    my $end = $cont ? $cont->(\@fate) : nfa::node(\@fate);
    push @$node, undef, undef, $end,
        undef, nfa::gnode($self->{i}, \@fate, sub { $end });
}
sub opt { my ($in) = @_;
    bless({ m => $in->{m}, nr => 0, l => 0, ll => 0, i => $in },
            'nfa::opt');
}

sub nfa::ltm::construct { my ($self, $node, $fate, $cont) = @_;
    push @$node, undef;
    if ($fate->[0]) {
        my $end;
        for my $br (@{ $self->{br} }) {
            push @$node, undef, nfa::gnode($br->[1], $fate,
                sub { $end //= $cont->($fate) });
        }
    } else {
        my $ix;
        for my $br (@{ $self->{br} }) {
            my @fate = @$fate;
            push @fate, $self->{t}, $br->[0], pack("NN",
                ~($br->[1]{ll}), $ix++);
            push @$node, undef, nfa::gnode($br->[1], \@fate, $cont);
        }
    }
}
sub ltm { my ($tag, @branches) = @_;
    my $nr = 1;
    my @m;
    for (@branches) {
        $nr &&= $_->[1]{nr};
        push @m, @{ $_->[1]{m} };
    }
    bless({ m => \@m, nr => $nr, l => 0, ll => 0, t => $tag, br => \@branches },
            'nfa::ltm');
}

sub nfa::cclass::construct { my ($self, $node, $fate, $cont) = @_;
    my $end = $cont ? $cont->($fate) : nfa::node($fate);
    push @$node, undef, map { $_, $end } @{ $self->{t} };
}
sub cclass { my @terms = @_;
    bless({ m => [], nr => 0, l => 0, ll => 0, t => \@terms }, 'nfa::cclass');
}

sub nfa::string::construct { my ($self, $node, $fate, $cont) = @_;
    my ($i, $t) = @{ $self }{ 'i', 't' };
    if ($t eq '') {
        nfa::rgnode($NULL, $node, $fate, $cont);
    } else {
        my @nexts = ((map { nfa::node() } (1 .. length($t) - 1)),
            ($cont ? $cont->($fate) : nfa::node($fate)));
        for my $ch (split //, $t) {
            push @$node, undef, map { [$_], $nexts[0] }
                ($i ? (uc($ch), lc($ch)) : $ch);
            $node = $::NFANODES[$nexts[0]];
            shift @nexts;
        }
    }
}
sub string { my ($i, $text) = @_;
    bless({ m => [], nr => 0, l => 1, ll => length($text), i => $i,
            t => $text }, 'nfa::string');
}
}

my $IMP = $nfa::IMP;
my $NULL = $nfa::NULL;

{ package REbase;
}

{ package RE_ast; our @ISA = 'REbase';
    sub nfa { my $self = shift; my $C = shift;
        ::here();
        $self->{'re'}->nfa($C);
    }
}

{ package RE_assertion; our @ISA = 'REbase';
    sub nfa { my ($self, $C) = @_;
        if ($self->{assert} eq '?') {
            my $re = $self->{re};
            return nfa::seq($re->nfa($C), $IMP);
        }
        return $NULL;
    }
}

{ package RE_assertvar; our @ISA = 'REbase';
    sub nfa { $IMP }
}

{ package RE_block; our @ISA = 'REbase';
    sub nfa { $IMP }
}

{ package RE_bindvar; our @ISA = 'REbase';
    sub nfa { my $self = shift; my $C = shift; ::here();
        $self->{'atom'}->nfa($C);
    }
}

{ package RE_bindnamed; our @ISA = 'REbase';
    sub nfa { my $self = shift; my $C = shift; ::here();
        $self->{'atom'}->nfa($C);
    }
}

{ package RE_bindpos; our @ISA = 'REbase';
    sub nfa { my $self = shift; my $C = shift; ::here();
        $self->{'atom'}->nfa($C);
    }
}

{ package RE_bracket; our @ISA = 'REbase';
    sub nfa { my $self = shift; my $C = shift; ::here();
        $self->{'re'}->nfa($C);
    }
}

{ package RE_cclass; our @ISA = 'REbase';
    sub _get_char {
        if ($_[0] =~ s/^([^\\])//s) { return ord($1) }
        if ($_[0] =~ s/^\\n//)   { return 10 }
        if ($_[0] =~ s/^\\t//)   { return 9 }
        if ($_[0] =~ s/^\\x\{(.*?)\}//s)   { return hex($1); }
        if ($_[0] =~ s/^\\x(..)//s)   { return hex($1); }
        if ($_[0] =~ s/^\\(.)//s)   { return ord($1) }

        return undef;
    }

    sub nfa { my ($self, $C) = @_; ::here($self->{text});
        $CursorBase::fakepos++;
        my $cc = $self->{'text'};
        Encode::_utf8_on($cc);
        my ($neg, $text) = $cc =~ /^(-?)\[(.*)\]$/s;
        die "whoops! $cc" unless defined $text;

        #XXX this ought to be pre parsed
        my ($ch, $ch2);
        my @chs;
        while (1) {
            $text =~ s/^\s+//;
            if ($text =~ s/^\\s//) {
                push @chs, 'Space/Y';
                next;
            }
            if ($text =~ s/^\\w//) {
                push @chs, '_', 'Gc/L', 'Gc/N';
                next;
            }
            last if $text eq '';
            $ch = _get_char($text);
            if ($text =~ s/^\s*\.\.//) {
                $ch2 = _get_char($text);
            } else {
                $ch2 = $ch;
            }
            push @chs, map { chr $_ } ($ch .. $ch2);
        }

        if ($self->{i}) {
            @chs = map { uc($_), lc($_) } @chs;
        }

        $neg ? nfa::cclass(['ALL', @chs]) : nfa::cclass(map { [$_] } @chs);
    }
}

{ package RE_decl; our @ISA = 'REbase';
    sub nfa { $NULL }
}

{ package RE_double; our @ISA = 'REbase';
    # XXX inadequate for "\n" without interpolation
    sub nfa { my ($self, $C) = @_;
        my $text = $self->{'text'};
        Encode::_utf8_on($text);
        ::here($text);
        $Cursor::fakepos++ if $text ne '';
        my ($fixed, $imp);
        if ( $text =~ /^(.*?)[\$\@\%\&\{]/ ) {
            $fixed = $1; $imp = 1;
        }
        else {
            $fixed = $text;
        }
        $fixed = nfa::string($self->{i}, $fixed);
        $fixed = nfa::seq($fixed, $IMP) if $imp;
        $fixed;
    }
}

{ package RE_meta; our @ISA = 'REbase';
    my %meta_nfa = (
        # XXX I don't think these are quite right
        '^' => $NULL, '^^' => $NULL, '$$' => $NULL, '$' => $NULL,
        '«' => $NULL, '<<' => $NULL, '>>' => $NULL, '»' => $NULL,
        # what?
        '\\\\' => nfa::cclass(['\\']),
        '\\"' =>  nfa::cclass(['"']),
        '\\\'' => nfa::cclass(["'"]),
        '\D' =>   nfa::cclass(['ALL', 'Gc/N']),
        '\d' =>   nfa::cclass(['Gc/N']),
        '\H' =>   nfa::cclass(['ALL', 'Perl/Blank']),
        '\h' =>   nfa::cclass(['Perl/Blank'], ["\015"]),
        '\N' =>   nfa::cclass(['ALL', "\n"]),
        '\n' =>   nfa::cclass(["\n"]),
        '\S' =>   nfa::cclass(['ALL', 'Space/Y']),
        '\s' =>   nfa::cclass(['Space/Y']),
        '\V' =>   nfa::cclass(['ALL', 'Perl/VertSpac']),
        '\v' =>   nfa::cclass(['Perl/VertSpac']),
        '\W' =>   nfa::cclass(['ALL', '_', 'Gc/L', 'Gc/N']),
        '\w' =>   nfa::cclass(['_'], ['Gc/L'], ['Gc/N']),
        '.'  =>   nfa::cclass(['ALL']),
        '::' =>   $IMP,
        ':::' =>  $IMP,
        '.*?' =>  $IMP,
        '.*' =>   nfa::star(nfa::cclass(['ALL'])),
    );

    sub nfa { my $self = shift; my ($C) = @_; 
        my $text = $self->{'text'};
        Encode::_utf8_on($text);
        ::here($text);
        return $meta_nfa{$text} // die "unhandled meta $text";
    }
}

{ package RE_method; our @ISA = 'REbase';
    sub nfa { my ($self, $C) = @_;
        my $name = $self->{'name'};
        return $IMP if $self->{'rest'};
        Encode::_utf8_on($name);
        ::here($name);

        if ($name eq 'null' or $name eq 'ww') { return $NULL }
        if ($name eq 'ws') { return $IMP; }
        if ($name eq 'alpha') { $CursorBase::fakepos++; return nfa::cclass(['_'], ['Gc/L']); }
        if ($name eq 'sym') {
            $CursorBase::fakepos++;
            my $sym = $self->{'sym'};
            Encode::_utf8_on($sym);
            return nfa::string($self->{i}, $sym);
        }

        # XXX
        $name = 'termish' if $name eq 'EXPR';

        my $mname = $name . '__PEEK';
        my $lexer = $C->can($mname) ? $C->$mname()->{NFAT} : $IMP;
        return nfa::method($name, $lexer);
    }
}

{ package RE_method_internal; our @ISA = 'REbase';
    sub nfa { $IMP }
}

{ package RE_method_re; our @ISA = 'REbase';
    sub nfa { my ($self, $C) = @_;
        my $name = $self->{name};
        Encode::_utf8_on($name);
        ::here($name);
        my $re = $self->{re};
        if ($name eq '') {
            return $IMP;
        } elsif ($name eq 'after') {
            return $NULL;
        } elsif ($name eq 'before') {
            return nfa::seq($re->nfa($C), $IMP);
        } else {
            my $mname = $name . '__PEEK';
            my $lexer = $C->can($mname) ? $C->$mname($re) : $IMP;
            return nfa::method($name, $lexer->{NFAT});
        }
    }
}

{ package RE_noop; our @ISA = 'REbase';
    sub nfa { $NULL }
}

{ package RE_every; our @ISA = 'REbase';
    sub nfa { $IMP }
}

{ package RE_first; our @ISA = 'REbase';
    sub nfa { my ($self, $C) = @_;
        my $alts = $self->{'zyg'};
        ::here(0+@$alts);
        nfa::horizon($alts->[0]->nfa($C));
    }
}

{ package RE_paren; our @ISA = 'REbase';
    sub nfa { my $self = shift; my $C = shift; ::here();
        $self->{'re'}->nfa($C);
    }
}

{ package RE_quantified_atom; our @ISA = 'REbase';
    sub nfa { my ($self, $C) = @_; ::here();
        my $oldfakepos = $CursorBase::fakepos++;
        my $subnfa = $self->{atom}->nfa($C);
        #return $IMP if $self->{quant}[1];  XXX viv omits this currently
        # XXX S05 is not quite clear; it could be read as saying to cut LTM
        # *after* the atom
        return $IMP if $self->{quant}[2]
            && $self->{quant}[2]->isa('RE_block');

        my $k = $self->{quant}[0];
        if ($k eq '?') {
            return nfa::opt($subnfa);
        } elsif ($k eq '*') {
            return nfa::star($subnfa);
        } elsif ($k eq '+') {
            return nfa::seq($subnfa, nfa::star($subnfa));
        } elsif ($k eq '**') {
            my $subnfa2 = $self->{quant}[2]->nfa($C);
            return nfa::seq($subnfa, nfa::star(nfa::seq($subnfa2, $subnfa)));
        } else {
            die "unknown quantifier $k";
        }
    }
}

{ package RE_qw; our @ISA = 'REbase';
    sub nfa { my ($self, $C) = @_;
        my $text = $self->{'text'};
        Encode::_utf8_on($text);
        ::here($text);
        $CursorBase::fakepos++;
        $text =~ s/^<\s*//;
        $text =~ s/\s*>$//;

        nfa::horizon(nfa::ltm("", map { ["", nfa::string($self->{i}, $_)] } split(/\s+/, $text)));
    }
}

{ package RE_sequence; our @ISA = 'REbase';
    sub nfa { my ($self, $C) = @_; ::here;
        my @zyg;
        for my $k (@{ $self->{zyg} }) {
            push @zyg, $k->nfa($C);
            last if nfa::noreturn($zyg[-1]);
        }
        push @zyg, $NULL if !@zyg;
        while (@zyg > 1) {
            push @zyg, nfa::seq(splice(@zyg, -2, 2));
        }
        $zyg[0];
    }
}

{ package RE_string; our @ISA = 'REbase';
    sub nfa { my ($self, $C) = @_;
        my $text = $self->{'text'};
        Encode::_utf8_on($text);
        ::here($text);
        $CursorBase::fakepos++ if $text ne '';
        nfa::string($self->{i}, $text);
    }
}

{ package RE_submatch; our @ISA = 'REbase';
    sub nfa { $IMP }
}

{ package RE_all; our @ISA = 'REbase';
    sub nfa { $IMP }
}

{ package RE_any; our @ISA = 'REbase';
    sub nfa { my $self = shift; my ($C) = @_; 
        my $alts = $self->{'zyg'};
        ::here(0+@$alts);
        my @outs;
        my $oldfakepos = $CursorBase::fakepos;
        my $minfakepos = $CursorBase::fakepos + 1;
        my $ix = 0;

        for my $alt (@$alts) {
            $CursorBase::fakepos = $oldfakepos;

            push @outs, [ $ix++, $alt->nfa($C) ];

            $minfakepos = $oldfakepos if $CursorBase::fakepos == $oldfakepos;
        }
        $CursorBase::fakepos = $minfakepos;  # Did all branches advance?
        nfa::ltm($self->{altname}, @outs);
    }
}

{ package RE_var; our @ISA = 'REbase';
    sub nfa { my ($self, $C) = @_;
        my $var = $self->{var};
        if (my $p = $C->_PARAMS) {
            my $text = $p->{$var} || return $IMP;
            $CursorBase::fakepos++ if length($text);
            return nfa::string($self->{i}, $text);
        }
        return $IMP;
    }
}

1;
