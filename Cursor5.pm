our $CTX;
$CTX->{lvl} = 0;
package Cursor5;

my $lexverbose = 1;
my $DEBUG = 1;

use strict;
use warnings;
use Encode;

our %AUTOLEXED;
our $PURE;
our $ALT;
our $PREFIX = "";

binmode(STDIN, ":utf8");
binmode(STDERR, ":utf8");
binmode(STDOUT, ":utf8");

use Carp;
use utf8;

$SIG{__DIE__} = sub { confess(@_) };

sub new {
    my $class = shift;
    my %args = ('pos' => 0, 'from' => 0);
    while (@_) {
	my $name = shift;
	if ($name eq 'orig') {
	    $args{$name} = \shift;
	}
	else {
	    $args{$name} = shift;
	}
    }
    my $self = bless \%args, ref $class || $class;
    my $buf = $self->{orig};
    print STDERR " orig ", $$buf,"\n" if $DEBUG;
    $self->BUILD;
    $self->_AUTOLEXpeek('Perl::expect_term');
    $self;
}

use YAML::XS;

my $VERBOSE = 1;
my $RE_verbose = 1;

# XXX still full of ASCII assumptions

# most cursors just copy forward the previous value of the following two items:
#has $self->{orig};        # per match, the original string we are matching against
our %lexers;       # per language, the cache of lexers, keyed by rule name

#has Bool $.bool = 1;
#has StrPos $.from = 0;
#has StrPos $.to = 0;
#has StrPos $.pos = 0;
#has Cursor $.prior;
#has %.mykeys};
#has Str $.name;
#has $.item;

sub bool { $_[0]->{bool} }
sub from { $_[0]->{from} }
sub to { $_[0]->{to} }
sub pos { $_[0]->{pos} }
sub name { $_[0]->{name} }

# unwarranted chumminess with Perl grammar
sub ws_from { $_[0]->{name} }
sub ws_to { $_[0]->{name} }

sub lexers { my $self = shift;
    \%lexers;    # XXX should be different per language, sigh
}

my $fakepos = 1;

sub _AUTOLEXpeek { my $self = shift;
    my $key = shift;

    print STDERR "?" x 72, "\n" if $DEBUG;
    print STDERR "AUTOLEXpeek $key\n" if $DEBUG;
    die "Null key" if $key eq '';
    if ($self->lexers->{$key}) {
        if ($AUTOLEXED{$key}) {   # no left recursion allowed in lexer!
            die "Left recursion in $key" if $fakepos == $AUTOLEXED{$key};
            print STDERR "Suppressing lexer recursion on $key\n" if $DEBUG;
            return hash();  # (but if we advanced just assume a :: here)
        }
        elsif (ref($self->lexers->{$key}) eq 'Hash') {
            return $self->lexers->{$key} // hash();
        }
        else {
            print STDERR "oops ", ref($key), "\n";
        }
    }
    return $self->lexers->{$key} = $self->_AUTOLEXgen($key);
}

sub _AUTOLEXgen { my $self = shift;
    my $key = shift;

    print STDERR "=" x 72, "\n" if $DEBUG;
    print STDERR "AUTOLEXgen $key\n" if $DEBUG;
    my $lexer = {};
    (my $file = $key) =~ s/::/--/g;
    $file =~ s/^Perl--//;
    if (-s "lex/$file") {
	$lexer = loadlexer($key);
    }
    else {
	{ package RE_base; 1; }
	my $ast = $::RE{$key};	# should be per package
	my $oldfakepos = $AUTOLEXED{$key} // 0;

	$AUTOLEXED{$key} = $fakepos;
	my @pat = $ast->longest($self);
	for (@pat) {
	    s/(\t\(\?#FATE.*?\))(.*)/$2$1/;
	    s/^\t/.\t/;		# empty pattern, match anything
	}
	warn "(null pattern)" unless @pat;
	my $pat = join("\n", @pat);

	$AUTOLEXED{$key} = $oldfakepos;

	$lexer = { PATS => [@pat] };
	mkdir("lex") unless -d "lex";
	open(my $cache, '>', "lex/$file") // die "Can't print: $!";
	binmode($cache, ":utf8");
	print $cache join("\n",@pat),"\n" or die "Can't print: $!";
	close($cache) or die "Can't close: $!";
	print STDERR "regenerated lex/$file\n" if $DEBUG;
	# force operator precedence method to look like a term
	if ($file eq 'Perl--expect_term') {
	    system 'cp lex/Perl--expect_term lex/Perl--EXPR';
	}
    }
    $lexer;
}

sub loadlexer {
    my $key = shift;
    (my $file = $key) =~ s/::/--/g;
    $file =~ s/^Perl--//;
    print STDERR "using cached lex/$file\n" if $DEBUG;

    my @fates = ('');
    open(LEX, "lex/$file") or die "No lexer!";
    binmode(LEX, ":utf8");

    my @pat = <LEX>;
    chomp(@pat);
    close LEX;

    return {"PATS" => \@pat};
}

# Can the current pattern match the current position?

sub canmatch {
    my ($p,$c) = @_;
    my $f = substr($p,0,1,'');
    if ($f eq '\\') {
	if ($p =~ s/^(\W)//) {
	    return 1 if $c eq $1;
	}
	elsif ($p =~ s/^(\w)//) {
	    $f .= $1;
	    if ($1 eq 'x') {
		if ($p =~ s/^(\w\w)//) {
		    $f .= $1;
		}
		elsif ($p =~ s/^(\[\w+\])//) {
		    $f .= $1;
		}
	    }
	    return 1 if $c =~ /^$f/;
	}
    }
    elsif ($f eq '[') {
	if ($p =~ s/^(\^?.[^]]*\])//) {
	    $f .= $1;
	    return 1 if $c =~ /^$f/;
	}
    }
    elsif ($f eq '(') {
	if ($p =~ s/^(\?:[^)]*\))//) {
	    $f .= $1;
	    return 1 if $c =~ /^$f/;
	}
    }
    elsif ($f eq '.') {
	return 1;
    }
    elsif ($f eq $c) {
	return 1;
    }
    # nullable first char?
    if ($p =~ s/^[*?]//) {
	return canmatch($p,$c);
    }
    return 0;
}

sub _AUTOLEXnow { my $self = shift;
    my $key = shift;

    print STDERR "!" x 72, "\n" if $DEBUG;
    print STDERR "AUTOLEXnow $key\n" if $DEBUG;
    my $lexer = $self->lexers->{$key} // do {
	local %AUTOLEXED;
	$self->_AUTOLEXpeek($key);
    };
    my $buf = $self->{orig};
    my $P = $self->{pos};
    if ($P == length($$buf)) {
	return sub { '' };
    }
    my $chr = substr($$buf,$self->{pos},1);

    $lexer->{$chr} //= do {
	print STDERR '=' x 72, "\n" if $DEBUG;
	print STDERR "GENERATING $key patterns starting with '$chr'\n" if $DEBUG;

	my @pats = grep { canmatch($_, $chr) } @{$lexer->{PATS}};
	if (!@pats) {
	    print STDERR "No $key patterns start with '$chr'\n" if $DEBUG;
	    sub { '' };
	}
	else {
	    my $i = 1;
	    my $fate = [];
	    for (@pats) {
		s/\(\?#FATE (.*?)\)/(?#@{ $fate->[$i] = $1; [$i++]} FATE $1)/;
	    }

	    my $pat = "^(?:\n(" . join(")\n|(",@pats) . '))';

	    # extract fate comments before they are deleted
	    print STDERR "LEXER: ", $pat,"\n" if $lexverbose;

	    # remove stuff that will confuse TRE greatly
	    $pat =~ s/\(\?#.*?\)//g;
	    $pat =~ s/^[ \t]*\n//gm;
	    $pat =~ s/\s+//g;
	    $pat =~ s/:://g;

	    1 while $pat =~ s/\(\?:\)\??//;
	    1 while $pat =~ s/([^\\])\(((\?:)?)\)/$1($2 !!!OOPS!!! )/;
	    1 while $pat =~ s/\[\]/[ !!!OOPS!!! ]/;
	    $pat =~ s/\\x20/ /g;
	    my $tmp = $pat;
	    "42" =~ /$tmp/;	# XXX see if p5 parses it

	    print STDERR "TRE: ", $pat,"\n" if $DEBUG;

	    print STDERR "#FATES: ", @$fate - 1, "\n" if $lexverbose;

	    for my $i (1..@$fate-1) {
		print STDERR $i, ': ', $fate->[$i], "\n" if $lexverbose;
	    }

	    # generate match closure at the last moment
	    sub {
		my $C = shift;

		print STDERR "lexing $key\n" if $DEBUG;
		die "orig disappeared!!!" unless length($$buf);

		return '' unless $lexer;

		my $result = "";
		pos($$buf) = $C->{pos};

		##########################################
		# No normal p5 match/subst below here!!! #
		##########################################
		{
		    use re::engine::TRE;

		    print STDERR "/ running tre match at @{[ pos($$buf) ]} /\n" if $DEBUG;

		    if (($$buf =~ m/$pat/xgc)) {	# XXX does this recompile $pat every time?
			my $max = @+ - 1;
			my $last = @- - 1;	# ignore '$0'
#		        print STDERR "LAST: $last\n";
			$result = $fate->[$last] // return '';
			for my $x (1 .. $max) {
			    my $beg = $-[$x];
			    next unless defined $beg;
			    my $end = $+[$x];
			    my $f = $fate->[$x];
			    no strict 'refs';
			    if ($lexverbose or ($DEBUG and $x == $last)) {
				print STDERR "\$$x: $beg..$end\t$$x\t ",
				    $x == $last ? "====>" : "---->",
				    " $f\n";
			    }
			}
			print STDERR "success at '", substr($$buf,$C->{pos},10), "'\n" if $DEBUG;
		    }
		    else {
			print STDERR "NO LEXER MATCH at '", substr($$buf,$C->{pos},10), "'\n" if $DEBUG;
		    }
		    $result;
		}
	    }
	}
    };
}

sub setname { my $self = shift;
    my $k = shift;

    $self->{name} = "$k";
    $self;
}

{ package Match;
    sub new { my $self = shift;
	my %args = @_;
	bless \%args, $self;
    }

    sub from { my $self = shift;
	$self->{_f};
    }

    sub to { my $self = shift;
	$self->{_t};
    }

    sub dump { my $self = shift;
	my $name = shift // 'Match';
	my $text = "$name: $$self{_f}..$$self{_t}\n";
	foreach my $k (sort keys %$self) {
	    next if $k =~ /^_/;
	    my $v = $$self{$k};
	    if (not defined $v) {
		warn "$k undefined in $text";
		$text .= "$k: " . YAML::XS::Dump($self);
	    }
	    elsif (ref $v eq 'HASH') {
		$text .= "$k: " . YAML::XS::Dump($v);
	    }
	    elsif (ref $v) {
		$text .= ::indent($v->dump($k));
	    }
	    else {
		$text .= "$k: $v\n";
	    }
	}
	$text;
    }
}

sub matchify { my $self = shift;
    my $bindings;
    print STDERR "matchify ", ref $self, "\n" if $DEBUG;
    if ($self->{M}) {
	@{$self->{M}}{'_f','_t'} = ($self->{from}, $self->{to});
    }
    else {
	$self->{M} = $bindings = 'Match'->new(_f => $self->{from}, _t => $self->{to} );
    }
    for (my $c = $self->{prior}; $c; $c = $c->{prior}) {
        my $n = $c->{name};
        print STDERR "matchify prior $n\n" if $DEBUG;
        if (not $bindings->{$n}) {
            $bindings->{$n} = [];
        }
	if ($c->{M}) {
	    unshift @{$bindings->{$n}}, $c->{M};
	    last;
	}
    }
    for my $k (keys(%$bindings)) { my $v = $bindings->{$k};
	if (ref $v eq 'ARRAY' and @$v == 1) {
	    $bindings->{$k} = $v->[0];
	}
    }
    delete $self->{prior};
    print STDERR $self->dump if $DEBUG;
    $self;
}

sub dump { my $self = shift;
    my $name = shift // 'Cursor';
    my $text = "$name: $$self{from}..$$self{to}\n";
    if ($self->{M}) {
	$text .= ::indent($self->{M}->dump($$self{name}));
    }
    $text =~ s/ +$//;
    $text;
}

sub cursor_all { my $self = shift;
    my $fpos = shift;
    my $tpos = shift;

    my %r;
    $r{orig} = $$self{orig};
    $r{ws_to} = $$self{ws_to};
    $r{ws_from} = $$self{ws_from};
    $r{M} = $$self{M} if exists $$self{M};
    $r{from} = $fpos;
    $r{to} = $tpos;
    $r{pos} = $tpos;
    $r{prior} = defined $self->{name} ? $self : $self->{prior};
    print STDERR "orig at ", sprintf("%08x\n", unpack 'L', pack('p', ${$self->{orig}})) if $DEBUG;

    bless \%r, ref $self;
}

sub cursor { my $self = shift;
    my $tpos = shift;

    my %r;
    $r{orig} = $$self{orig};
    $r{ws_to} = $$self{ws_to};
    $r{ws_from} = $$self{ws_from};
    $r{M} = $$self{M} if exists $$self{M};
    $r{from} = $self->{pos} // 0;
    $r{to} = $tpos;
    $r{pos} = $tpos;
    $r{prior} = defined $self->{name} ? $self : $self->{prior};
    print STDERR "orig at ", sprintf("%08x\n", unpack 'L', pack('p', ${$self->{orig}})) if $DEBUG;

    bless \%r, ref $self;
}

sub cursor_rev { my $self = shift;
    my $fpos = shift;

    my %r;
    $r{orig} = $$self{orig};
    $r{ws_to} = $$self{ws_to};
    $r{ws_from} = $$self{ws_from};
    $r{M} = $$self{M} if exists $$self{M};
    $r{pos} = $fpos;
    $r{from} = $fpos;
    $r{to} = $self->{from};
    $r{prior} = defined $self->{name} ? $self : $self->{prior};
    print STDERR "orig at ", sprintf("%08x\n", unpack 'L', pack('p', ${$self->{orig}})) if $DEBUG;

    bless \%r, ref $self;
}

local $CTX = { lvl => 0 };

sub callm { my $self = shift;
    my $arg = shift;

    my $lvl = 0;
    my @subs;
    while (my @c = caller($lvl)) { $lvl++; (my $s = $c[3]) =~ s/^.*:://; $s =~ s/^__ANON//; push @subs, $s; }
    splice(@subs, 0, 2);
    my ($package, $file, $line, $subname, $hasargs) = caller(1);
    my $name = $subname;
    if (defined $arg) { 
        $name .= " " . $arg;
    }
    my $pos = '?';
    $pos = $self->{pos} if defined $self->{pos};
    print STDERR $pos,"\t", ':' x $lvl, ' ', $name, " [", $file, ":", $line, "] @subs\n" if $DEBUG;
    {lvl => $lvl};
}

sub whats { my $self = shift;

    my @k = keys(%{$self->{mykeys}});
    print STDERR "  $self ===> @{[$self->{from}.' '. $self->{to}.' '. $self->{item}]} (@k)\n" if $DEBUG;
    for my $k (@k) {
        print STDERR "   $k => @{[$self->{mykeys}->{$k}]}\n" if $DEBUG;
    }
}

sub retm { my $self = shift;
    my $bind = shift;

    warn "Returning non-Cursor: $self\n" unless exists $self->{pos};
    my $binding = "";
    if (defined $bind and $bind ne '') {
        $self->{name} = $bind;
        $binding = "      :bind<$bind>";
    }
    my ($package, $file, $line, $subname, $hasargs) = caller(1);
    print STDERR $self->{pos}, "\t", ':' x $CTX->{lvl}, ' ', $subname, " returning @{[$self->{from}]}..@{[$self->{to}]}$binding\n" if $DEBUG;
#    $self->whats();
    $self;
}

sub _MATCHIFY { my $self = shift;
    my $bind = shift;

    my @result = map { $_->cursor_all($self->{pos}, $_->{to})->retm($bind)->matchify } @_;
    if (wantarray) {
	@result;
    }
    else {
	$result[0];
    }
}

sub _STARf { my $self = shift;
    my $block = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;

    map { $_->retm($bind) }
        $self->cursor($self->{pos}),
        $self->_PLUSf($block);
}

sub _STARg { my $self = shift;
    my $block = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;

    map { $_->retm($bind) } reverse
            $self->cursor($self->{pos}),
            $self->_PLUSf($block);
}

sub _STARr { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm;
    my $to = $self;
    my @all;
    my $eos = length(${$self->{orig}});
    for (;;) {
      last if $to->{pos} == $eos;
	my @matches = $block->($to);  # XXX shouldn't read whole list
#            say @matches.perl;
      last unless @matches;
	my $first = $matches[0];  # no backtracking into block on ratchet
	push @all, $first;
	$to = $first;
    }
#    $self->cursor($to.to).retm($bind);
    $self->cursor($to->{pos});  # XXX $_->retm blows up pugs for unfathomable reasons
}

sub _PLUSf { my $self = shift;
    my $block = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $x = $self;

    my @result;
    # don't go beyond end of string
    return () if $self->{pos} == length(${$self->{orig}});
    do {
	for my $x ($block->($self)) {
	    push @result, map { $self->cursor($_->{to}) } $x, $x->_PLUSf($block);
	}
    };
    map { $_->retm($bind) } @result;
}

sub _PLUSg { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm;

    reverse $self->_PLUSf($block, @_);
}

sub _PLUSr { my $self = shift;
    my $block = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $to = $self;
    my @all;
    my $eos = length(${$self->{orig}});
    for (;;) {
      last if $to->{pos} == $eos;
	my @matches = $block->($to);  # XXX shouldn't read whole list
      last unless @matches;
	my $first = $matches[0];  # no backtracking into block on ratchet
	#print STDERR $matches->perl, "\n" if $DEBUG;
	push @all, $first;
	$to = $first;
    }
    return () unless @all;
    my $r = $self->cursor($to->{pos});
    $r->retm($bind);
}

sub _OPTr { my $self = shift;
    my $block = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;

    my $x = ($block->($self))[0];
    my $r = $x // $self->cursor($self->{pos});
    $r->retm($bind);
}

sub _OPTg { my $self = shift;
    my $block = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my @x = $block->($self);
    map { $_->retm($bind) }
        $block->($self),
        $self->cursor($self->{pos});
}

sub _OPTf { my $self = shift;
    my $block = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    map { $_->retm($bind) }
        $self->cursor($self->{pos}),
        $block->($self);
}

sub _BRACKET { my $self = shift;
    my $block = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    map { $_->retm($bind) }
        $block->($self);
}

sub _PAREN { my $self = shift;
    my $block = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    map { $_->matchify->retm($bind) }
        $block->($self);
}

sub _NOTBEFORE { my $self = shift;
    my $block = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my @all = $block->($self);
    return () if @all;  # XXX loses continuation
    return $self->cursor($self->{pos})->retm($bind);
}

sub _NOTCHAR { my $self = shift;
    my $block = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my @all = $block->($self);
    return () if @all;  # XXX loses continuation
    return $self->cursor($self->{pos}+1)->retm($bind);
}

sub before { my $self = shift;
    my $fate = shift;
    my $block = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my @all = $block->($self);
    if (@all and $all[0]) {
        return $all[0]->cursor_all(($self->{pos}) x 2)->retm($bind);
    }
    return ();
}

sub after { my $self = shift;
    my $fate = shift;
    my $block = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $end = $self->cursor($self->{pos});
    my @all = $block->($end);          # Make sure $_->{from} == $_->{to}
    if (@all and $all[0]) {
        return $all[0]->cursor_all(($self->{pos}) x 2)->retm($bind);
    }
    return ();
}

sub null { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    return $self->cursor($self->{pos})->retm($bind);
}

sub _ASSERT { my $self = shift;
    my $block = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my @all = $block->($self);
    if ((@all and $all[0]->{bool})) {
        return $self->cursor($self->{pos})->retm($bind);
    }
    return ();
}

sub _BINDVAR { my $self = shift;
    my $var = shift;
    my $block = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    map { $$var = $_; $_->retm($bind) }  # XXX doesn't "let"
        $block->($self);
}

sub _BINDPOS { my $self = shift;
    my $bind = shift;
    my $block = shift;

    local $CTX = $self->callm;
    map { $_->retm($bind) }
        $block->($self);
}

sub _BINDNAMED { my $self = shift;
    my $bind = shift;
    my $block = shift;

    local $CTX = $self->callm;
    map { $_->retm($bind) }
        $block->($self);
}

sub _EXACT { my $self = shift;
    my $s = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm($s);
    my $P = $self->{pos} // 0;
    my $len = length($s);
    my $buf = $self->{orig};
    if (substr($$buf, $P, $len) eq $s) {
        print STDERR "EXACT $s matched @{[substr($$buf,$P,$len)]} at $P $len\n" if $DEBUG;
        my $r = $self->cursor($P+$len);
        $r->retm($bind);
    }
    else {
        print STDERR "EXACT $s didn't match @{[substr($$buf,$P,$len)]} at $P $len\n" if $DEBUG;
        return ();
    }
}

sub _EXACT_rev { my $self = shift;
    my $s = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $len = length($s);
    my $from = $self->{from} - $len;
    my $buf = $self->{orig};
    if ($from >= 0 and substr($$buf, $from, $len) eq $s) {
        my $r = $self->cursor_rev($from);
        $r->retm($bind);
    }
    else {
#        say "EXACT_rev $s didn't match @{[substr($!orig,$from,$len)]} at $from $len";
        return ();
    }
}

sub _DIGIT { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $P = $self->{pos};
    my $buf = $self->{orig};
    my $char = substr($$buf, $P, 1);
    if ($char =~ /^\d$/) {
        my $r = $self->cursor($P+1);
        return $r->retm($bind);
    }
    else {
#        say "DIGIT didn't match $char at $P";
        return ();
    }
}

sub _DIGIT_rev { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $from = $self->{from} - 1;
    if ($from < 0) {
#        say "DIGIT_rev didn't match $char at $from";
        return ();
    }
    my $buf = $self->{orig};
    my $char = substr($$buf, $from, 1);
    if ($char =~ /^\d$/) {
        my $r = $self->cursor_rev($from);
        return $r->retm($bind);
    }
    else {
#        say "DIGIT_rev didn't match $char at $from";
        return ();
    }
}

sub _ALNUM { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $P = $self->{pos};
    my $buf = $self->{orig};
    my $char = substr($$buf, $P, 1);
    if ($char =~ /^\w$/) {
        my $r = $self->cursor($P+1);
        return $r->retm($bind);
    }
    else {
#        say "ALNUM didn't match $char at $P";
        return ();
    }
}

sub _ALNUM_rev { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $from = $self->{from} - 1;
    if ($from < 0) {
#        say "ALNUM_rev didn't match $char at $from";
        return ();
    }
    my $buf = $self->{orig};
    my $char = substr($$buf, $from, 1);
    if ($char =~ /^\w$/) {
        my $r = $self->cursor_rev($from);
        return $r->retm($bind);
    }
    else {
#        say "ALNUM_rev didn't match $char at $from";
        return ();
    }
}

sub alpha { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $P = $self->{pos};
    my $buf = $self->{orig};
    my $char = substr($$buf, $P, 1);
    if ($char =~ /^[a-z_A-Z]$/) {
        my $r = $self->cursor($P+1);
        return $r->retm($bind);
    }
    else {
#        say "alpha didn't match $char at $P";
        return ();
    }
}

sub _SPACE { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $P = $self->{pos};
    my $buf = $self->{orig};
    my $char = substr($$buf, $P, 1);
    if ($char =~ /^\s$/) {
        my $r = $self->cursor($P+1);
        return $r->retm($bind);
    }
    else {
#        say "SPACE didn't match $char at $P";
        return ();
    }
}

sub _SPACE_rev { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $from = $self->{from} - 1;
    if ($from < 0) {
#        say "SPACE_rev didn't match $char at $from";
        return ();
    }
    my $buf = $self->{orig};
    my $char = substr($$buf, $from, 1);
    if ($char =~ /^\s$/) {
        my $r = $self->cursor_rev($from);
        return $r->retm($bind);
    }
    else {
#        say "SPACE_rev didn't match $char at $from";
        return ();
    }
}

sub _HSPACE { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $P = $self->{pos};
    my $buf = $self->{orig};
    my $char = substr($$buf, $P, 1);
    if ($char =~ /^[ \t\r]$/ or ($char =~ /^\s$/ and $char !~ /^[\n\f\0x0b\x{2028}\x{2029}]$/)) {
        my $r = $self->cursor($P+1);
        return $r->retm($bind);
    }
    else {
#        say "HSPACE didn't match $char at $P";
        return ();
    }
}

sub _HSPACE_rev { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $from = $self->{from} - 1;
    if ($from < 0) {
#        say "HSPACE_rev didn't match $char at $from";
        return ();
    }
    my $buf = $self->{orig};
    my $char = substr($$buf, $from, 1);
    if ($char =~ /^[ \t\r]$/ or ($char =~ /^\s$/ and $char !~ /^[\n\f\0x0b\x{2028}\x{2029}]$/)) {
        my $r = $self->cursor_rev($from);
        return $r->retm($bind);
    }
    else {
#        say "HSPACE_rev didn't match $char at $from";
        return ();
    }
}

sub _VSPACE { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $P = $self->{pos};
    my $buf = $self->{orig};
    my $char = substr($$buf, $P, 1);
    if ($char =~ /^[\n\f\x0b\x{2028}\x{2029}]$/) {
        my $r = $self->cursor($P+1);
        return $r->retm($bind);
    }
    else {
#        say "VSPACE didn't match $char at $P";
        return ();
    }
}

sub _VSPACE_rev { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $from = $self->{from} - 1;
    if ($from < 0) {
#        say "VSPACE_rev didn't match $char at $from";
        return ();
    }
    my $buf = $self->{orig};
    my $char = substr($$buf, $from, 1);
    if ($char =~ /^[\n\f\x0b\x{2028}\x{2029}]$/) {
        my $r = $self->cursor_rev($from);
        return $r->retm($bind);
    }
    else {
#        say "VSPACE_rev didn't match $char at $from";
        return ();
    }
}

sub _CCLASS { my $self = shift;
    my $cc = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $P = $self->{pos};
    my $buf = $self->{orig};
    my $char = substr($$buf, $P, 1);
    if ($char =~ /$cc/) {
        my $r = $self->cursor($P+1);
        return $r->retm($bind);
    }
    else {
#        say "CCLASS didn't match $char at $P";
        return ();
    }
}

sub _CCLASS_rev { my $self = shift;
    my $cc = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $from = $self->{from} - 1;
    if ($from < 0) {
#        say "CCLASS didn't match $char at $from";
        return ();
    }
    my $buf = $self->{orig};
    my $char = substr($$buf, $from, 1);
    if ($char =~ /$cc/) {
        my $r = $self->cursor_rev($from);
        return $r->retm($bind);
    }
    else {
#        say "CCLASS didn't match $char at $from";
        return ();
    }
}

sub _ANY { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $P = $self->{pos};
    my $buf = $self->{orig};
    if ($P < length($$buf)) {
        $self->cursor($P+1)->retm($bind);
    }
    else {
#        say "ANY didn't match anything at $P";
        return ();
    }
}

sub _BOS { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $P = $self->{pos};
    if ($P == 0) {
        $self->cursor($P)->retm($bind);
    }
    else {
        return ();
    }
}

sub _BOL { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $P = $self->{pos};
    my $buf = $self->{orig};
    if ($P == 0 or substr($$buf, $P-1, 1) =~ /^[\n\f\x0b\x{2028}\x{2029}]$/) {
        $self->cursor($P)->retm($bind);
    }
    else {
        return ();
    }
}

sub _EOS { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $P = $self->{pos};
    my $buf = $self->{orig};
    if ($P == length($$buf)) {
        $self->cursor($P)->retm($bind);
    }
    else {
        return ();
    }
}

sub _EOL { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $P = $self->{pos};
    my $buf = $self->{orig};
    if ($P == length($$buf) or substr($$buf, $P, 1) =~ /^(?:\r\n|[\n\f\x0b\x{2028}\x{2029}])$/) {
        $self->cursor($P)->retm($bind);
    }
    else {
        return ();
    }
}

sub _RIGHTWB { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $P = $self->{pos};
    my $buf = $self->{orig};
    pos($$buf) = $P - 1;
    if ($$buf =~ /\w\b/) {
        $self->cursor($P)->retm($bind);
    }
    else {
        return ();
    }
}

sub _LEFTWB { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $P = $self->{pos};
    my $buf = $self->{orig};
    pos($$buf) = $P;
    if ($$buf =~ /\b(?=\w)/) {
        $self->cursor($P)->retm($bind);
    }
    else {
        return ();
    }
}

sub _REDUCE { my $self = shift;
    my $tag = shift;

    local $CTX = $self->callm($tag);
    my $P = $self->{pos};
    my $F = $self->{from};
    $self->{R} = $tag;
    print STDERR "Success $tag from $F to $P\n" if $DEBUG;
#    $self->whats;
    $self;
#    $self->cursor($P);
}

sub _COMMITBRANCH { my $self = shift;

    local $CTX = $self->callm;
    my $P = $self->{pos};
    print STDERR "Commit branch to $P\n" if $DEBUG;
#    $self->cursor($P);  # XXX currently noop
    $self;
}

sub _COMMITRULE { my $self = shift;

    local $CTX = $self->callm;
    my $P = $self->{pos};
    print STDERR "Commit rule to $P\n" if $DEBUG;
#    $self->cursor($P);  # XXX currently noop
    $self;
}

sub commit { my $self = shift;

    local $CTX = $self->callm;
    my $P = $self->{pos};
    print STDERR "Commit match to $P\n" if $DEBUG;
#    $self->cursor($P);  # XXX currently noop
    $self;
}

sub fail { my $self = shift;
    my $m = shift;
    die $m;
}

#############################################################3

{ package main;
    sub indent { my $s = shift;
	$s =~ s/\n/\n  /g;
	"  " . $s;
    }

    sub qm { my $s = shift;
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
	return unless $RE_verbose;
	my $arg = shift;
	my $lvl = 0;
	while (caller($lvl)) { $lvl++ }
        my ($package, $file, $line, $subname, $hasargs) = caller(0);

	my $name = $package;   # . '::' . substr($subname,1);
	if (defined $arg) { 
	    $name .= " " . $arg;
	}
	print STDERR ':' x $lvl, ' ', $name, " [", $file, ":", $line, "]\n";
    }
}

{ package REbase;
    sub longest { my $self = shift; my ($C) = @_;  ::here("UNIMPL @{[ref $self]}"); "$self" }
}

{ package RE; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        ::here();
        local $PURE = 1;
	local $ALT = '';
        $self->{'re'}->longest($C);
    }
}

{ package RE_adverb; our @ISA = 'RE_base';
    #method longest ($C) { ... }
}

{ package RE_assertion; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        for (scalar($self->{'assert'})) { if ((0)) {}
            elsif ($_ eq '?') {
                my $re = $self->{'re'};
		print STDERR ::Dump($self) unless $re;
                if (ref($re) eq 'RE_method_re' and $re->{'name'} eq 'before') {
                    my @result = $re->longest($C);
                    $PURE = 0;
                    return @result;
                }
            }
        }
        return;
    }
}

{ package RE_assertvar; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        $PURE = 0;
        return;
    }
}

{ package RE_block; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        $PURE = 0;
        return;
    }
}

{ package RE_bindvar; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; ::here();
	$self->{'atom'}->longest($C);
    }
}

{ package RE_bindnamed; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; ::here();
	$self->{'atom'}->longest($C);
    }
}

{ package RE_bindpos; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; ::here();
	$self->{'atom'}->longest($C);
    }
}

{ package RE_bracket; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; ::here();
	$self->{'re'}->longest($C);
    }
}

{ package RE_cclass; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; ::here($self->{'text'});
        $fakepos++;
        my $cc = $self->{'text'};
	Encode::_utf8_on($cc);
        $cc =~ s/^\-\[/[^/;
        $cc =~ s/^\+\[/[/;
        $cc =~ s/\s*\.\.\s*/-/g;
        $cc =~ s/\s*//g;
        $cc;
    }
}

{ package RE_decl; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_;  return; }
}

{ package RE_double; our @ISA = 'RE_base';
    # XXX inadequate for "\n" without interpolation
    sub longest { my $self = shift; my ($C) = @_; 
        my $text = $self->{'text'};
	Encode::_utf8_on($text);
        ::here($text);
	my $fixed = '';
	if ( $text =~ /^(.*?)[\$\@\%\&\{]/ ) {
	    $PURE = 0;
	    $fixed = $1;
	}
	else {
	    $fixed = $text;
	}
	if ($fixed ne '') {
	    $fakepos++;
	    ::qm($fixed);
	}
        $fixed;
    }
}

{ package RE_meta; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        my $text = $self->{'text'};
	Encode::_utf8_on($text);
        ::here($text);
        for (scalar($text)) { if ((0)) {}
            elsif ($_ eq '^' or
		   $_ eq '$' or
		   $_ eq '.' or
		   $_ eq '\\w' or
		   $_ eq '\\s' or
		   $_ eq '\\d')
	    {
                return $text;
            }
            elsif ($_ eq '\\h') {
                return '[\\x20\\t\\r]';
	    }
            elsif ($_ eq '\\h') {
                return '[\\n\\f]';
            }
            elsif ($_ eq ':' or $_ eq '^^') {
		return;
	    }
            elsif ($_ eq '»' or $_ eq '>>') {
		return '\>';
	    }
            elsif ($_ eq '«' or $_ eq '<<') {
		return '\<';
	    }
            elsif ($_ eq '::' or $_ eq ':::') {
                $PURE = 0;
                return;
            }
            else {
                return $text;
            }
        }
    }
}

{ package RE_method_noarg; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        my $name = $self->{'name'};
	Encode::_utf8_on($name);
        ::here($name);
        for (scalar($name)) { if ((0)) {}
            elsif ($_ eq 'null') {
                return;
            }
            elsif ($_ eq '') {
                $PURE = 0;
                return;
            }
            elsif ($_ eq 'ws') {
                $PURE = 0;
                return;
            }
            elsif ($_ eq 'unsp') {
                $PURE = 0;
                return;
            }
            elsif ($_ eq 'nofat_space') {
                $PURE = 0;
                return;
            }
            elsif ($_ eq 'sym') {
                $fakepos++;
		my $sym = $self->{'sym'};
		Encode::_utf8_on($sym);
                return ::qm($sym);
            }
            elsif ($_ eq 'alpha') {
                $fakepos++;
                return '[a-z_A-Z]';	# XXX not unicodey
            }
	    elsif ($_ eq 'EXPR') {
		if (not -e 'lex/EXPR') {
		    $PURE = 0;
		    return;
		}
	    }
	    my $lexer;
	    {
		local $PREFIX = "";
		$lexer = $C->$name(['?', 'peek']);
	    }
	    my @pat = @{$lexer->{PATS}};
	    return unless @pat;
	    if ($PREFIX) {
		for (@pat) {
		    s/(\t\(\?#FATE) *(.*?\))(.*)/$3$1$PREFIX $2/g;
		}
	    }
	    return @pat;
        }
    }
}

{ package RE_method_internal; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        $PURE = 0;
        return;
    }
}

{ package RE_method_re; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        my $name = $self->{'name'};
	Encode::_utf8_on($name);
        ::here($name);
        my $re = $self->{'re'};
        for (scalar($name)) { if ((0)) {}
            elsif ($_ eq '') {
                $PURE = 0;
                return;
            }
            elsif ($_ eq 'after') {
                return;
            }
            elsif ($_ eq 'before') {
                my @result = $re->longest($C);
                $PURE = 0;
                return @result;
            }
            else {
                my $lexer = $C->$name(['?', 'peek'], $re);
		my @pat = @{$lexer->{PATS}};
		return unless @pat;
		return @pat;
            }
        }
    }
}

{ package RE_method_str; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        my $name = $self->{'name'};
	Encode::_utf8_on($name);
        ::here($name);
        my $str = $self->{'str'};
        for (scalar($name)) { if ((0)) {}
            elsif ($_ eq 'lex1') {
                return;
            }
            elsif ($_ eq 'panic' | 'obs') {
                $PURE = 0;
                return;
            }
            else {
                my $lexer = $C->$name(['?', 'peek'], $str);
		my @pat = @{$lexer->{PATS}};
		return '' unless @pat;
		return @pat;
            }
        }
    }
}

{ package RE_method; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        $PURE = 0;
        return;
    }
}

{ package RE_noop; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        return;
    }
}

{ package RE_every; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        $PURE = 0;
        return;
    }
}

{ package RE_first; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        my $alts = $self->{'zyg'};
        ::here(0+@$alts);
        my @result;
        for my $alt (@$alts) {
            my @pat = $alt->longest($C);
            push @result, @pat;
            last;
        }
        print STDERR join("\n",@result), "\n" if $RE_verbose;
        @result;
    }
}

{ package RE_paren; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; ::here();
    ("(" . join('|', $self->{'re'}->longest($C)) . ")") }	# XXX bad if fates
}

{ package RE_quantified_atom; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        ::here();
        my $oldfakepos = $fakepos++;
	my $a = $self->{atom};
        my @atom = $a->longest($C);
	return unless @atom;
	my $atom = join('|',@atom);
	return if $atom eq '';
	$atom = "(?:" . $atom . ')' unless $a->{min} == 1 and ref($a) =~ /^RE_(?:meta|cclass|string)/;
        if ($self->{'quant'}[0] eq '+') {
	    if (@atom > 1) {
		$PURE = 0;
		return @atom;
	    }
            return "$atom+";
        }
        elsif ($self->{'quant'}[0] eq '*') {
            $fakepos = $oldfakepos;
	    if (@atom > 1) {
		$PURE = 0;
		return @atom,'';
	    }
            return "$atom*";
        }
        elsif ($self->{'quant'}[0] eq '?') {
            $fakepos = $oldfakepos;
	    if (@atom > 1) {
		return @atom,'';
	    }
            return "$atom?";
        }
        elsif ($self->{'quant'}[0] eq '**') {
            my $x = $self->{'quant'}[2];
	    if ($x =~ /^\d/) {
		$x =~ s/\.\./,/;
		$x =~ s/\*//;
		$fakepos = $oldfakepos if $x =~ m/^0/;
		return $atom . "{$x}";
	    }
	    else {
		$PURE = 0;
		return $atom;
	    }
        }
	$PURE = 0;
	return;
    }
}

{ package RE_qw; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        my $text = $self->{'text'};
	Encode::_utf8_on($text);
        ::here($text);
        $fakepos++;
        $text =~ s/^<\s*//;
        $text =~ s/\s*>$//;
        $text =~ s/\s+/|/;
        '(?: ' . $text . ')';
    }
}

{ package RE_sequence; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        local $PURE = 1;
        my $c = $self->{'zyg'};
        my @chunks = @$c;
        ::here(0+@chunks);
        my $result = [''];
	local $PREFIX = $PREFIX;

        for my $chunk (@chunks) {
	    # ignore negative lookahead
	    next if ref($chunk) eq 'RE_assertion' and $chunk->{assert} eq '!';
	    warn "NULLABLE ".ref($chunk)."\n" unless $chunk->{min};
            my @newalts = $chunk->longest($C);
            if (not @newalts) {
		next if $PURE;
		last;
	    }
#	    if (not $chunk->{min} and $next[-1] ne '') {
#		push(@next, '');	# install bypass around nullable atom
#	    }
	    my $newresult = [];
	    for my $oldalt (@$result) {
		for my $newalt (@newalts) {
		    $PREFIX = '' if $newalt =~ /FATE/;;
		    if ($oldalt =~ /FATE/ and $newalt =~ /FATE/) {
			my $newold = $oldalt;
			my $newnew = $newalt;
			$newnew =~ s/\t\(\?#FATE *(.*?)\)//;
			my $morefate = $1;
			$newold =~ s/(FATE.*?)\)/$1 $morefate)/;
			push(@$newresult, $newold . $newnew);
		    }
		    else {
			push(@$newresult, $oldalt . $newalt);
		    }
		}
	    }
	    $result = $newresult;
	    # ignore everything after positive lookahead
	    last if ref($chunk) eq 'RE_assertion';
	    next if $PURE;
        }
	@$result;
    }
}

{ package RE_string; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        my $text = $self->{'text'};
	Encode::_utf8_on($text);
        ::here($text);
        $fakepos++ if $self->{'min'};
        $text = ::qm($text);
	$text;
    }
}

{ package RE_submatch; our @ISA = 'RE_base';
    #method longest ($C) { ... }
}

{ package RE_all; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        $PURE = 0;
        return;
    }
}

{ package RE_any; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        my $alts = $self->{'zyg'};
        ::here(0+@$alts);
        my @result;
        my $oldfakepos = $fakepos;
        my $minfakepos = $fakepos + 1;
	my $base = $ALT // '';
	$base .= ' ' if $base;
        for my $alt (@$alts) {
            $fakepos = $oldfakepos;
	    local $ALT = $base . $alt->{alt};
	    {
		local $PREFIX = $PREFIX . ' ' . $ALT;
		my @pat = ($alt->longest($C));
		push @result, map { /#FATE/ or s/$/\t(?#FATE $PREFIX)/; $_ } @pat;
	    }
            $minfakepos = $oldfakepos if $fakepos == $oldfakepos;
        }
        print STDERR join("\n", @result), "\n" if $RE_verbose;
        $fakepos = $minfakepos;  # Did all branches advance?
        @result;
    }
}

{ package RE_var; our @ISA = 'RE_base';
    #method longest ($C) { ... }
    sub longest { my $self = shift; my ($C) = @_; 
        $PURE = 0;
        return;
    }
}


