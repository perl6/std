our $CTX;
$CTX->{lvl} = 0;
package Cursor5;

use strict;
use warnings;
use Encode;

our %AUTOLEXED;
our $FATES;
our $PURE;
our $ALT;

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
    print " orig ", $$buf,"\n";
    $self->BUILD;
    $self;
}

use YAML::Syck;

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

sub lexers { my $self = shift;
    %lexers;    # XXX should be different per language, sigh
}

my $fakepos = 1;

sub _AUTOLEXpeek { my $self = shift;
    my $key = shift;

    print "?" x 72, "\n";
    print "AUTOLEXpeek $key\n";
    die "Null key" if $key eq '';
    if ($self->{lexers}->{$key}) {
        if ($AUTOLEXED{$key}) {   # no left recursion allowed in lexer!
            die "Left recursion in $key" if $fakepos == $AUTOLEXED{$key};
            warn "Suppressing lexer recursion on $key";
            return hash();  # (but if we advanced just assume a :: here)
        }
        elsif (ref($self->{lexers}->{$key}) eq 'Hash') {
            return $self->{lexers}->{$key} // hash();
        }
        else {
            print "oops ", ref($key), "\n";
        }
    }
    return $self->{lexers}{$key} = $self->_AUTOLEXgen($key);
}

sub _AUTOLEXgen { my $self = shift;
    my $key = shift;

    print "=" x 72, "\n";
    print "AUTOLEXgen $key\n";
    my $lexer = {};
    (my $file = $key) =~ s/::/--/g;
    if (-s "lex/$file.yml") {
	print "using cached lex/$file.yml\n";
    }
    else {
	my $ast = LoadFile("yamlg5/$file.yml");
	Encode::_utf8_on($ast);
	my $oldfakepos = $AUTOLEXED{$key} // 0;
	local $FATES;
	$FATES = [];

	$AUTOLEXED{$key} = $fakepos;
	my $pat = $ast->longest($self,0);
	print "(null pattern)\n" unless $pat;
#	if (Encode::is_utf8($pat)) { print "UTF8 ON\n" } else { print "UTF8 OFF\n" }
	if ($pat =~ /Â/) { print "bad Â\n" }

	$AUTOLEXED{$key} = $oldfakepos;

	$lexer = { PAT => $pat, FATES => $FATES };
	mkdir("lex") unless -d "lex";
	open(my $cache, '>', "lex/$file.yml") // warn "Can't print: $!";
	print $cache Dump($lexer) or warn "Can't print: $!";
	close($cache) or warn "Can't close: $!";
	print "regenerated lex/$file.yml\n";
    }
    $lexer;
}

sub _AUTOLEXnow { my $self = shift;
    my $key = shift;

    print "!" x 72, "\n";
    print "AUTOLEXnow $key\n";
    my $lexer = $self->{lexers}->{$key} // do {
	local %AUTOLEXED;
	$self->_AUTOLEXpeek($key);
    };

    $lexer->{MATCH} //= do {
	print "generating lexer closure for $key:\n";

	my $buf = $self->{orig};	# XXX this might lose pos()...
#	print "AT: ", substr($$buf,0,20), "\n";

	# generate match closure at the last moment
	sub {
	    my $C = shift;

	    print '=' x 72, "\n";
	    print "lexing $key\n";
	    die "orig disappeared!!!" unless length($$buf);

	    my $stuff;
	    (my $file = $key) =~ s/::/--/g;
	    if ((-e "lex/$file.yml")) {
		$stuff = LoadFile("lex/$file.yml");
	    }
	    else {
		$stuff = {"PAT" => $lexer->{PAT}, "FATES" => $lexer->{FATES}};
	    }

	    if ($stuff->{PAT} eq '') {
		return '';
	    }

	    my $pat = '^' . decode('utf8', $stuff->{PAT});
	    Encode::_utf8_off($pat);
	    $pat = decode('utf8', $pat);
#	    if (Encode::is_utf8($pat)) { print "UTF8 ON\n" } else { print "UTF8 OFF\n" }
	    
	    {
		my $i = 1;
		$pat =~ s/\((?!\?[#:])/( (?#@{[$i++]})/g;
	    }
	    print "PAT: ", $pat,"\n";
	    $pat =~ s/\(\?#.*?\)//g;
	    $pat =~ s/^[ \t]*\n//gm;

	    # remove whitespace that will confuse TRE greatly
	    $pat =~ s/\s+//g;

	    1 while $pat =~ s/\(((\?:)?)\)/($1 !!!OOPS!!! )/;
	    1 while $pat =~ s/\[\]/[ !!!OOPS!!! ]/;
#	    my $tmp = $pat;
#	    "42" =~ /$tmp/ and print "PARSES !\n";

#	    print "PAT: ", $pat,"\n";

	    my $fate = $stuff->{FATES};
	    print "#FATES: ", @$fate + 0, "\n";

	    unshift @$fate, "";	# start at $1
	    for my $i (1..@$fate-1) {
		print $i, ': ', $fate->[$i], "\n";
	    }
	    my $result = "";
	    pos($$buf) = $C->{pos};

	    ##########################################
	    # No normal p5 match/subst below here!!! #
	    ##########################################
	    {
		use re::engine::TRE;

		print "/ running tre match at @{[ pos($$buf) ]} /\n";

		if (($$buf =~ m/$pat/xgc)) {	# XXX does this recompile $pat every time?
		    my $max = @+ - 1;
		    my $last = @- - 1;	# ignore '$0'
#		    print "LAST: $last\n";
		    $result = $fate->[$last] // "OOPS";
		    for my $x (1 .. $max) {
			my $beg = $-[$x];
			next unless defined $beg;
			my $end = $+[$x];
			my $f = $fate->[$x];
			no strict 'refs';
			print "\$$x: $beg..$end\t$$x\t ",
			    $x == $last ? "====>" : "---->",
			    " $f\n";
		    }
		    print "success at '", substr($$buf,$C->{pos},10), "'\n";
		}
		else {
		    print "NO LEXER MATCH at '", substr($$buf,$C->{pos},10), "'\n";
		}
		$result;
	    }
	}
    };
}

sub setname { my $self = shift;
    my $k = shift;

    $self->{name} = "$k";
    $self;
}

sub matchify { my $self = shift;

    my %bindings;
    my $m;
    my $next;
    print "MATCHIFY ", ref $self, "\n";
    for ($m = $self->{prior}; $m; $m = $next) {
        $next = $m->{prior};
        undef $m->{prior};
        my $n = $m->{name};
        print "MATCHIFY $n\n";
        if (not $bindings{$n}) {
            $bindings{$n} = [];
#                %.mykeys = Hash.new unless defined %!mykeys;
            $self->{mykeys}{$n}++;
        }
        unshift @{$bindings{$n}}, $m;
    }
    for my $k (keys(%bindings)) { my $v = $bindings{$k};
	if (ref $v eq 'ARRAY' and @$v == 1) {
	    $self->{$k} = $v->[0];
	}
	else {
	    $self->{$k} = $v;
	}
        # XXX alas, gets lost in the next cursor...
        print "copied $k ", $v, "\n";
    }
    print Dump($self);
    undef $self->{prior};
    $self;
}

sub cursor_all { my $self = shift;
    my $fpos = shift;
    my $tpos = shift;

    my %r = %$self;
    $r{from} = $fpos;
    $r{to} = $tpos;
    $r{pos} = $tpos;
    $r{prior} = defined $self->{name} ? $self : $self->{prior};
    print "orig at ", sprintf("%08x\n", unpack 'L', pack('p', ${$self->{orig}}));

    bless \%r, ref $self;
}

sub cursor { my $self = shift;
    my $tpos = shift;

    my %r = %$self;
    $r{from} = $self->{pos} // 0;
    $r{to} = $tpos;
    $r{pos} = $tpos;
    $r{prior} = defined $self->{name} ? $self : $self->{prior};
    print "orig at ", sprintf("%08x\n", unpack 'L', pack('p', ${$self->{orig}}));

    bless \%r, ref $self;
}

sub cursor_rev { my $self = shift;
    my $fpos = shift;

    my %r = %$self;
    $r{pos} = $fpos;
    $r{from} = $fpos;
    $r{to} = $self->{from};
    $r{prior} = defined $self->{name} ? $self : $self->{prior};
    print "orig at ", sprintf("%08x\n", unpack 'L', pack('p', ${$self->{orig}}));

    bless \%r, ref $self;
}

local $CTX = { lvl => 0 };

sub callm { my $self = shift;
    my $arg = shift;

    my $lvl = 0;
    while (caller($lvl)) { $lvl++ }
    my ($package, $file, $line, $subname, $hasargs) = caller(1);
    my $name = $subname;
    if (defined $arg) { 
        $name .= " " . $arg;
    }
    my $pos = '?';
    $pos = $self->{pos} if defined $self->{pos};
    print $pos,"\t", ':' x $lvl, ' ', $name, " [", $file, ":", $line, "]\n";
    {lvl => $lvl};
}

sub whats { my $self = shift;

    my @k = keys(%{$self->{mykeys}});
    print "  $self ===> @{[$self->{from}.' '. $self->{to}.' '. $self->{item}]} (@k)\n";
    for my $k (@k) {
        print "   $k => @{[$self->{mykeys}->{$k}]}\n";
    }
}

sub retm { my $self = shift;
    my $bind = shift;

    print "Returning non-Cursor: $self\n" unless exists $self->{pos};
    my $binding = "";
    if (defined $bind and $bind ne '') {
        $self->{name} = $bind;
        $binding = "      :bind<$bind>";
    }
    my ($package, $file, $line, $subname, $hasargs) = caller(1);
    print $self->{pos}, "\t", ':' x $CTX->{lvl}, ' ', $subname, " returning @{[$self->{from}]}..@{[$self->{to}]}$binding\n";
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
    for (;;) {
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
    do {
	for my $x ($block->($self)) {
	    push @result, map { $self->cursor($_->{to}) } $x, $self->_PLUSf($x, $block);
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
    for (;;) {
	my @matches = $block->($to);  # XXX shouldn't read whole list
    last unless @matches;
	my $first = $matches[0];  # no backtracking into block on ratchet
	#print $matches->perl, "\n";
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

sub before { my $self = shift;
    my $fate = shift;
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

sub after { my $self = shift;
    my $fate = shift;
    my $block = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $end = $self->cursor($self->{pos});
    warn ref($block);
    my @all = $block->($end);          # Make sure $_->{from} == $_->{to}
    if ((@all and $all[0]->{bool})) {
        return $end->retm($bind);
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
    my $block = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    map { $_->retm($bind) }
        $block->($self);
}

sub _BINDNAMED { my $self = shift;
    my $block = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

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
        print "EXACT $s matched @{[substr($$buf,$P,$len)]} at $P $len\n";
        my $r = $self->cursor($P+$len);
        $r->retm($bind);
    }
    else {
        print "EXACT $s didn't match @{[substr($$buf,$P,$len)]} at $P $len\n";
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
    print "Success $tag from $F to $P\n";
#    $self->whats;
    $self;
#    $self->cursor($P);
}

sub _COMMITBRANCH { my $self = shift;

    local $CTX = $self->callm;
    my $P = $self->{pos};
    print "Commit branch to $P\n";
#    $self->cursor($P);  # XXX currently noop
    $self;
}

sub _COMMITRULE { my $self = shift;

    local $CTX = $self->callm;
    my $P = $self->{pos};
    print "Commit rule to $P\n";
#    $self->cursor($P);  # XXX currently noop
    $self;
}

sub commit { my $self = shift;

    local $CTX = $self->callm;
    my $P = $self->{pos};
    print "Commit match to $P\n";
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

    sub qm { my $s = Encode::decode('utf8', shift);
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

    sub here { my $arg = shift;
	my $lvl = 0;
	while (caller($lvl)) { $lvl++ }
        my ($package, $file, $line, $subname, $hasargs) = caller(0);

	my $name = $package;   # . '::' . substr($subname,1);
	if (defined $arg) { 
	    $name .= " " . $arg;
	}
	print ':' x $lvl, ' ', $name, " [", $file, ":", $line, "]\n" if $RE_verbose;
    }
}

{ package REbase;
    sub longest { my $self = shift; my ($C,$q) = @_;  ::here("UNIMPL @{[ref $self]}"); "$self" }
}

{ package RE; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; 
        ::here();
        local $PURE = 1;
	local $ALT = '';
        $self->{'re'}->longest($C,$q);
    }
}

{ package RE_adverb; our @ISA = 'RE_base';
    #method longest ($C) { ... }
}

{ package RE_assertion; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; 
        for (scalar($self->{'assert'})) { if ((0)) {}
            elsif ($_ eq '?') {
                my $re = $self->{'re'};
                if ($re->{'name'} eq 'before') {
                    my $result = $re->longest($C,$q);
                    $PURE = 0;
                    return $result;
                }
            }
        }
        '[]';
    }
}

{ package RE_assertvar; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; 
        $PURE = 0;
        '';
    }
}

{ package RE_block; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; 
        $PURE = 0;
        '';
    }
}

{ package RE_bindvar; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; ::here();
    $self->{'atom'}->longest($C,$q) }
}

{ package RE_bindnamed; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; ::here();
    $self->{'atom'}->longest($C,$q) }
}

{ package RE_bindpos; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; ::here();
    $self->{'atom'}->longest($C,$q) }
}

{ package RE_bracket; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; ::here();
    ::indent("\n(?:\n" . ::indent($self->{'re'}->longest($C,0)) . "\n)") }
}

{ package RE_cclass; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; ::here($self->{'text'});
        $fakepos++;
        my $cc = $self->{'text'};
        $cc =~ s/^\-\[/[^/;
        $cc =~ s/^\+\[/[/;
        $cc =~ s/\s*\.\.\s*/-/g;
        $cc =~ s/\s*//g;
        $cc;
    }
}

{ package RE_decl; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_;  '[]' }
}

{ package RE_double; our @ISA = 'RE_base';
    # XXX inadequate for "\n" without interpolation
    sub longest { my $self = shift; my ($C,$q) = @_; 
        my $text = $self->{'text'};
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
	    $fixed = "(?: $fixed )" if $q;
	}
        $fixed;
    }
}

{ package RE_meta; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; 
        my $text = $self->{'text'};
        ::here($text);
        for (scalar($text)) { if ((0)) {}
            elsif ($_ eq '^' | '$' | '.' | '\\w' | '\\s' | '\\d') {
                return $text;
            }
            elsif ($_ eq '\\h') {
                return '[\\x20\\t]';
            }
            elsif ($_ eq '::') {
                $PURE = 0;
                return '';
            }
            else {
                return $text;
            }
        }
    }
}

{ package RE_method_noarg; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; 
        my $name = $self->{'name'};
        ::here($name);
        for (scalar($name)) { if ((0)) {}
            elsif ($_ eq 'null') {
                return '';
            }
            elsif ($_ eq '') {
                $PURE = 0;
                return '';
            }
            elsif ($_ eq 'ws') {
                $PURE = 0;
                return '';
            }
            elsif ($_ eq 'EXPR') {
                $PURE = 0;
                return '';
            }
            elsif ($_ eq 'sym') {
                $fakepos++;
                return ::qm($self->{'sym'});
            }
            elsif ($_ eq 'alpha') {
                $fakepos++;
                return '[a-z_A-Z]';	# XXX not unicodey
            }
            else {
		# XXX should be ."$name"
		my $prefix;
		if (@$FATES) {
		    $prefix = @$FATES[-1] . " ";
		}
		my $flen = 0+@$FATES;
                my $lexer = $C->$name(['?']);
		warn "FATES CHANGED" unless $flen == 0+@$FATES;
		for my $fate (@{$lexer->{FATES}}) {
		    push @$FATES, "$prefix$fate";
		}
                return $lexer->{PAT};
            }
        }
    }
}

{ package RE_method_internal; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; 
        $PURE = 0;
        '';
    }
}

{ package RE_method_re; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; 
        my $name = $self->{'name'};
        ::here($name);
        my $re = $self->{'re'};
        for (scalar($name)) { if ((0)) {}
            elsif ($_ eq '') {
                $PURE = 0;
                return '';
            }
            elsif ($_ eq 'after') {
                return '[]';
            }
            elsif ($_ eq 'before') {
                my $result = $re->longest($C,$q);
                $PURE = 0;
                return $result;
            }
            else {
                my $lexer = $C->$name(['?'], $re);
                return $lexer->{PAT};
            }
        }
    }
}

{ package RE_method_str; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; 
        my $name = $self->{'name'};
        ::here($name);
        my $str = $self->{'str'};
        for (scalar($name)) { if ((0)) {}
            elsif ($_ eq 'lex1') {
                return '[]';
            }
            elsif ($_ eq 'panic' | 'obs') {
                $PURE = 0;
                return '';
            }
            else {
                my $lexer = $C->$name(['?'], $str);
                return $lexer->{PAT};
            }
        }
    }
}

{ package RE_method; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; 
        $PURE = 0;
        '';
    }
}

{ package RE_noop; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; 
        '[]';
    }
}

{ package RE_ordered_conjunction; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; 
        $PURE = 0;
        '';
    }
}

{ package RE_ordered_disjunction; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; 
        my $alts = $self->{'zyg'};
        ::here(0+@$alts);
        my @result;
        for my $alt (@$alts) {
            my $pat = $alt->longest($C,$q);
            push @result, $pat;
            last;
        }
        my $result = $result[0];
        print $result, "\n";
        $result;
    }
}

{ package RE_paren; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; ::here();
    unshift @$FATES, "";
    ::indent("\n(\n" . ::indent($self->{'re'}->longest($C,0)) . "\n)") }
}

{ package RE_quantified_atom; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; 
        ::here();
	warn "Quantified atom in quantified context" if $q;
        my $oldfakepos = $fakepos++;
        my $atom = $self->{'atom'}->longest($C,1);
	return '' if $atom eq '';
        if ($self->{'quant'}[0] eq '+') {
            return "$atom+";
        }
        elsif ($self->{'quant'}[0] eq '*') {
            $fakepos = $oldfakepos;
            return "$atom*";
        }
        elsif ($self->{'quant'}[0] eq '?') {
            $fakepos = $oldfakepos;
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
        }
	$PURE = 0;
	return '';
    }
}

{ package RE_qw; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; 
        my $text = $self->{'text'};
        ::here($text);
        $fakepos++;
        $text =~ s/^<\s*//;
        $text =~ s/\s*>$//;
        $text =~ s/\s+/|/;
        '(?: ' . $text . ')';
    }
}

{ package RE_sequence; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; 
        local $PURE = 1;
        my $c = $self->{'zyg'};
        my @chunks = @$c;
        ::here(0+@chunks);
        my @result;
        my $last;
#        while @chunks and
#            $last = @chunks[-1] and  # XXX sb *-1 someday
#            not $last.<min>
#        {
#                pop @chunks;
#        }
        for (@chunks) {
            my $next = $_->longest($C,0) // '';
            $next = '' if $next eq '[]';
            last if $next eq '';
            push @result, $next;
            last unless $PURE or $_->{min} == 0;
        }
	if (@result) {
	    my $result = "@result";
	    $result = "(?: $result )" if $q;
	    $result;
	}
	else {
	    "";
	}
    }
}

{ package RE_string; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; 
        my $text = $self->{'text'};
        ::here($text);
        $fakepos++ if $self->{'min'};
        $text = ::qm($text);
	$text = "(?: $text )" if $q;
	$text;
    }
}

{ package RE_submatch; our @ISA = 'RE_base';
    #method longest ($C) { ... }
}

{ package RE_unordered_conjunction; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; 
        $PURE = 0;
        '';
    }
}

{ package RE_unordered_disjunction; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C,$q) = @_; 
        my $alts = $self->{'zyg'};
        ::here(0+@$alts);
        my @result;
        my $oldfakepos = $fakepos;
        my $minfakepos = $fakepos + 1;
	my $base = $ALT;
	$base .= ' ' if $base;
        for my $alt (@$alts) {
            $fakepos = $oldfakepos;
	    local $ALT = $base . $alt->{alt};
	    push @$FATES, $ALT;
            my $pat = ::indent($alt->longest($C,0));
	    $pat = "OOPS" if $pat =~ m/^\s*$/;
	    if ($pat =~ /\n/) {
		$pat = "(\t\t(?#START $ALT)\n$pat\n)\t\t(?#END $ALT)";
	    }
	    else {
		$pat = "( (?# $ALT)\t$pat )";
	    }
            push @result, $pat;
            $minfakepos = $oldfakepos if $fakepos == $oldfakepos;
        }
        my $result = "\n(?:\n  " . ::indent(join "\n| ", @result) . "\n)";
        print $result, "\n";
        $fakepos = $minfakepos;  # Did all branches advance?
        $result;
    }
}

{ package RE_var; our @ISA = 'RE_base';
    #method longest ($C) { ... }
    sub longest { my $self = shift; my ($C,$q) = @_; 
        $PURE = 0;
        '';
    }
}


