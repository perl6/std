our $CTX;
$CTX->{lvl} = 0;
package Cursor5;

use strict;
use warnings;
use Encode;

our %AUTOLEXED;
our $FATES;
our $PURE;

binmode(STDIN, ":utf8");
binmode(STDERR, ":utf8");
binmode(STDOUT, ":utf8");

use Carp;
use utf8;

$SIG{__DIE__} = sub { confess(@_) };

sub new {
    my $class = shift;
    my %args = ('pos' => 0, @_);
    print length($args{orig});
    bless \%args, ref $class || $class;
}

use YAML::Syck;

my $VERBOSE = 1;
my $callouts = 1;
my $RE_verbose = 1;

# XXX still full of ASCII assumptions

# most cursors just copy forward the previous value of the following two items:
#has $self->{orig};        # per match, the original string we are matching against
our %lexers;       # per language, the cache of lexers, keyed by (|) location

#has Bool $.bool = 1;
#has StrPos $.from = 0;
#has StrPos $.to = 0;
#has StrPos $.pos = 0;
#has Cursor $.prior;
#has %.mykeys};
#has Str $.name;
#has $.item;

sub lexers { my $self = shift;
    %lexers;    # XXX should be different per language, sigh
}

my $fakepos = 1;

sub _AUTOLEXpeek { my $self = shift;
    my $key = shift;

    print "peek $key";
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
            print "oops ", ref($key);
        }
    }
    return $self->{lexers}{$key} = $self->_AUTOLEXgen($key);
}

sub _AUTOLEXgen { my $self = shift;
    my $key = shift;

    print "gen0 $key";
    my $lexer = { x => 'y' };
    if (! -s "lex/$key.yml") {
	print "gen1";

	my $ast = LoadFile("yamlg5/$key.yml");
	Encode::_utf8_on($ast);
	my $oldfakepos = $AUTOLEXED{$key} // 0;
	local $FATES;
	$FATES = [];

	$AUTOLEXED{$key} = $fakepos;
	my $pat = $ast->longest($self);
	warn $pat;
	if (Encode::is_utf8($pat)) { print "UTF8 ON" } else { print "UTF8 OFF" }
	if ($pat =~ /Â/) { print "bad Â" }

	$AUTOLEXED{$key} = $oldfakepos;

	$lexer = { PAT => $pat, FATES => $FATES };
	print "gen2";
	open(my $cache, '>', "lex/$key.yml") // warn "Can't print: $!";
	print $cache Dump($lexer) or warn "Can't print: $!";
	close($cache) or warn "Can't close: $!";
	print "gen3";
    }
    $lexer;
}

sub _AUTOLEXnow { my $self = shift;
    my $key = shift;

    print "now $key";
    my $lexer = $self->{lexers}->{$key} // do {
	local %AUTOLEXED;
	$self->_AUTOLEXpeek($key);
    };

    $lexer->{MATCH} //= do {
	print $key,":";

	my $buf = $self->{orig};	# XXX this might lose pos()...
	print "AT: ", substr($buf,0,20);

	# generate match closure at the last moment
	sub {
	    $| = 1;
	    my $C = shift;

	    print "LEN: ", length($buf),"\n";

	    my $stuff;
	    if ((-e "lex/$key.yml")) {
		$stuff = LoadFile("lex/$key.yml");
	    }
	    else {
		$stuff = {"PAT" => $lexer->{PAT}, "FATES" => $lexer->{FATES}};
	    }

	    die "Huh?  No PAT!!!\n" unless $stuff->{PAT};

	    my $pat = '^' . decode('utf8', $stuff->{PAT});
	    Encode::_utf8_off($pat);
	    $pat = decode('utf8', $pat);
	    if (Encode::is_utf8($pat)) { print "UTF8 ON" } else { print "UTF8 OFF" }
	    
	    print '=' x 72, "\n";
	    {
		my $i = 1;
		$pat =~ s/\((?!\?[#:])/(?#@{[$i++]})(/g;
	    }
	    print "PAT: ", $pat,"\n";
	    $pat =~ s/\(\?#.*?\)//g;
	    $pat =~ s/^[ \t]*\n//gm;

	    # remove whitespace that will confuse TRE greatly
	    $pat =~ s/\s+//g;

	    1 while $pat =~ s/\(((\?:)?)\)/($1 !!!OOPS!!! )/;
	    1 while $pat =~ s/\[\]/[ !!!OOPS!!! ]/;
	    my $tmp = $pat;
	    "42" =~ /$tmp/ and print "PARSES!";

	    print "PAT: ", $pat,"\n";
	    print '=' x 72, "\n";

	    my $fate = $stuff->{FATES};
	    print "#FATES: ", @$fate + 0;

	    unshift @$fate, "";	# start at $1
	    for my $i (1..@$fate-1) {
		print $i, ': ', $fate->[$i];
	    }
	    my $result = "";

	    #########################################
	    # No normal p5 match/subst below here!!!
	    #########################################
	    {
	    use re::engine::TRE;

	    if (($buf =~ m/$pat/xgc)) {	# XXX does this recompile $pat every time?
		my $max = @+ - 1;
		my $last = @- - 1;	# ignore '$0'
		print "LAST: $last";
		$result = $fate->[$last] // "OOPS";
		for my $x (1 .. $max) {
		    my $beg = $-[$x];
		    next unless defined $beg;
		    my $end = $+[$x];
		    my $f = $fate->[$x];
		    no strict 'refs';
		    print "\$$x: $beg..$end\t$$x\t----> $f\n";
		}
	    }
	    else {
		print "NO LEXER MATCH at", substr($buf,$C->{pos},10), "\n";
	    }
	    print "$result\n";
	    $result =~ s/\w+\///g;
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

sub item { my $self = shift;

    if (not defined $self->{item}) {
        $self->{item} = substr($self->{orig}, $self->{from}, $self->{to} - $self->{from});
    }
    $self->{item};
}

sub matchify { my $self = shift;

    my %bindings;
    my $m;
    my $next;
    print "MATCHIFY ", ref $self;
    for ($m = $self->{prior}; $m; $m = $next) {
        $next = $m->{prior};
        undef $m->{prior};
        my $n = $m->{name};
        print "MATCHIFY $n";
        if (not $bindings{$n}) {
            $bindings{$n} = [];
#                %.mykeys = Hash.new unless defined %!mykeys;
            $self->{mykeys}{$n}++;
        }
        unshift @{$bindings{$n}}, $m;
    }
    for my $k (keys(%bindings)) { my $v = $bindings{$k};
        $self->{$k} = $v;
        # XXX alas, gets lost in the next cursor...
        print "copied $k ", $v;
    }
    undef $self->{prior};
    $self;
}

sub cursor_all { my $self = shift;
    my $fpos = shift;
    my $tpos = shift;

    my $r = $self->new(
        orig => ($self->{orig}),
        #lexers => ($self->lexers),
        from => ($fpos),
        to => ($tpos),
        pos => ($tpos),
        prior => (defined $self->{name} ? $self : $self->{prior}),
        mykeys => $self->{mykeys},
    );
    print "PRIOR ", $r->{prior}->{name} if defined $r->{prior};
    $r;
}

sub cursor { my $self = shift;
    my $tpos = shift;

    $self->new(
        orig => ($self->{orig}),
        #lexers => ($self->lexers),
        from => ($self->{pos} // 0),
        to => ($tpos),
        pos => ($tpos),
        prior => (defined $self->{name} ? $self : $self->{prior}),
        mykeys => $self->{mykeys},
    );
}

sub cursor_rev { my $self = shift;
    my $fpos = shift;

    $self->new(
        orig => ($self->{orig}),
        #lexers => ($self->lexers),
        pos => ($fpos),
        from => ($fpos),
        to => ($self->{from}),
        prior => (defined $self->{name} ? $self : $self->{prior}),
        mykeys => $self->{mykeys},
    );
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
    $pos = $self->{pos} if defined $self->{orig};
    print $pos,"\t", ':' x $lvl, ' ', $name, " [", $file, ":", $line, "]";
    {lvl => $lvl};
}

sub whats { my $self = shift;

    my @k = keys(%{$self->{mykeys}});
    print "  $self ===> @{[$self->{from}.' '. $self->{to}.' '. $self->{item}]} (@k)";
    for my $k (@k) {
        print "   $k => @{[$self->{mykeys}->{$k}]}";
    }
}

sub retm { my $self = shift;
    my $bind = shift;

    print "Returning non-Cursor: $self" unless exists $self->{pos};
    my $binding = "";
    if (defined $bind and $bind ne '') {
        $self->{name} = $bind;
        $binding = "      :bind<$bind>";
    }
    my ($package, $file, $line, $subname, $hasargs) = caller(1);
    print $self->{pos}, "\t", ':' x $CTX->{lvl}, ' ', $subname, " returning @{[$self->{from}]}..@{[$self->{to}]}$binding";
#    $self->whats();
    $self;
}

sub _MATCHIFY { my $self = shift;
    my $bind = shift;

    map { $_->cursor_all($self->{pos}, $_->{to})->retm($bind) }
    map { $_->matchify } @_;
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
        #XXX voodoo fix to prevent bogus stringification
        map { $_->perl->say; $_ }
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
    $self->cursor($to->{to});  # XXX $_->retm blows up pugs for unfathomable reasons
}

sub _PLUSf { my $self = shift;
    my $block = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $x = $self;

    my @result;
    map { $_->retm($bind) } do {
	for my $x ($block->($self)) {
	    push @result, map { $self->cursor($_->{to}) } $x, $self->_PLUSf($x, $block);
	}
    };
    @result;
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
	#print $matches->perl;
	push @all, $first;
	$to = $first;
    }
    return () unless @all;
    my $r = $self->cursor($to->{to});
    $r->retm($bind);
}

sub _OPTr { my $self = shift;
    my $block = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;

    my $x = $block->($self)[0]; # ratchet
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

# fast rejection of current prefix
sub _EQ { my $self = shift;
    my $P = shift;
    my $s = shift;

    my $len = length($s);
    return 1 if substr($self->{orig}, $P, $len) eq $s;
    return ();
}

sub _EXACT { my $self = shift;
    my $s = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm($s);
    my $P = $self->{pos};
    my $len = length($s);
    if (substr($self->{orig}, $P, $len) eq $s) {
#        say "EXACT $s matched {substr($!orig,$P,$len)} at $P $len";
        my $r = $self->cursor($P+$len);
        $r->retm($bind);
    }
    else {
#        say "EXACT $s didn't match {substr($!orig,$P,$len)} at $P $len";
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
    if ($from >= 0 and substr($self->{orig}, $from, $len) eq $s) {
        my $r = $self->cursor_rev($from);
        $r->retm($bind);
    }
    else {
#        say "vEXACT $s didn't match {substr($!orig,$from,$len)} at $from $len";
        return ();
    }
}

sub _DIGIT { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $P = $self->{pos};
    my $char = substr($self->{orig}, $P, 1);
    if ("0" le $char and $char le "9") {
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
#        say "vDIGIT didn't match $char at $from";
        return ();
    }
    my $char = substr($self->{orig}, $from, 1);
    if ("0" le $char and $char le "9") {
        my $r = $self->cursor_rev($from);
        return $r->retm($bind);
    }
    else {
#        say "vDIGIT didn't match $char at $from";
        return ();
    }
}

sub _ALNUM { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $P = $self->{pos};
    my $char = substr($self->{orig}, $P, 1);
    if ("0" le $char and $char le "9" or 'A' le $char and $char le 'Z' or 'a' le $char and $char le 'z' or $char eq '_') {
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
#        say "vALNUM didn't match $char at $from";
        return ();
    }
    my $char = substr($self->{orig}, $from, 1);
    if ("0" le $char and $char le "9" or 'A' le $char and $char le 'Z' or 'a' le $char and $char le 'z' or $char eq '_') {
        my $r = $self->cursor_rev($from);
        return $r->retm($bind);
    }
    else {
#        say "vALNUM didn't match $char at $from";
        return ();
    }
}

sub alpha { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $P = $self->{pos};
    my $char = substr($self->{orig}, $P, 1);
    if ('A' le $char and $char le 'Z' or 'a' le $char and $char le 'z' or $char eq '_') {
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
    my $char = substr($self->{orig}, $P, 1);
    if ($char eq " " | "\t" | "\r" | "\n" | "\f") {
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
#        say "vSPACE didn't match $char at $from";
        return ();
    }
    my $char = substr($self->{orig}, $from, 1);
    if ($char eq " " | "\t" | "\r" | "\n" | "\f") {
        my $r = $self->cursor_rev($from);
        return $r->retm($bind);
    }
    else {
#        say "vSPACE didn't match $char at $from";
        return ();
    }
}

sub _HSPACE { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $P = $self->{pos};
    my $char = substr($self->{orig}, $P, 1);
    if ($char eq " " | "\t" | "\r") {
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
#        say "vHSPACE didn't match $char at $from";
        return ();
    }
    my $char = substr($self->{orig}, $from, 1);
    if ($char eq " " | "\t" | "\r") {
        my $r = $self->cursor_rev($from);
        return $r->retm($bind);
    }
    else {
#        say "vHSPACE didn't match $char at $from";
        return ();
    }
}

sub _VSPACE { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $P = $self->{pos};
    my $char = substr($self->{orig}, $P, 1);
    if ($char eq "\n" | "\f") {
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
#        say "vVSPACE didn't match $char at $from";
        return ();
    }
    my $char = substr($self->{orig}, $from, 1);
    if ($char eq "\n" | "\f") {
        my $r = $self->cursor_rev($from);
        return $r->retm($bind);
    }
    else {
#        say "vVSPACE didn't match $char at $from";
        return ();
    }
}

sub _ANY { my $self = shift;
    my %args = @_;
    my $bind = $args{bind} // '';

    local $CTX = $self->callm;
    my $P = $self->{pos};
    if ($P < length($self->{orig})) {
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
    # XXX should define in terms of BOL or after vVSPACE
    if ($P == 0 or substr($self->{orig}, $P-1, 1) eq "\n" or substr($self->{orig}, $P-2, 2) eq "\r\n") {
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
    if ($P == length($self->{orig})) {
        $self->cursor($P)->retm($bind);
    }
    else {
        return ();
    }
}

sub _REDUCE { my $self = shift;
    my $tag = shift;

    local $CTX = $self->callm($tag);
#    my $P = $self->{pos};
#    say "Success $tag from $+FROM to $P";
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
	print ':' x $lvl, ' ', $name, " [", $file, ":", $line, "]" if $RE_verbose;
    }
}

{ package REbase;
    sub longest { my $self = shift; my ($C) = @_;  ::here("UNIMPL @{[ref $self]}"); "$self" }
}

{ package RE; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        ::here();
        local $PURE = 1;
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
                if ($re->{'name'} eq 'before') {
                    my $result = $re->longest($C);
                    $PURE = 0;
                    return $result;
                }
            }
        }
        '[]';
    }
}

{ package RE_assertvar; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        $PURE = 0;
        '';
    }
}

{ package RE_block; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        $PURE = 0;
        '';
    }
}

{ package RE_bindvar; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_;  ::here(); $self->{'atom'}->longest($C) }
}

{ package RE_bindnamed; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_;  ::here(); $self->{'atom'}->longest($C) }
}

{ package RE_bindpos; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_;  ::here(); $self->{'atom'}->longest($C) }
}

{ package RE_bracket; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_;  ::here(); ::indent("\n(?:\n" . ::indent($self->{'re'}->longest($C)) . "\n)") }
}

{ package RE_cclass; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        ::here($self->{'text'});
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
    sub longest { my $self = shift; my ($C) = @_;  '[]' }
}

{ package RE_double; our @ISA = 'RE_base';
    # XXX inadequate for "\n" without interpolation
    sub longest { my $self = shift; my ($C) = @_; 
        $PURE = 0;
        '';
    }
}

{ package RE_meta; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
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
    sub longest { my $self = shift; my ($C) = @_; 
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
                return '[a-z_A-Z]';
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
    sub longest { my $self = shift; my ($C) = @_; 
        $PURE = 0;
        '';
    }
}

{ package RE_method_re; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
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
                my $result = $re->longest($C);
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
    sub longest { my $self = shift; my ($C) = @_; 
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
    sub longest { my $self = shift; my ($C) = @_; 
        $PURE = 0;
        '';
    }
}

{ package RE_noop; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        '[]';
    }
}

{ package RE_ordered_conjunction; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        $PURE = 0;
        '';
    }
}

{ package RE_ordered_disjunction; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        my $alts = $self->{'zyg'};
        ::here(0+@$alts);
        my @result;
        for my $alt (@$alts) {
            my $pat = $alt->longest($C);
#            $pat .= ':' . $alt.<alt> if $callouts;
            push @result, $pat;
            last;
        }
        my $result = $result[0];
        print $result;
        $result;
    }
}

{ package RE_paren; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; ::here(); unshift @$FATES, ""; ::indent("\n(\n" . ::indent($self->{'re'}->longest($C)) . "\n)") }
}

{ package RE_quantified_atom; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        ::here();
        my $oldfakepos = $fakepos++;
        my $atom = $self->{'atom'}->longest($C);
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
    sub longest { my $self = shift; my ($C) = @_; 
        my $text = $self->{'text'};
        ::here($text);
        $fakepos++;
        $text =~ s/^<\s*//;
        $text =~ s/\s*>$//;
        $text =~ s/\s+/|/;
        $text;
    }
}

{ package RE_sequence; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
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
            my $next = $_->longest($C);
            last if $next eq '';
            $next = '' if $next eq '[]';
            push @result, $next;
            last unless $PURE;
        }
	if (@result) {
	    "(?: @{[ join(' ', @result) ]} )";
	}
	else {
	    "";
	}
    }
}

{ package RE_string; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($c) = @_; 
        ::here($self->{'text'});
        my $text = $self->{'text'};
        $fakepos++ if $self->{'min'};
        ::qm($text);
    }
}

{ package RE_submatch; our @ISA = 'RE_base';
    #method longest ($C) { ... }
}

{ package RE_unordered_conjunction; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        $PURE = 0;
        '';
    }
}

{ package RE_unordered_disjunction; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        my $alts = $self->{'zyg'};
        ::here(0+@$alts);
        my @result;
        my $oldfakepos = $fakepos;
        my $minfakepos = $fakepos + 1;
        for my $alt (@$alts) {
            $fakepos = $oldfakepos;
	    my $a = $alt->{alt};
	    push @$FATES, $a;
            my $pat = ::indent($alt->longest($C));
	    $pat = "OOPS" if $pat =~ m/^\s*$/;
            $pat = "( (?#START $a)\n$pat)\n(?#END $a)" if $callouts;
            push @result, $pat;
            $minfakepos = $oldfakepos if $fakepos == $oldfakepos;
        }
        my $result = "\n(?:\n  " . ::indent(join "\n| ", @result) . "\n)";
        print $result;
        $fakepos = $minfakepos;  # Did all branches advance?
        $result;
    }
}

{ package RE_var; our @ISA = 'RE_base';
    #method longest ($C) { ... }
    sub longest { my $self = shift; my ($C) = @_; 
        $PURE = 0;
        '';
    }
}


