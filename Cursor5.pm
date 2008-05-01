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
our $DEPTH = 0;

binmode(STDIN, ":utf8");
binmode(STDERR, ":utf8");
binmode(STDOUT, ":utf8");

use Carp;
use utf8;

$SIG{__DIE__} = sub { confess(@_) };

sub new {
    my $class = shift;
    my $orig = shift;
    my %args = ('_pos' => 0, '_from' => 0, '_orig' => \$orig);
    while (@_) {
	my $name = shift;
	$args{'_' . $name} = shift;
    }
    my $self = bless \%args, ref $class || $class;
    my $buf = $self->{_orig};
    print STDERR " orig ", $$buf,"\n" if $DEBUG;
    $self->BUILD;
    $self->_AUTOLEXpeek('Perl::expect_term');
    $self;
}

use YAML::XS;

my $VERBOSE = 1;
my $RE_verbose = 1;

# XXX still full of ASCII assumptions

our %lexers;       # per language, the cache of lexers, keyed by rule name

# most cursors just copy forward the previous value of the following two items:
#has $._orig; 	       # per match, the original string we are matching against
#has StrPos $._from = 0;
#has StrPos $._to = 0;
#has StrPos $._pos = 0;
#has Cursor $._prior;

sub from { $_[0]->{_from} }
sub to { $_[0]->{_to} }
sub pos { $_[0]->{_pos} }
sub peek { $_[0]->{_peek} }
sub orig { $_[0]->{_orig} }

# unwarranted chumminess with Perl grammar
sub ws_from { $_[0]->{_ws_from} }
sub ws_to { $_[0]->{_ws_to} }

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
	my @pat = $ast->longest($self->cursor_peek());
	for (@pat) {
	    s/(\t\(\?#FATE.*?\))(.*)/$2$1/;
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
    my $buf = $self->{_orig};
    my $P = $self->{_pos};
    if ($P == length($$buf)) {
	return sub { undef };
    }
    my $chr = substr($$buf,$self->{_pos},1);

    $lexer->{$chr} //= do {
	print STDERR '=' x 72, "\n" if $DEBUG;
	print STDERR "GENERATING $key patterns starting with '$chr'\n" if $DEBUG;

	my @pats = grep { canmatch($_, $chr) } map { s/\t/.?\t/; $_; } @{$lexer->{PATS}};
	if (!@pats) {
	    print STDERR "No $key patterns start with '$chr'\n" if $DEBUG;
	    sub { undef };
	}
	else {
	    # extract fate comments before they are deleted
	    my $i = 1;
	    my $fates = [];
	    for (@pats) {
		s/\(\?#FATE (.*?)\)/(?#$i FATE $1)/ or return sub { undef };
		my $fstr = $1;
		$fates->[$i] = [0,0,0,$fstr];
		my $fate = $fates->[$i];
		while ($fstr =~ s/(\S+)\s+(\S+)\s*//) {
		    $fate->[0] = $1;
		    $fate->[1] = $2;
		    $fate = $fate->[2] = [0,0,0,$fstr] if $fstr ne '';
		}
		$i++;
	    }

	    my $pat = "^(?:\n(" . join(")\n|(",@pats) . '))';

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

	    print STDERR "#FATES: ", @$fates - 1, "\n" if $lexverbose;

	    for my $i (1..@$fates-1) {
		print STDERR $i, ': ', $fates->[$i][3], "\n" if $lexverbose;
	    }

	    # generate match closure at the last moment
	    sub {
		my $C = shift;

		print STDERR "lexing $key\n" if $DEBUG;
		die "orig disappeared!!!" unless length($$buf);

		return '' unless $lexer;

		pos($$buf) = $C->{_pos};

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
			my $result = $fates->[$last] // return;
			for my $x (1 .. $max) {
			    my $beg = $-[$x];
			    next unless defined $beg;
			    my $end = $+[$x];
			    my $f = $fates->[$x][3];
			    no strict 'refs';
			    if ($lexverbose or ($DEBUG and $x == $last)) {
				print STDERR "\$$x: $beg..$end\t$$x\t ",
				    $x == $last ? "====>" : "---->",
				    " $f\n";
			    }
			}
			print STDERR "success at '", substr($$buf,$C->{_pos},10), "'\n" if $DEBUG;
			$result;
		    }
		    else {
			print STDERR "NO LEXER MATCH at '", substr($$buf,$C->{_pos},10), "'\n" if $DEBUG;
			return;
		    }
		}
	    }
	}
    };
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
}

sub cursor_peek { my $self = shift;

    my %r = %$self;
    $r{_peek} = 1;
    bless \%r, ref $self;
}

sub cursor_fate { my $self = shift;
    my $pkg = shift;
    my $name = shift;

    my %r = %$self;
    my $tag;
    my $try;
    
    my $fate = $self->{_fate};
    if ($fate and $fate->[0] eq $name) {
        print STDERR "Fate passed to $name: $$fate[3]\n";
        ($tag, $try, $fate) = @$fate;
	$r{_fate} = $fate;
    }
    else {
        $fate = $self->_AUTOLEXnow("${pkg}::$name")->($self);
        if ($fate) {
            print STDERR "FATE OF ${pkg}::$name: $$fate[3]\n";
            ($tag, $try, $fate) = @$fate;
	    $r{_fate} = $fate;
        }
        else {
            print STDERR "NO FATE FOR ${pkg}::$name (will probe)\n";
            $tag = '';
        }
    }
    return (bless \%r, ref $self), $tag, $try;
}

sub cursor_all { my $self = shift;
    my $fpos = shift;
    my $tpos = shift;

    my %r = %$self;
    $r{_from} = $fpos;
    $r{_to} = $tpos;
    $r{_pos} = $tpos;

    bless \%r, ref $self;
}

sub cursor { my $self = shift;
    my $tpos = shift;

    my %r = %$self;
    $r{_from} = $self->{_pos} // 0;
    $r{_to} = $tpos;
    $r{_pos} = $tpos;

    bless \%r, ref $self;
}

sub cursor_rev { my $self = shift;
    my $fpos = shift;

    my %r = %$self;
    $r{_pos} = $fpos;
    $r{_from} = $fpos;
    $r{_to} = $self->{_from};

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
    $pos = $self->{_pos} if defined $self->{_pos};
    print STDERR $pos,"\t", ':' x $lvl, ' ', $name, " [", $file, ":", $line, "] @subs\n" if $DEBUG;
    {lvl => $lvl};
}

sub retm { my $self = shift;
    warn "Returning non-Cursor: $self\n" unless exists $self->{_pos};
    my ($package, $file, $line, $subname, $hasargs) = caller(1);
    print STDERR $self->{_pos}, "\t", ':' x $CTX->{lvl}, ' ', $subname, " returning @{[$self->{_from}]}..@{[$self->{_to}]}\n" if $DEBUG;
    $self;
}

sub _MATCHIFY { my $self = shift;
    my @result = map { $_->cursor_all($self->{_pos}, $_->{_to})->retm() } @_;
    if (wantarray) {
	@result;
    }
    else {
	$result[0];
    }
}

sub _STARf { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm;

    map { $_->retm() }
        $self->cursor($self->{_pos}),
        $self->_PLUSf($block);
}

sub _STARg { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm;

    map { $_->retm() } reverse
            $self->cursor($self->{_pos}),
            $self->_PLUSf($block);
}

sub _STARr { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm;
    my $to = $self;
    my @all;
    my $eos = length(${$self->{_orig}});
    for (;;) {
      last if $to->{_pos} == $eos;
	my @matches = $block->($to);  # XXX shouldn't read whole list
#            say @matches.perl;
      last unless @matches;
	my $first = $matches[0];  # no backtracking into block on ratchet
	push @all, $first;
	$to = $first;
    }
    $self->cursor($to->{_to})->retm();
}

sub _PLUSf { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm;
    my $x = $self;

    my @result;
    # don't go beyond end of string
    return () if $self->{_pos} == length(${$self->{_orig}});
    do {
	for my $x ($block->($self)) {
	    push @result, map { $self->cursor($_->{_to}) } $x, $x->_PLUSf($block);
	}
    };
    map { $_->retm() } @result;
}

sub _PLUSg { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm;

    reverse $self->_PLUSf($block, @_);
}

sub _PLUSr { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm;
    my $to = $self;
    my @all;
    my $eos = length(${$self->{_orig}});
    for (;;) {
      last if $to->{_pos} == $eos;
	my @matches = $block->($to);  # XXX shouldn't read whole list
      last unless @matches;
	my $first = $matches[0];  # no backtracking into block on ratchet
	#print STDERR $matches->perl, "\n" if $DEBUG;
	push @all, $first;
	$to = $first;
    }
    return () unless @all;
    my $r = $self->cursor($to->{_pos});
    $r->retm();
}

sub _REPSEPf { my $self = shift;
    my $sep = shift;
    my $block = shift;

    local $CTX = $self->callm;
    my $x = $self;

    my @result;
    # don't go beyond end of string
    return () if $self->{_pos} == length(${$self->{_orig}});
    do {
	for my $x ($block->($self)) {
	    for my $s ($sep->($x)) {
		push @result, map { $self->cursor($_->{_to}) } $x, $s->_REPSEPf($sep,$block);
	    }
	}
    };
    map { $_->retm() } @result;
}

sub _REPSEPg { my $self = shift;
    my $sep = shift;
    my $block = shift;

    local $CTX = $self->callm;

    reverse $self->_REPSEPf($sep, $block, @_);
}

sub _REPSEPr { my $self = shift;
    my $sep = shift;
    my $block = shift;

    local $CTX = $self->callm;
    my $to = $self;
    my @all;
    my $eos = length(${$self->{_orig}});
    for (;;) {
      last if $to->{_pos} == $eos;
	my @matches = $block->($to);  # XXX shouldn't read whole list
      last unless @matches;
	my $first = $matches[0];  # no backtracking into block on ratchet
	#print STDERR $matches->perl, "\n" if $DEBUG;
	push @all, $first;
	my @seps = $sep->($first);
      last unless @seps;
	my $sep = $seps[0];
	$to = $sep;
    }
    return () unless @all;
    my $r = $self->cursor($to->{_pos});
    $r->retm();
}

sub _OPTr { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm;

    my $x = ($block->($self))[0];
    my $r = $x // $self->cursor($self->{_pos});
    $r->retm();
}

sub _OPTg { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm;
    my @x = $block->($self);
    map { $_->retm() }
        $block->($self),
        $self->cursor($self->{_pos});
}

sub _OPTf { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm;
    map { $_->retm() }
        $self->cursor($self->{_pos}),
        $block->($self);
}

sub _BRACKET { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm;
    map { $_->retm() }
        $block->($self);
}

sub _PAREN { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm;
    map { $_->retm() }
        $block->($self);
}

sub _NOTBEFORE { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm;
    my @all = $block->($self);
    return () if @all;  # XXX loses continuation
    return $self->cursor($self->{_pos})->retm();
}

sub _NOTCHAR { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm;
    my @all = $block->($self);
    return () if @all;  # XXX loses continuation
    return $self->cursor($self->{_pos}+1)->retm();
}

sub before { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm;
    my @all = $block->($self);
    if (@all and $all[0]) {
        return $all[0]->cursor_all(($self->{_pos}) x 2)->retm();
    }
    return ();
}

sub after { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm;
    my $end = $self->cursor($self->{_pos});
    my @all = $block->($end);          # Make sure $_->{_from} == $_->{_to}
    if (@all and $all[0]) {
        return $all[0]->cursor_all(($self->{_pos}) x 2)->retm();
    }
    return ();
}

sub null { my $self = shift;
    local $CTX = $self->callm;
    return $self->cursor($self->{_pos})->retm();
}

sub _ASSERT { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm;
    my @all = $block->($self);
    if ((@all and $all[0]->{_bool})) {
        return $self->cursor($self->{_pos})->retm();
    }
    return ();
}

sub _BINDVAR { my $self = shift;
    my $var = shift;
    my $block = shift;

    local $CTX = $self->callm;
    map { $$var = $_; $_->retm() }  # XXX doesn't "let"
        $block->($self);
}

sub _BINDPOS { my $self = shift;
    my $pos = shift;
    my $block = shift;

    local $CTX = $self->callm;
    map { $_->retm() }
        $block->($self);
}

sub _BINDNAMED { my $self = shift;
    my $name = shift;
    my $block = shift;

    local $CTX = $self->callm;
    map { $_->retm() }
        $block->($self);
}

sub _EXACT { my $self = shift;
    my $s = shift;

    local $CTX = $self->callm($s);
    my $P = $self->{_pos} // 0;
    my $len = length($s);
    my $buf = $self->{_orig};
    if (substr($$buf, $P, $len) eq $s) {
        print STDERR "EXACT $s matched @{[substr($$buf,$P,$len)]} at $P $len\n" if $DEBUG;
        my $r = $self->cursor($P+$len);
        $r->retm();
    }
    else {
        print STDERR "EXACT $s didn't match @{[substr($$buf,$P,$len)]} at $P $len\n" if $DEBUG;
        return ();
    }
}

sub _EXACT_rev { my $self = shift;
    my $s = shift;

    local $CTX = $self->callm;
    my $len = length($s);
    my $from = $self->{_from} - $len;
    my $buf = $self->{_orig};
    if ($from >= 0 and substr($$buf, $from, $len) eq $s) {
        my $r = $self->cursor_rev($from);
        $r->retm();
    }
    else {
#        say "EXACT_rev $s didn't match @{[substr($!orig,$from,$len)]} at $from $len";
        return ();
    }
}

sub _ARRAY { my $self = shift;
    local $CTX = $self->callm(0+@_);
    my $P = $self->{_pos} // 0;
    my $buf = $self->{_orig};
    my @array = sort { length($b) <=> length($a) } @_;	# XXX suboptimal
    my @result = ();
    for my $s (@array) {
	my $len = length($s);
	if (substr($$buf, $P, $len) eq $s) {
	    print STDERR "ARRAY elem $s matched @{[substr($$buf,$P,$len)]} at $P $len\n" if $DEBUG;
	    my $r = $self->cursor($P+$len);
	    push @result, $r->retm('');
	}
    }
    return @result;
}

sub _ARRAY_rev { my $self = shift;
    local $CTX = $self->callm(0+@_);
    my $buf = $self->{_orig};
    my @array = sort { length($b) <=> length($a) } @_;	# XXX suboptimal
    my @result = ();
    for my $s (@array) {
	my $len = length($s);
	my $from = $self->{_from} = $len;
	if (substr($$buf, $from, $len) eq $s) {
	    print STDERR "ARRAY_rev elem $s matched @{[substr($$buf,$from,$len)]} at $from $len\n" if $DEBUG;
	    my $r = $self->cursor_rev($from);
	    push @result, $r->retm('');
	}
    }
    return @result;
}

sub _DIGIT { my $self = shift;
    local $CTX = $self->callm;
    my $P = $self->{_pos};
    my $buf = $self->{_orig};
    my $char = substr($$buf, $P, 1);
    if ($char =~ /^\d$/) {
        my $r = $self->cursor($P+1);
        return $r->retm();
    }
    else {
#        say "DIGIT didn't match $char at $P";
        return ();
    }
}

sub _DIGIT_rev { my $self = shift;
    local $CTX = $self->callm;
    my $from = $self->{_from} - 1;
    if ($from < 0) {
#        say "DIGIT_rev didn't match $char at $from";
        return ();
    }
    my $buf = $self->{_orig};
    my $char = substr($$buf, $from, 1);
    if ($char =~ /^\d$/) {
        my $r = $self->cursor_rev($from);
        return $r->retm();
    }
    else {
#        say "DIGIT_rev didn't match $char at $from";
        return ();
    }
}

sub _ALNUM { my $self = shift;
    local $CTX = $self->callm;
    my $P = $self->{_pos};
    my $buf = $self->{_orig};
    my $char = substr($$buf, $P, 1);
    if ($char =~ /^\w$/) {
        my $r = $self->cursor($P+1);
        return $r->retm();
    }
    else {
#        say "ALNUM didn't match $char at $P";
        return ();
    }
}

sub _ALNUM_rev { my $self = shift;
    local $CTX = $self->callm;
    my $from = $self->{_from} - 1;
    if ($from < 0) {
#        say "ALNUM_rev didn't match $char at $from";
        return ();
    }
    my $buf = $self->{_orig};
    my $char = substr($$buf, $from, 1);
    if ($char =~ /^\w$/) {
        my $r = $self->cursor_rev($from);
        return $r->retm();
    }
    else {
#        say "ALNUM_rev didn't match $char at $from";
        return ();
    }
}

sub alpha { my $self = shift;
    local $CTX = $self->callm;
    my $P = $self->{_pos};
    my $buf = $self->{_orig};
    my $char = substr($$buf, $P, 1);
    if ($char =~ /^[a-z_A-Z]$/) {
        my $r = $self->cursor($P+1);
        return $r->retm();
    }
    else {
#        say "alpha didn't match $char at $P";
        return ();
    }
}

sub _SPACE { my $self = shift;
    local $CTX = $self->callm;
    my $P = $self->{_pos};
    my $buf = $self->{_orig};
    my $char = substr($$buf, $P, 1);
    if ($char =~ /^\s$/) {
        my $r = $self->cursor($P+1);
        return $r->retm();
    }
    else {
#        say "SPACE didn't match $char at $P";
        return ();
    }
}

sub _SPACE_rev { my $self = shift;
    local $CTX = $self->callm;
    my $from = $self->{_from} - 1;
    if ($from < 0) {
#        say "SPACE_rev didn't match $char at $from";
        return ();
    }
    my $buf = $self->{_orig};
    my $char = substr($$buf, $from, 1);
    if ($char =~ /^\s$/) {
        my $r = $self->cursor_rev($from);
        return $r->retm();
    }
    else {
#        say "SPACE_rev didn't match $char at $from";
        return ();
    }
}

sub _HSPACE { my $self = shift;
    local $CTX = $self->callm;
    my $P = $self->{_pos};
    my $buf = $self->{_orig};
    my $char = substr($$buf, $P, 1);
    if ($char =~ /^[ \t\r]$/ or ($char =~ /^\s$/ and $char !~ /^[\n\f\0x0b\x{2028}\x{2029}]$/)) {
        my $r = $self->cursor($P+1);
        return $r->retm();
    }
    else {
#        say "HSPACE didn't match $char at $P";
        return ();
    }
}

sub _HSPACE_rev { my $self = shift;
    local $CTX = $self->callm;
    my $from = $self->{_from} - 1;
    if ($from < 0) {
#        say "HSPACE_rev didn't match $char at $from";
        return ();
    }
    my $buf = $self->{_orig};
    my $char = substr($$buf, $from, 1);
    if ($char =~ /^[ \t\r]$/ or ($char =~ /^\s$/ and $char !~ /^[\n\f\0x0b\x{2028}\x{2029}]$/)) {
        my $r = $self->cursor_rev($from);
        return $r->retm();
    }
    else {
#        say "HSPACE_rev didn't match $char at $from";
        return ();
    }
}

sub _VSPACE { my $self = shift;
    local $CTX = $self->callm;
    my $P = $self->{_pos};
    my $buf = $self->{_orig};
    my $char = substr($$buf, $P, 1);
    if ($char =~ /^[\n\f\x0b\x{2028}\x{2029}]$/) {
        my $r = $self->cursor($P+1);
        return $r->retm();
    }
    else {
#        say "VSPACE didn't match $char at $P";
        return ();
    }
}

sub _VSPACE_rev { my $self = shift;
    local $CTX = $self->callm;
    my $from = $self->{_from} - 1;
    if ($from < 0) {
#        say "VSPACE_rev didn't match $char at $from";
        return ();
    }
    my $buf = $self->{_orig};
    my $char = substr($$buf, $from, 1);
    if ($char =~ /^[\n\f\x0b\x{2028}\x{2029}]$/) {
        my $r = $self->cursor_rev($from);
        return $r->retm();
    }
    else {
#        say "VSPACE_rev didn't match $char at $from";
        return ();
    }
}

sub _CCLASS { my $self = shift;
    my $cc = shift;

    local $CTX = $self->callm;
    my $P = $self->{_pos};
    my $buf = $self->{_orig};
    my $char = substr($$buf, $P, 1);
    if ($char =~ /$cc/) {
        my $r = $self->cursor($P+1);
        return $r->retm();
    }
    else {
#        say "CCLASS didn't match $char at $P";
        return ();
    }
}

sub _CCLASS_rev { my $self = shift;
    my $cc = shift;

    local $CTX = $self->callm;
    my $from = $self->{_from} - 1;
    if ($from < 0) {
#        say "CCLASS didn't match $char at $from";
        return ();
    }
    my $buf = $self->{_orig};
    my $char = substr($$buf, $from, 1);
    if ($char =~ /$cc/) {
        my $r = $self->cursor_rev($from);
        return $r->retm();
    }
    else {
#        say "CCLASS didn't match $char at $from";
        return ();
    }
}

sub _ANY { my $self = shift;
    local $CTX = $self->callm;
    my $P = $self->{_pos};
    my $buf = $self->{_orig};
    if ($P < length($$buf)) {
        $self->cursor($P+1)->retm();
    }
    else {
#        say "ANY didn't match anything at $P";
        return ();
    }
}

sub _BOS { my $self = shift;
    local $CTX = $self->callm;
    my $P = $self->{_pos};
    if ($P == 0) {
        $self->cursor($P)->retm();
    }
    else {
        return ();
    }
}

sub _BOL { my $self = shift;
    local $CTX = $self->callm;
    my $P = $self->{_pos};
    my $buf = $self->{_orig};
    if ($P == 0 or substr($$buf, $P-1, 1) =~ /^[\n\f\x0b\x{2028}\x{2029}]$/) {
        $self->cursor($P)->retm();
    }
    else {
        return ();
    }
}

sub _EOS { my $self = shift;
    local $CTX = $self->callm;
    my $P = $self->{_pos};
    my $buf = $self->{_orig};
    if ($P == length($$buf)) {
        $self->cursor($P)->retm();
    }
    else {
        return ();
    }
}

sub _EOL { my $self = shift;
    local $CTX = $self->callm;
    my $P = $self->{_pos};
    my $buf = $self->{_orig};
    if ($P == length($$buf) or substr($$buf, $P, 1) =~ /^(?:\r\n|[\n\f\x0b\x{2028}\x{2029}])$/) {
        $self->cursor($P)->retm();
    }
    else {
        return ();
    }
}

sub _RIGHTWB { my $self = shift;
    local $CTX = $self->callm;
    my $P = $self->{_pos};
    my $buf = $self->{_orig};
    pos($$buf) = $P - 1;
    if ($$buf =~ /\w\b/) {
        $self->cursor($P)->retm();
    }
    else {
        return ();
    }
}

sub _LEFTWB { my $self = shift;
    local $CTX = $self->callm;
    my $P = $self->{_pos};
    my $buf = $self->{_orig};
    pos($$buf) = $P;
    if ($$buf =~ /\b(?=\w)/) {
        $self->cursor($P)->retm();
    }
    else {
        return ();
    }
}

sub _REDUCE { my $self = shift;
    my $tag = shift;

    local $CTX = $self->callm($tag);
    my $P = $self->{_pos};
    my $F = $self->{_from};
    $self->{_Rtag} = $tag;
    print STDERR "Success $tag from $F to $P\n" if $DEBUG;
#    $self->whats;
    $self;
#    $self->cursor($P);
}

sub _COMMITBRANCH { my $self = shift;
    local $CTX = $self->callm;
    my $P = $self->{_pos};
    print STDERR "Commit branch to $P\n" if $DEBUG;
#    $self->cursor($P);  # XXX currently noop
    $self;
}

sub _COMMITRULE { my $self = shift;
    local $CTX = $self->callm;
    my $P = $self->{_pos};
    print STDERR "Commit rule to $P\n" if $DEBUG;
#    $self->cursor($P);  # XXX currently noop
    $self;
}

sub commit { my $self = shift;
    local $CTX = $self->callm;
    my $P = $self->{_pos};
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
	$s =~ s/^/\n  /mg;
	$s;
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
		$lexer = $C->$name(undef);
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
                my $lexer = $C->$name(undef, $re);
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
                my $lexer = $C->$name(undef, $str);
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
	    print STDERR "NULLABLE ".ref($chunk)."\n" unless $chunk->{min};
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


