use strict;
use warnings;

our $CTX = 0;
our $DEBUG = $ENV{STD5DEBUG} // 0;
$::DEBUG = $DEBUG;

{ package DEBUG;
    use constant {
        autolexer => 1,
        lexer => 2,
        fixed_length => 4,
        fates => 8,
        longest_token_pattern_generation => 16,
        EXPR => 32,
        matchers => 64,
        trace_call=> 128,
        cursors => 256,
        try_processing_in_STD5_dot_pm => 1024,
        mixins => 2048,
        callm_show_subnames => 16384
    };
}

our $DEPTH = 0;

sub ::deb {
    print ::LOG @_, "\n";
}

package Cursor5;

sub deb { my $self = shift;
    my $pos = ref $self && defined $self->{_pos} ? $self->{_pos} : "?";
    print ::LOG $pos, "\t", ':' x $CTX, ' ', @_, "\n";
}

$::DEBUG //= 0;

use Moose ':all' => { -prefix => 'moose_' };

use Encode;

our %AUTOLEXED;
our $ALT;
our $PREFIX = "";
my $IMP = '(?#::)';

binmode(STDIN, ":utf8");
binmode(STDERR, ":utf8");
binmode(STDOUT, ":utf8");
BEGIN {
    if ($^P) {
	open(::LOG, ">&1") or die "Can't create $0.log: $!";
    }
    else {
	open(::LOG, ">$0.log") or die "Can't create $0.log: $!";
    }
    binmode(::LOG, ":utf8");
}

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
#    $self->deb(" orig ", $$buf) if $DEBUG & DEBUG::cursors;
    $self->BUILD;
    $self;
}

sub mixin {
    my $self = shift;
    my @mixins = @_;

    my $WHAT = $self . '::';
    for my $mixin (@mixins) {
	(my $ext = $mixin) =~ s/^.*:://;	# just looking for a "cache" key, really
	$WHAT .= '_' . $ext;
    }
    $self->deb("mixin $WHAT $self") if $DEBUG & DEBUG::mixins;
    no strict 'refs';
    if (not @{$WHAT.'::ISA'}) {		# never composed this one yet?
	# fake up mixin with MI, being sure to put "roles" in front
	my $eval = "package $WHAT; use Moose ':all' => { -prefix => 'moose_' };  moose_extends('$self'); moose_with(" . join(',', map {"'$_'"} @mixins) . ");\n";
	$self->deb($eval) if $DEBUG & DEBUG::mixins;
	eval $eval;
	warn $@ if $@;
    }
    return $WHAT;
}

sub _PARAMS {}	# overridden in parametric role packages

use YAML::XS;

our %lexers;       # per language, the cache of lexers, keyed by rule name

# most cursors just copy forward the previous value of the following two items:
#has $._orig; 	       # per match, the original string we are matching against
#has StrPos $._from = 0;
#has StrPos $._to = 0;
#has StrPos $._pos = 0;
#has Cursor $._prior;

sub from { $_[0]->{_from} }
sub to { $_[0]->{_to} }
sub chars { $_[0]->{_to} - $_[0]->{_from} }
sub text { substr(${$_[0]->{_orig}}, $_[0]->{_from}, $_[0]->{_to} - $_[0]->{_from}) }
sub pos { $_[0]->{_pos} }
sub peek { $_[0]->{_peek} }
sub orig { $_[0]->{_orig} }
sub WHAT { ref $_[0] }

sub item { exists $_[0]->{''} ? $_[0]->{''} : $_[0]->text }

sub list { my $self = shift;
    my @result;
    # can't just do this in numerical order because some might be missing
    # and we don't know the max
    for my $k (keys %$self) {
	$result[$k] = $self->{$k} if $k =~ /^\d/;
    }
    \@result;
}
sub hash { my $self = shift;
    my %result;
    # can't just do this in numerical order because some might be missing
    # and we don't know the max
    for my $k (keys %$self) {
	$result{$k} = $self->{$k} if $k !~ /^[_\d]/;
    }
    \%result;
}

sub lexers { my $self = shift;
    my $lang = ref $self;
    $self->deb("LANG = $lang") if $DEBUG & DEBUG::autolexer;
    $lexers{$lang} //= {};
}

my $fakepos = 1;

sub _AUTOLEXpeek { my $self = shift;
    my $key = shift;
    my $retree = shift;

    $self->deb("AUTOLEXpeek $key") if $DEBUG & DEBUG::autolexer;
    die "Null key" if $key eq '';
#    if (my $lexer = $self->lexers->{$key}) {
#        if ($AUTOLEXED{$key}) {   # no left recursion allowed in lexer!
#            die "Left recursion in $key" if $fakepos == $AUTOLEXED{$key};
#            $self->deb("Suppressing lexer recursion on $key") if $DEBUG & DEBUG::autolexer;
#            return hash();  # (but if we advanced just assume a :: here)
#        }
#        elsif (ref($lexer) eq 'HASH') {
#            return $lexer // hash();
#        }
#        else {
#	    $self->deb(::Dump($lexer));
#            $self->deb("_AUTOLEXpeek oops\n");
#	    return;
#        }
#    }
    return $self->lexers->{$key} = $self->_AUTOLEXgen($key, $retree);
}

sub _AUTOLEXgen { my $self = shift;
    my $key = shift;
    my $retree = shift;

    my $lang = ref $self;
    $self->deb("AUTOLEXgen $key in $lang") if $DEBUG & DEBUG::autolexer;
    my $lexer = {};
    (my $dir = 'lex::' . $lang) =~ s/::/\//g;
    (my $file = $key) =~ s/::/-/g;
    $file =~ s/:\*$//;

    if (open(LEX, "$dir/$file")) {
	binmode(LEX, ":utf8");
	$self->deb("using cached $dir/$file") if $DEBUG & DEBUG::autolexer;

	my @pat = <LEX>;
	chomp(@pat);
	close LEX;

	return {"PATS" => \@pat};
    }
    else {
	{ package RE_base; 1; }
	my @pat;
	my $oldfakepos = $AUTOLEXED{$key} // 0;
	$AUTOLEXED{$key} = $fakepos;
	my $ast = $retree->{$key};
	if ($ast) {
	    @pat = $ast->longest($self->cursor_peek());
	}
	else {	# a protomethod, look up all methods it can call
	    my $proto = $key;
	    if ($proto =~ s/:\*$//) {
		my $protopat = $proto . '__S_';
		my $protolen = length($protopat);
		my $altnum = 0;
		my $peek = $self->cursor_peek();
		for my $class ($self->meta->linearized_isa) {
		    for my $method (sort $class->meta->get_method_list) {
			if (substr($method,0,$protolen) eq $protopat) {
			    my $callname = $class . '::' . $method;
			    my $peeklex = $peek->$callname();
			    if ($peeklex and $peeklex->{PATS}) {
				my @alts = @{$peeklex->{PATS}};
				for my $alt (@alts) {
				    $alt .= "\t(?#FATE)" unless $alt =~ /FATE/;
				    $alt =~ s/\(\?#FATE/(?#FATE $proto ${class}::$method/;
				    $altnum++;
				}
				push @pat, @alts;
			    }
			}
		    }
		}
	    }
	    else {
		die "BAD KEY $key";
	    }
	}
	for (@pat) {
	    s/(\t\(\?#FATE.*?\))(.*)/$2$1/;
	    s/(\(\?#::\))+/(?#::)/;
	}
	warn "(null pattern for $key)" unless @pat;
	my $pat = join("\n", @pat);

	$AUTOLEXED{$key} = $oldfakepos;

	$lexer = { PATS => [@pat] };

	if (not -d $dir) {
	    use File::Path 'mkpath';
	    mkpath($dir);
	}

	open(my $cache, '>', "$dir/$file") // die "Can't print: $!";
	binmode($cache, ":utf8");
	print $cache join("\n",@pat),"\n" or die "Can't print: $!";
	close($cache) or die "Can't close: $!";
	$self->deb("regenerated $dir/$file") if $DEBUG & DEBUG::autolexer;
	# force operator precedence method to look like a term
	if ($file eq 'expect_term') {
	    system "cp $dir/expect_term $dir/EXPR";
	}
    }
    $lexer;
}


# Can the current pattern match the current position according to 1st char?

sub canmatch {
    my ($p,$c) = @_;
    $p =~ s/\(\?#[^)]*\)//g;
    my $f = substr($p,0,1,'');
    if ($f eq '\\') {
	if ($p =~ s/^(\W)//) {
	    return 1 if $c eq $1;
	}
	elsif ($p =~ s/^(\w)//) {
	    $f .= $1;
	    if ($1 eq 'x') {
		if ($p =~ s/^([0-9a-zA-Z]{2,4})//) {
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
	else {
	    return 0;
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

sub rxlen {
    my $p = shift;
    my $len = 0;
    while ($p ne '' and $p ne '.?') {
	return -1 if $p =~ /^[*+?(|]/;
	return -1 if $p =~ /^\{[\d,]+\}/;
	$len++, next if $p =~ s/^\[\^?.[^\]]*?\]//;
	if ($p =~ s/^\\//) {
	    next if $p =~ s/^>//;
	    $len++, next if $p =~ s/^\W//;
	    $len++, next if $p =~ s/^[ntfrdswDSW]//;
	    $len++, next if $p =~ s/^\d+//;
	    $len++, next if $p =~ s/^x[\da-fA-F]{1,4}//;
	    return -1;
	}
	$len++, next if $p =~ s/^.//s;
    }
    return $len;
}

sub _AUTOLEXnow { my $self = shift;
    my $key = shift;
    my $retree = shift;

    $self->deb("AUTOLEXnow $key") if $DEBUG & DEBUG::autolexer;
    my $lexer = $self->lexers->{$key} // do {
	local %AUTOLEXED;
	$self->_AUTOLEXpeek($key,$retree);
    };
    my $buf = $self->{_orig};
    my $P = $self->{_pos};
    if ($P == length($$buf)) {
	return sub { return };
    }
    my $chr = substr($$buf,$self->{_pos},1);

    $lexer->{$chr} //= do {
	$self->deb("GENERATING $key patterns starting with '$chr'") if $DEBUG & DEBUG::autolexer;

	my @tmp =  @{$lexer->{PATS}};
	my @pats = grep { canmatch($_, $chr) } map { s/\t/.?\t/; $_; } @tmp;
	my @rxlenmemo;
	if (!@pats) {
	    $self->deb("No $key patterns start with '$chr'") if $DEBUG & DEBUG::autolexer;
	    sub { return };
	}
	else {
	    # extract fate comments before they are deleted
	    my $i = 0;
	    my $fates = [];
	    for (@pats) {
		s/\(\?#FATE (.*?)\)/(?#$i FATE $1)/ or return sub { return };
		my $fstr = $1;
		my $fate = $fates->[$i] = [0,0,0,$fstr];
		while ($fstr =~ s/(\S+)\s+(\S+)\s*//) {
		    $fate->[0] = $1;
		    $fate->[1] = $2;
		    $fate = $fate->[2] = [0,0,0,$fstr] if $fstr ne '';
		}
		$i++;
	    }

	    if ($DEBUG & DEBUG::autolexer) {
		my $tmp = "^(?:\n(" . join(")\n|(",@pats) . '))';
		$self->deb("LEXER: ", $tmp);
	    }

	    # remove stuff that will confuse TRE greatly
	    for my $pat (@pats) {
		$pat =~ s/\(\?#.*?\)//g;
		$pat =~ s/\s+//g;
		$pat =~ s/:://g;

		$pat =~ s/\\x(\w\w)/chr(hex($1))/eg;
		$pat =~ s/\\x\{(\w+)\}/chr(hex($1))/eg;
	    }

	    my $pat = "^(?:(" . join(")|(",@pats) . '))';
	    1 while $pat =~ s/\(\?:\)\??//;
	    1 while $pat =~ s/([^\\])\(((\?:)?)\)/$1($2 !!!OOPS!!! )/;
	    1 while $pat =~ s/\[\]/[ !!!OOPS!!! ]/;

	    $self->deb("TRE: ", $pat) if $DEBUG & DEBUG::autolexer;

	    $self->deb("#FATES: ", 0+@$fates) if $DEBUG & DEBUG::autolexer;

	    for my $i (0..@$fates-1) {
		$self->deb("\t", $i, ': ', $fates->[$i][3]) if $DEBUG & DEBUG::autolexer;
	    }
	    for my $pat (@pats) {
		$pat =~ s/\.\?$//;	# ltm backoff doesn't need tre workaround
	    }

	    # generate match closure at the last moment
	    sub {
		my $C = shift;

		die "orig disappeared!!!" unless length($$buf);

		return unless $lexer;

		pos($$buf) = $C->{_pos};
#		my $stoplen = -1;
#		if ($::STOP and $$buf =~ m/\G(??{$::STOP})/gc) {
#		    $stoplen = pos($$buf) - $C->{_pos};
#		    pos($$buf) = $C->{_pos};
#		    print STDERR "STOPLEN = $stoplen for $::STOP\n";
#		}

		if ($DEBUG & DEBUG::lexer) {
		    my $peek = substr($$buf,$C->{_pos},20);
		    $peek =~ s/\n/\\n/g;
		    $peek =~ s/\t/\\t/g;
		    $self->deb("looking for $key at --------->$peek");
		}

		##########################################
		# No normal p5 match/subst below here!!! #
		##########################################
		{
		    use re::engine::TRE;

		    # if trystate is defined, the "obvious" LTM failed, so must back off
		    # a parallel nfa matcher might or might not do better here...
		    # this has the advantage of being fairly compact
		    if (defined $_[0]) {
			my $tried = \${$_[0]}[0];   # vec of tried pats
			my $trylen = \${$_[0]}[1];  # next len to try
			my $rxlens = ${$_[0]}[2];   # our state's idea of rx lengths
			return if $$trylen < 0;
			if (not @$rxlens) {
			    if (@rxlenmemo) {
				@$rxlens = @rxlenmemo;
			    }
			    else {
				for my $px (0..@pats-1) {
				    $$rxlens[$px] = rxlen($pats[$px]);
				    $self->deb("Fixed len $$rxlens[$px] for $pats[$px]") if $DEBUG & DEBUG::fixed_length;
				}
				@rxlenmemo = @$rxlens;
			    }
			}
			my @result;
			while (not @result and $$trylen >= 0) {
			    for my $px (0..@pats-1) {
				next if vec($$tried,$px,1);	# already tried this one
				my $l = $$rxlens[$px];
				if ($l == -1) {
				    my $p = '^' . $pats[$px];
				    if (($$buf =~ m/$p/xgc)) {
					$$rxlens[$px] = $l = $+[0] - $-[0];
					if ($l == $$trylen) {
					    push @result, $fates->[$px];
					    vec($$tried,$px,1) = 1;
					}
					next;
				    }
				    else {	# pattern doesn't match at all, invalidate
					vec($$tried,$px,1) = 1;
					next;
				    }
				}
				if ($l == $$trylen) {
				    # already known to match if null or variable length
				    if (not $l or $rxlenmemo[$px] < 0) {
					push @result, $fates->[$px];
				    }
				    else {
					my $p = '^' . $pats[$px];
					if ($$buf =~ m/$p/xgc) {
					    push @result, $fates->[$px];
					}
				    }
				    vec($$tried,$px,1) = 1;	# mark this one tried
				}
			    }
			    --$$trylen;
			}

			return @result;
		    }

		    $self->deb("/ running tre match at @{[ pos($$buf) ]} /") if $DEBUG & DEBUG::lexer;

		    if (($$buf =~ m/$pat/xgc)) {	# XXX does this recompile $pat every time?
			my $max = @+ - 1;
			my $last = @- - 1;	# ignore '$0'
#		        $self->deb("LAST: $last\n");
			my $result = $fates->[$last-1] // return;
			for my $x (1 .. $max) {
			    my $beg = $-[$x];
			    next unless defined $beg;
			    my $end = $+[$x];
#			    return if $stoplen >= $end - $beg;
			    my $f = $fates->[$x-1][3];
			    no strict 'refs';
			    if ($DEBUG & DEBUG::fates or ($DEBUG & DEBUG::lexer and $x == $last)) {
				my $p = $pats[$x] // '<nopat>';
				$self->deb("\$$x: $beg..$end\t$$x\t ",
				    $x == $last ? "====>" : "---->",
				    " $f\t/$p/");
			    }
			}
#			$self->deb("success at '", substr($$buf,$C->{_pos},10), "'") if $DEBUG & DEBUG::lexer;
			my $tried = "";
			vec($tried,$last-1,1) = 1;
			$_[0] = [$tried, $+[0] - $-[0], []];
			$result;
		    }
		    else {
			$self->deb("NO LEXER MATCH") if $DEBUG & DEBUG::lexer;
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
    $self->deb("cursor_peek") if $DEBUG & DEBUG::cursors;
    my %r = %$self;
    $r{_peek} = 1;
    bless \%r, ref $self;
}

sub cursor_fresh { my $self = shift;
    my %r;
    my $lang = @_ && $_[0] ? shift() : ref $self;
    $self->deb("cursor_fresh lang $lang") if $DEBUG & DEBUG::cursors;
    $r{_orig} = $self->{_orig};
    $r{_to} = $r{_from} = $r{_pos} = $self->{_pos};
    $r{_fate} = $self->{_fate};
    $r{ws_to} = $self->{ws_to};
    $r{ws_from} = $self->{ws_from};
    bless \%r, $lang;
}

sub cursor_bind { my $self = shift;	# this is parent's match cursor
    my $bindings = shift;
    my $submatch = shift;		# this is the submatch's cursor

    $self->deb("cursor_bind @$bindings") if $DEBUG & DEBUG::cursors;
    my %r = %$self;
    if ($bindings) {
	for my $binding (@$bindings) {
	    if (ref $r{$binding} eq 'ARRAY') {
		push(@{$r{$binding}}, $submatch);
	    }
	    else {
		$r{$binding} = $submatch;
	    }
	}
    }
    $r{_pos} = $r{_to} = $submatch->{_to};
    delete $r{_fate};
    bless \%r, ref $self;		# return new match cursor for parent
}

sub cursor_fate { my $self = shift;
    my $pkg = shift;
    my $name = shift;
    my $retree = shift;
    # $_[0] is now ref to a $trystate;

    $self->deb("cursor_fate $pkg $name") if $DEBUG & DEBUG::cursors;
    my %r = %$self;
    my $tag;
    my $try;
    my $relex;
    
    my $fate = $self->{_fate};
    if ($fate and $fate->[0] eq $name) {
        $self->deb("Fate passed to $name: $$fate[3]") if $DEBUG & DEBUG::fates;
        ($tag, $try, $fate) = @$fate;
	$r{_fate} = $fate;
    }
    else {
        $relex = $self->_AUTOLEXnow($name,$retree);
	$fate = $relex->($self,$_[0]);
        if ($fate) {
            $self->deb("FATE OF ${pkg}::$name: $$fate[3]") if $DEBUG & DEBUG::fates;
            ($tag, $try, $fate) = @$fate;
	    $r{_fate} = $fate;
        }
        else {
            $self->deb("NO FATE FOR ${pkg}::$name (will probe)") if $DEBUG & DEBUG::fates;
            $tag = '';
        }
    }
    return (bless \%r, ref $self), $tag, $try, $relex;
}

sub cursor_all { my $self = shift;
    my $fpos = shift;
    my $tpos = shift;

    $self->deb("cursor_all from $fpos to $tpos") if $DEBUG & DEBUG::cursors;
    my %r = %$self;
    $r{_from} = $fpos;
    $r{_to} = $tpos;
    $r{_pos} = $tpos;

    bless \%r, ref $self;
}

sub cursor { my $self = shift;
    my $tpos = shift;

    $self->deb("cursor to $tpos") if $DEBUG & DEBUG::cursors;
    my %r = %$self;
    $r{_from} = $self->{_pos} // 0;
    $r{_to} = $tpos;
    $r{_pos} = $tpos;

    bless \%r, ref $self;
}

sub cursor_rev { my $self = shift;
    my $fpos = shift;

    $self->deb("cursor_rev back to $fpos") if $DEBUG & DEBUG::cursors;
    my %r = %$self;
    $r{_pos} = $fpos;
    $r{_from} = $fpos;
    $r{_to} = $self->{_from};

    bless \%r, ref $self;
}

sub callm { my $self = shift;
    my $arg = shift;

    my $lvl = 0;
    my @subs;
    if ($DEBUG & DEBUG::callm_show_subnames) {
	while (my @c = caller($lvl)) { $lvl++; (my $s = $c[3]) =~ s/^.*:://; $s =~ s/^__ANON//; push @subs, $s; }
	splice(@subs, 0, 2);
    }
    else {
	while (my @c = caller($lvl)) { $lvl++; }
    }
    my ($package, $file, $line, $subname, $hasargs) = caller(1);
    my $name = $subname;
    if (defined $arg) { 
        $name .= " " . $arg;
    }
    my $pos = '?';
    $self->deb($name, " [", $file, ":", $line, "] @subs") if $DEBUG & DEBUG::trace_call;
    $lvl;
}

sub retm { my $self = shift;
    return $self unless $DEBUG & DEBUG::trace_call;
    warn "Returning non-Cursor: $self\n" unless exists $self->{_pos};
    my ($package, $file, $line, $subname, $hasargs) = caller(1);
    $self->deb($subname, " returning @{[$self->{_from}]}..@{[$self->{_to}]}");
    $self;
}

sub _MATCHIFY { my $self = shift;
    my @result = map { $_->{_from} = $self->{_from}; $_->retm() } @_;
    if (wantarray) {
	@result;
    }
    else {
	$result[0];
    }
}

sub _STARf { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;

    map { $_->retm() }
        $self->cursor($self->{_pos}),
        $self->_PLUSf($block);
}

sub _STARg { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;

    map { $_->retm() } reverse
            $self->cursor($self->{_pos}),
            $self->_PLUSf($block);
}

sub _STARr { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
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
    $self->cursor($to->{_pos})->retm();
}

sub _PLUSf { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
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

    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;

    reverse $self->_PLUSf($block, @_);
}

sub _PLUSr { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
    my $to = $self;
    my @all;
    my $eos = length(${$self->{_orig}});
    for (;;) {
      last if $to->{_pos} == $eos;
	my @matches = $block->($to);  # XXX shouldn't read whole list
      last unless @matches;
	my $first = $matches[0];  # no backtracking into block on ratchet
	#$first->deb($matches->perl) if $DEBUG;
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

    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
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

    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;

    reverse $self->_REPSEPf($sep, $block, @_);
}

sub _REPSEPr { my $self = shift;
    my $sep = shift;
    my $block = shift;

    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
    my $to = $self;
    my @all;
    my $eos = length(${$self->{_orig}});
    for (;;) {
      last if $to->{_pos} == $eos;
	my @matches = $block->($to);  # XXX shouldn't read whole list
      last unless @matches;
	my $first = $matches[0];  # no backtracking into block on ratchet
	#$first->deb($matches->perl) if $DEBUG;
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

    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;

    my $x = ($block->($self))[0];
    my $r = $x // $self->cursor($self->{_pos});
    $r->retm();
}

sub _OPTg { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
    my @x = $block->($self);
    map { $_->retm() }
        $block->($self),
        $self->cursor($self->{_pos});
}

sub _OPTf { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
    map { $_->retm() }
        $self->cursor($self->{_pos}),
        $block->($self);
}

sub _BRACKET { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
    map { $_->retm() }
        $block->($self);
}

sub _PAREN { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
    map { $_->retm() }
        $block->($self);
}

sub _NOTBEFORE { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
    my @all = $block->($self);
    return () if @all;  # XXX loses continuation
    return $self->cursor($self->{_pos})->retm();
}

sub _NOTCHAR { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
    my @all = $block->($self);
    return () if @all;  # XXX loses continuation
    return $self->cursor($self->{_pos}+1)->retm();
}

sub before { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
    my @all = $block->($self);
    if (@all and $all[0]) {
        return $all[0]->cursor_all(($self->{_pos}) x 2)->retm();
    }
    return ();
}

sub after { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
    my $end = $self->cursor($self->{_pos});
    my @all = $block->($end);          # Make sure $_->{_from} == $_->{_to}
    if (@all and $all[0]) {
        return $all[0]->cursor_all(($self->{_pos}) x 2)->retm();
    }
    return ();
}

sub null { my $self = shift;
    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
    return $self->cursor($self->{_pos})->retm();
}

## token ws
##      token ws {
##          :my @stub = return self if self.pos === $!ws_to; # really fast memoizing
##          [
##          || <?after \w> <?before \w> ::: <!>        # must \s+ between words
##          || { $!ws_from = $¢.pos } \s* { $!ws_to = $¢.pos }
##          ]
##      }

sub ws_from { $_[0]->{ws_from} }
sub ws_to { $_[0]->{ws_to} }

sub ws {
    my $self = shift;
    my @stub = return $self if $self->pos == $self->{ws_to};

    local $CTX = $self->callm() if $DEBUG & DEBUG::trace_call;
    if ($self->{_peek}) {
        return;
    }

    my $C = $self;

    $self->_MATCHIFY(
        $C->_BRACKET( sub { my $C=shift;
            do { my @gather;
                    push @gather, (map { my $C=$_;
                        (map { my $C=$_;
                            (map { my $C=$_;
                                $C->_NOTBEFORE( sub { my $C=shift;
                                    $C
                                })
                            } $C->_COMMITRULE())
                        } $C->before(sub { my $C=shift;
                            $C->_ALNUM()
                        }))
                    } $C->before( sub { my $C=shift;
                        $C->after(sub { my $C=shift;
                            $C->_ALNUM_rev()
                        })
                    }))
                    or
                    push @gather, (map { my $C=$_;
                        (map { my $C=$_;
                            scalar(do { $self->{ws_to} = $C->{_pos} }, $C)
                        } $C->_STARr(sub { my $C=shift;
                            $C->_SPACE()
                        }))
                    } scalar(do { $self->{ws_from} = $C->{_pos} }, $C));
              @gather;
            }
        })
    );
}

sub _ASSERT { my $self = shift;
    my $block = shift;

    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
    my @all = $block->($self);
    if ((@all and $all[0]->{_bool})) {
        return $self->cursor($self->{_pos})->retm();
    }
    return ();
}

sub _BINDVAR { my $self = shift;
    my $var = shift;
    my $block = shift;

    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
    map { $$var = $_; $_->retm() }  # XXX doesn't "let"
        $block->($self);
}

sub _SUBSUME { my $self = shift;
    my $names = shift;
    my $block = shift;

    local $CTX = $self->callm($names ? "@$names" : "") if $DEBUG & DEBUG::trace_call;
    map { $self->cursor_bind($names, $_)->retm() }
        $block->($self);
}

sub _EXACT { my $self = shift;
    my $s = shift;

    local $CTX = $self->callm($s) if $DEBUG & DEBUG::trace_call;
    my $P = $self->{_pos} // 0;
    my $len = length($s);
    my $buf = $self->{_orig};
    if (substr($$buf, $P, $len) eq $s) {
        $self->deb("EXACT $s matched @{[substr($$buf,$P,$len)]} at $P $len") if $DEBUG & DEBUG::matchers;
        my $r = $self->cursor($P+$len);
        $r->retm();
    }
    else {
        $self->deb("EXACT $s didn't match @{[substr($$buf,$P,$len)]} at $P $len") if $DEBUG & DEBUG::matchers;
        return ();
    }
}

sub _SYM { my $self = shift;
    my $s = shift;

    local $CTX = $self->callm($s) if $DEBUG & DEBUG::trace_call;
    my $P = $self->{_pos} // 0;
    my $len = length($s);
    my $buf = $self->{_orig};
    if (substr($$buf, $P, $len) eq $s) {
        $self->deb("SYM $s matched @{[substr($$buf,$P,$len)]} at $P $len") if $DEBUG & DEBUG::matchers;
        my $r = $self->cursor($P+$len);
	$r->{sym} = $s;
        $r->retm();
    }
    else {
        $self->deb("SYM $s didn't match @{[substr($$buf,$P,$len)]} at $P $len") if $DEBUG & DEBUG::matchers;
        return ();
    }
}

sub _EXACT_rev { my $self = shift;
    my $s = shift;

    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
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
    local $CTX = $self->callm(0+@_) if $DEBUG & DEBUG::trace_call;
    my $P = $self->{_pos} // 0;
    my $buf = $self->{_orig};
    my @array = sort { length($b) <=> length($a) } @_;	# XXX suboptimal
    my @result = ();
    for my $s (@array) {
	my $len = length($s);
	if (substr($$buf, $P, $len) eq $s) {
	    $self->deb("ARRAY elem $s matched @{[substr($$buf,$P,$len)]} at $P $len") if $DEBUG & DEBUG::matchers;
	    my $r = $self->cursor($P+$len);
	    push @result, $r->retm('');
	}
    }
    return @result;
}

sub _ARRAY_rev { my $self = shift;
    local $CTX = $self->callm(0+@_) if $DEBUG & DEBUG::trace_call;
    my $buf = $self->{_orig};
    my @array = sort { length($b) <=> length($a) } @_;	# XXX suboptimal
    my @result = ();
    for my $s (@array) {
	my $len = length($s);
	my $from = $self->{_from} = $len;
	if (substr($$buf, $from, $len) eq $s) {
	    $self->deb("ARRAY_rev elem $s matched @{[substr($$buf,$from,$len)]} at $from $len") if $DEBUG & DEBUG::matchers;
	    my $r = $self->cursor_rev($from);
	    push @result, $r->retm('');
	}
    }
    return @result;
}

sub _DIGIT { my $self = shift;
    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
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
    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
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
    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
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
    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
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
    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
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

sub alpha_rev { my $self = shift;
    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
    my $from = $self->{_from} - 1;
    if ($from < 0) {
        return ();
    }
    my $buf = $self->{_orig};
    my $char = substr($$buf, $from, 1);
    if ($char =~ /^[a-z_A-Z]$/) {
        my $r = $self->cursor_rev($from);
        return $r->retm();
    }
    else {
        return ();
    }
}

sub _SPACE { my $self = shift;
    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
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
    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
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
    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
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
    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
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
    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
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
    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
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

    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
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

    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
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
    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
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

sub _ANY_rev { my $self = shift;
    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
    my $from = $self->{_from} - 1;
    if ($from < 0) {
        return ();
    }
    return $self->cursor_rev($from)->retm();
}

sub _BOS { my $self = shift;
    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
    my $P = $self->{_pos};
    if ($P == 0) {
        $self->cursor($P)->retm();
    }
    else {
        return ();
    }
}
sub _BOS_rev { $_[0]->_BOS }

sub _BOL { my $self = shift;
    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
    my $P = $self->{_pos};
    my $buf = $self->{_orig};
    if ($P == 0 or substr($$buf, $P-1, 1) =~ /^[\n\f\x0b\x{2028}\x{2029}]$/) {
        $self->cursor($P)->retm();
    }
    else {
        return ();
    }
}
sub _BOL_rev { $_[0]->_BOL }

sub _EOS { my $self = shift;
    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
    my $P = $self->{_pos};
    my $buf = $self->{_orig};
    if ($P == length($$buf)) {
        $self->cursor($P)->retm();
    }
    else {
        return ();
    }
}
sub _EOS_rev { $_[0]->_EOS }

sub _EOL { my $self = shift;
    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
    my $P = $self->{_pos};
    my $buf = $self->{_orig};
    if ($P == length($$buf) or substr($$buf, $P, 1) =~ /^(?:\r\n|[\n\f\x0b\x{2028}\x{2029}])$/) {
        $self->cursor($P)->retm();
    }
    else {
        return ();
    }
}
sub _EOL_rev { $_[0]->_EOL }

sub _RIGHTWB { my $self = shift;
    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
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
sub _RIGHTWB_rev { $_[0]->_RIGHTWB }

sub _LEFTWB { my $self = shift;
    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
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
sub _LEFTWB_rev { $_[0]->_LEFTWB }

sub _REDUCE { my $self = shift;
    my $tag = shift;

    local $CTX = $self->callm($tag) if $DEBUG & DEBUG::trace_call;
    my $P = $self->{_pos};
    my $F = $self->{_from};
    $self->{_reduced} = $tag;
    $self->deb("REDUCE $tag from $F to $P") if $DEBUG & DEBUG::matchers;
#    $self->whats;
    $self;
#    $self->cursor($P);
}

sub _COMMITBRANCH { my $self = shift;
    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
    my $P = $self->{_pos};
    $self->deb("Commit branch to $P") if $DEBUG & DEBUG::matchers;
#    $self->cursor($P);  # XXX currently noop
    $self;
}

sub _COMMITRULE { my $self = shift;
    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
    my $P = $self->{_pos};
    $self->deb("Commit rule to $P") if $DEBUG & DEBUG::matchers;
#    $self->cursor($P);  # XXX currently noop
    $self;
}

sub commit { my $self = shift;
    local $CTX = $self->callm if $DEBUG & DEBUG::trace_call;
    my $P = $self->{_pos};
    $self->deb("Commit match to $P") if $DEBUG & DEBUG::matchers;
#    $self->cursor($P);  # XXX currently noop
    $self;
}

sub fail { my $self = shift;
    my $m = shift;
    return ();
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
	return unless $DEBUG & DEBUG::longest_token_pattern_generation;
	my $arg = shift;
	my $lvl = 0;
	while (caller($lvl)) { $lvl++ }
        my ($package, $file, $line, $subname, $hasargs) = caller(0);

	my $name = $package;   # . '::' . substr($subname,1);
	if (defined $arg) { 
	    $name .= " " . $arg;
	}
	::deb("\t", ':' x $lvl, ' ', $name, " [", $file, ":", $line, "]") if $DEBUG & DEBUG::longest_token_pattern_generation;
    }
}

{ package REbase;
    sub longest { my $self = shift; my ($C) = @_;  ::here("UNIMPL @{[ref $self]}"); "$self" }
}

{ package RE; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        ::here();
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
		$C->deb("\n",::Dump($self)) unless $re;
                if (ref($re) eq 'RE_method_re' and $re->{'name'} eq 'before') {
                    my @result = $re->longest($C);
                    return map { $_ . $IMP } @result;
                }
            }
        }
        return '';
    }
}

{ package RE_assertvar; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        return $IMP;
    }
}

{ package RE_block; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        return $IMP;
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
	    $fixed = $1 . $IMP;
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
                return '[\\x20\\x09\\x0d]';
	    }
            elsif ($_ eq '\\v') {
                return '[\\x0a\\x0c]';
            }
            elsif ($_ eq '\\N') {
                return '[^\\x0a]';
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
                return $IMP;
            }
            else {
                return $text;
            }
        }
    }
}

{ package RE_method; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        my $name = $self->{'name'};
	return $IMP if $self->{'rest'};
	Encode::_utf8_on($name);
        ::here($name);
        for (scalar($name)) { if ((0)) {}
            elsif ($_ eq 'null') {
                return;
            }
            elsif ($_ eq '') {
                return $IMP;
            }
            elsif ($_ eq 'ws') {
                return $IMP;
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
		    return $IMP;
		}
	    }
	    my $lexer;
	    {
		local $PREFIX = "";
		$lexer = eval { $C->cursor_peek->$name() };
	    }
	    return $IMP unless $lexer;
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
        return $IMP;
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
                return $IMP;
            }
            elsif ($_ eq 'after') {
                return;
            }
            elsif ($_ eq 'before') {
                my @result = $re->longest($C);
                return map { $_ . $IMP } @result;
            }
            else {
                my $lexer = $C->cursor_peek->$name($re);
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
                return $IMP;
            }
            else {
                my $lexer = $C->cursor_peek->$name($str);
		my @pat = @{$lexer->{PATS}};
		return '' unless @pat;
		return @pat;
            }
        }
    }
}

{ package RE_noop; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        return;
    }
}

{ package RE_every; our @ISA = 'RE_base';
    sub longest { my $self = shift; my ($C) = @_; 
        return $IMP;
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
        $C->deb(join("\n",@result)) if $DEBUG & DEBUG::longest_token_pattern_generation;
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
		return map { $_ . $IMP } @atom;
	    }
            return "$atom+";
        }
        elsif ($self->{'quant'}[0] eq '*') {
            $fakepos = $oldfakepos;
	    if (@atom > 1) {
		return map { $_ . $IMP } @atom,'';
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
		return $atom . $IMP;
	    }
        }
	return $IMP;
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
	my $result = [''];
	my $c = $self->{'zyg'};
	my @chunks = @$c;
	::here(0+@chunks);
	local $PREFIX = $PREFIX;

	for my $chunk (@chunks) {
	    # ignore negative lookahead
	    next if ref($chunk) eq 'RE_assertion' and $chunk->{assert} eq '!';
	    $C->deb("NULLABLE ".ref($chunk)) if $DEBUG & DEBUG::longest_token_pattern_generation and not $chunk->{min};
	    my @newalts = $chunk->longest($C);
	    last unless @newalts;
#	    if (not $chunk->{min} and $next[-1] ne '') {
#		push(@next, '');	# install bypass around nullable atom
#	    }
	    my $newresult = [];
	    my $pure = 0;
	    for my $oldalt (@$result) {
		if ($oldalt =~ /\(\?#::\)/) {
		    push(@$newresult, $oldalt);
		    next;
		}

		for my $newalt (@newalts) {
		    $pure = 1 unless $newalt =~ /\(\?#::\)/;
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
	    last unless $pure;	# at least one alternative was pure
	    # ignore everything after positive lookahead
	    last if ref($chunk) eq 'RE_assertion';
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
        return $IMP;
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
        $C->deb(join("\n", @result)) if $DEBUG & DEBUG::longest_token_pattern_generation;
        $fakepos = $minfakepos;  # Did all branches advance?
        @result;
    }
}

{ package RE_var; our @ISA = 'RE_base';
    #method longest ($C) { ... }
    sub longest { my $self = shift; my ($C) = @_; 
	my $var = $self->{var};
	if (my $p = $C->_PARAMS) {
	    my $text = $p->{$var} || return $IMP;
	    $fakepos++ if length($text);
	    $text = ::qm($text);
	    return $text;
	}
        return $IMP;
    }
}


