package LazyMap;
use strict;
use warnings;
no warnings 'recursion';

use Exporter;

our @ISA = 'Exporter';

our @EXPORT = qw(lazymap eager);

our $AUTOLOAD;

sub AUTOLOAD {
    (my $meth = $AUTOLOAD) =~ s/.*:://;
    return if $meth eq 'DESTROY';
    print STDERR "AUTOLOAD $meth\n";
    my $self = shift;
    if (my ($eager) = $self->iter) {
	return $eager->$meth(@_), $self;
    }
    return ();
}

use overload 'bool' => 'true';

sub new {
    my $class = shift;
    my $block = shift;
    return bless { 'B' => $block, 'C' => [], 'L' => [@_] }, $class;
}

sub lazymap (&@) {
    my $block = shift;
    return () unless @_;
    my $lazy = bless { 'B' => $block, 'C' => [], 'L' => [@_] }, 'LazyMap';
    if (wantarray) {
	if (my @retval = iter($lazy)) {
	    push @retval, $lazy if @{$lazy->{C}} || @{$lazy->{L}};
	    return @retval;
	}
	return;
    }
    else {
	$lazy;
    }
}

sub iter {
    my $self = shift;
    my $lazies = $self->{L};
    my $called = $self->{C};
    while (not @$called) {
	return () unless @$lazies;
	my $lazy = $$lazies[0];
	if (ref($lazy) =~ /^Lazy/) {
	    my $todo = $lazy->iter;
	    if (defined $todo) {
		@$called = $self->{B}->($todo);
	    }
	    else {
		shift @$lazies;
	    }
	}
	elsif (defined $lazy) {
	    @$called = $self->{B}->(shift @$lazies);
	}
	else {
	    shift @$lazies;
	}
    }
    while (ref($$called[0]) =~ /^Lazy/) {
	my $really = $$called[0]->iter;
	if ($really) {
	    return $really;
	}
	else {
	    shift @$called;
	}
    }
    return shift @$called if @$called;
    return ();
}

sub true {
    my $self = shift();
    my $called = $self->{C};
    return 1 if @$called;
    my $lazies = $self->{L};
    return 0 unless @$lazies;
    return 0 unless my ($c) = $self->iter;
    unshift(@$called, $c);
    return 1;
}

sub eager {
    my @out;
    while (@_) {
	my $head = shift;
	if (ref($head) eq 'LazyMap') {	# don't unroll LazyConst
	    while (my ($next) = $head->iter) {
		push @out, $next;
	    }
	}
	else {
	    push @out, $head;
	}
    }
#    print STDERR ::Dump(@out);
    @out;
}

{ package LazyConst;
    sub new {
	my $self = shift;
	bless { 'K' => shift }, 'LazyConst';
    }
    sub true {
	1;
    }
    sub iter { $_[0]->{K} }
}

{ package LazyRange;
    sub new {
	my $class = shift;
	my $start = shift;
	my $end = shift;
	bless { 'N' => $start, 'E' => $end }, $class;
    }
    sub true {
	1;
    }
    sub iter {
	my $self = shift;
	if ((my $n = $self->{N}++) <= $self->{E}) {
	    $n;
	}
	else {
	    ();
	}
    }
}

{ package LazyRangeRev;
    sub new {
	my $class = shift;
	my $start = shift;
	my $end = shift;
	bless { 'N' => $start, 'E' => $end }, $class;
    }
    sub true {
	1;
    }
    sub iter {
	my $self = shift;
	if ((my $n = $self->{N}--) >= $self->{E}) {
	    $n;
	}
	else {
	    ();
	}
    }
}

1;
