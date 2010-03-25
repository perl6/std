package LazyMap;
use 5.010;

# LazyMap.pm
#
# Copyright 2007-2010, Larry Wall
#
# You may copy this software under the terms of the Artistic License,
#     version 2.0 or later.

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
    return bless { 'B' => $block, 'C' => [], 'L' => [@_], 'N' => 0 }, $class;
}

sub lazymap (&@) {
    my $block = shift;
    return () unless @_;
    my $lazy = bless { 'B' => $block, 'C' => [], 'L' => [@_], 'N' => 0 }, 'LazyMap';
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
    while (@$called or @$lazies) {
	# pull from lazy list only when forced to
	while (not @$called) {
	    return () unless @$lazies;
	    my $lazy = $$lazies[0];
	    # recursive lazies?  delegate to lower ->iter
	    if (ref($lazy) =~ /^Lazy/) {
		my $todo = $lazy->iter;
		if (defined $todo) {
		    @$called = $self->{B}->($todo);
		}
		else {
		    shift @$lazies;
		}
	    }
	    elsif (defined $lazy) { # just call our own block
		@$called = $self->{B}->(shift @$lazies);
	    }
	    else { # undef snuck into the list somehow
		shift @$lazies;
	    }
	}

	# evaluating the blocks may have returned something lazy, so delegate again
	while (@$called and ref($$called[0]) =~ /^Lazy/) {
	    my $really = $$called[0]->iter;
	    if ($really) {
		unshift @$called, $really;
	    }
	    else {
		shift @$called;
	    }
	}

	# finally have at least one real cursor, grep for first with live transaction
	while (@$called and ref($$called[0]) !~ /^Lazy/) {
	    my $candidate = shift @$called;
	    # make sure its transaction doesn't have a prior commitment
	    my $xact = $candidate->{_xact};
	    my $n = $self->{N}++;
	    return $candidate unless $xact->[-2] and $n;
	}
    }
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
	my $xact = shift;
	bless { 'K' => shift, 'X' => $xact }, 'LazyConst';
    }
    sub true {
	1;
    }
    sub iter {
	return () if $_[0]->{X}->[-2];
	$_[0]->{K};
    }
}

{ package LazyRange;
    sub new {
	my $class = shift;
	my $xact = shift;
	my $start = shift;
	my $end = shift;
	bless { 'N' => $start, 'E' => $end, 'X' => $xact }, $class;
    }
    sub true {
	1;
    }
    sub iter {
	my $self = shift;
	if ($self->{X}->[-2]) {
	    ()
	}
	elsif ((my $n = $self->{N}++) <= $self->{E}) {
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
	my $xact = shift;
	my $start = shift;
	my $end = shift;
	bless { 'N' => $start, 'E' => $end, 'X' => $xact }, $class;
    }
    sub true {
	1;
    }
    sub iter {
	my $self = shift;
	if ($self->{X}->[-2]) {
	    ()
	}
	elsif ((my $n = $self->{N}--) >= $self->{E}) {
	    $n;
	}
	else {
	    ();
	}
    }
}

1;
