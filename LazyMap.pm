package LazyMap;
use strict;
use warnings;

use Exporter;

our @ISA = 'Exporter';

our @EXPORT = 'lazymap';

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
    my $lazy = bless { 'B' => $block, 'C' => [], 'L' => [@_] }, 'LazyMap';
    if (wantarray) {
	if (my @eager = iter($lazy)) {
	    return @eager, $lazy;
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
	if (ref($lazy) eq 'LazyMap') {
	    my $todo = $lazy->iter;
	    if ($todo) {
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
    while (ref($$called[0]) eq 'LazyMap') {
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

1;
