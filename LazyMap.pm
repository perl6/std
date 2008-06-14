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
    return bless { 'B' => $block, 'E' => [], 'L' => [@_] }, $class;
}

sub lazymap (&@) {
    my $block = shift;
    my $lazy = bless { 'B' => $block, 'E' => [], 'L' => [@_] }, 'LazyMap';
    if (wantarray) {
	if (my @eager = $lazy->iter) {
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
    my $eagers = $self->{E};
    return shift @$eagers if @$eagers;
    my $lazies = $self->{L};
    while (@$lazies) {
	if (ref($$lazies[0]) ne 'LazyMap') {
	    return $self->{B}->(shift @$lazies);
	}
	my $next = $$lazies[0]->iter;
	if (ref $next eq 'LazyMap') {
	    unshift(@$lazies, $next);
	}
	else {
	    @$eagers = $self->{B}->($next);
	    return shift @$eagers if @$eagers;
	}
    }
    return ();
}

sub true {
    my $self = shift();
    my $eagers = $self->{E};
    return 1 if @$eagers;
    my $lazies = $self->{L};
    return 0 unless @$lazies;
    return 0 unless my ($e) = $self->iter;
    unshift(@$eagers, $e);
    return 1;
}

1;
