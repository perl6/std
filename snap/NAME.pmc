# NAME.pmc
#
# Copyright 2007-2010, Larry Wall
#
# You may copy this software under the terms of the Artistic License,
#     version 2.0 or later.

use 5.010;
package NAME;
sub new {
    my $class = shift;
    my %attrs = @_;
    bless \%attrs, $class;
}
sub name  { my $self = shift; return $self->{name} };
sub file  { my $self = shift; return $self->{file} };
sub line  { my $self = shift; return $self->{line} };
sub xlex  { my $self = shift; return $self->{xlex} };
sub olex  { my $self = shift; return $self->{olex} };
sub of    { my $self = shift; return $self->{of} };
1;
