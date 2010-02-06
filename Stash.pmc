# Stash.pmc
#
# Copyright 2009-2010, Larry Wall
#
# You may copy this software under the terms of the Artistic License,
#     version 2.0 or later.

use 5.010;
package Stash;
sub new {
    my $class = shift;
    my %attrs = @_;
    bless \%attrs, $class;
}
sub idref    { return $_[0]->{'!id'} };
sub id    { return $_[0]->{'!id'}->[0] // '???' };
sub file  { return $_[0]->{'!file'} };
sub line  { return $_[0]->{'!line'} };
1;
