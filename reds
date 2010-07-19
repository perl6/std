#!/usr/bin/env perl

use FindBin;
BEGIN { unshift @INC, $FindBin::Bin if -s "$FindBin::Bin/STD.pmc"; }

use strict;
use warnings;

use STD;
use utf8;
use feature 'say';

$::ACTIONS = 'Actions';

sub MAIN {
    my $output = 'ast';

    STD->parsefile($_[0]);
}

###################################################################

{ package Actions;

    our $AUTOLOAD;

    sub AUTOLOAD {
	my $self = shift;
	my $C = shift;
	my $F = $C->{_from};
	my $P = $C->{_pos};
	$AUTOLOAD =~ s/^Actions:://;
	say "$AUTOLOAD $F $P";
    }
}

MAIN(@ARGV);
