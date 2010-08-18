#! /usr/bin/env perl
use 5.012;  # unicore format is unstable
use strict;
use warnings;
use autodie;

my @tables = (qw|Gc/N Gc/L Perl/Blank Space/Y Perl/VertSpac|);

open DUMP, ">", "uniprops";
binmode DUMP;

for my $propname (@tables) {
    my @top = (("\0" x 128) x 1088);
    my $used = "\0" x 136;
    for my $l (split("\n", (do "unicore/lib/$propname.pl"))) {
        my ($from, $to) = split("\t", $l);
        $from = hex $from;
        $to = hex $to || $from;

        for (my $x = $from; $x <= $to; $x++) {
            vec($top[$x >> 10], $x & 1023, 1) = 1;
            vec($used, $x >> 10, 1) = 1;
        }
    }

    print DUMP chr(length($propname));
    print DUMP $propname;
    print DUMP $used;
    for (my $i = 0; $i < 1088; $i++) {
        if (vec($used, $i, 1)) {
            print DUMP $top[$i];
        }
    }
}

close DUMP;
