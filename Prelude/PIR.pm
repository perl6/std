#module Prelude::PIR-0.0.1;
# XXX -- for some reason, compilation doesn't work if the above line is uncommented.

# our &prefix:<?> := &true doesn't work yet.
sub prefix:<?> ($var) { true $var }

sub chomp (Str $str is rw) {
    if substr($str, -1, 1) eq "\n" {
        $str = substr $str, 0, chars($str) - 1;
        "\n";
    } else {
        undef;
    }
}

sub chop (Str $str is rw) {
    if chars($str) == 0 {
        undef;
    } else {
        my $removed = substr $str, -1, 1;
        $str = substr $str, 0, chars($str) - 1;
        $removed;
    }
}
