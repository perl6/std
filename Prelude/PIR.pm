#module Prelude::PIR-0.0.1;
# XXX -- for some reason, compilation doesn't work if the above line is uncommented.

# our &prefix:<?> := &true doesn't work yet.
sub prefix:<?> ($var) returns Bool { true $var }

sub chomp (Str $str is rw) returns Str {
    if substr($str, -1, 1) eq "\n" {
        $str = substr $str, 0, chars($str) - 1;
        "\n";
    } else {
        undef;
    }
}

sub chop (Str $str is rw) returns Str {
    if chars($str) == 0 {
        undef;
    } else {
        my $removed = substr $str, -1, 1;
        $str = substr $str, 0, chars($str) - 1;
        $removed;
    }
}

sub sleep (Num $seconds) returns Num {
    my $time = time;
    Pugs::Internals::sleep $seconds;
    my $seconds_slept = time() - $time;
    $seconds_slept;
}

sub exit (Int ?$status = 0) {
    Pugs::Internals::exit $status;
}

sub pi () returns Num {
    3.14159265358979323846264338327950288419716939937510;
}

sub lcfirst (Str $str) returns Str {
    lc(substr $str, 0, 1) ~ substr $str, 1, chars($str) - 1;
}

sub ucfirst (Str $str) returns Str {
    uc(substr $str, 0, 1) ~ substr $str, 1, chars($str) - 1;
}
