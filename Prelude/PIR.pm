#module Prelude::PIR-0.0.1;
# XXX -- for some reason, compilation doesn't work if the above line is uncommented.

# our &prefix:<?> := &true doesn't work yet.
sub prefix:<?> ($var) returns Bool is builtin is primitive { true $var }

sub chomp (Str $str) returns Str is builtin is primitive {
    # XXX return $str but newline("\n")
    if substr($str, -1, 1) eq "\n" {
        substr $str, 0, chars($str) - 1;
    } else {
        $str;
    }
}

sub chop (Str $str is rw) returns Str is builtin is primitive {
    if chars($str) == 0 {
        undef;
    } else {
        my $removed = substr $str, -1, 1;
        $str = substr $str, 0, chars($str) - 1;
        $removed;
    }
}

sub sleep (Num $seconds) returns Num is builtin is primitive {
    my $time = time;
    Perl6::Internals::sleep $seconds;
    my $seconds_slept = time() - $time;
    $seconds_slept;
}

sub exit (Int ?$status = 0) is builtin is primitive {
    Perl6::Internals::exit $status;
}

sub Perl6::Internals::eval_parrot (Str $code) is builtin is primitive {
    my $sub = substr($code, 0, 1) eq "."
        ?? Perl6::Internals::compile_pir($code)
        !! Perl6::Internals::compile_pir(".sub pugs_eval_parrot\n$code\n.end\n");
    $sub();
}

sub pi () returns Num is builtin is primitive {
    3.14159265358979323846264338327950288419716939937510;
}

sub lcfirst (Str $str) returns Str is builtin is primitive {
    lc(substr $str, 0, 1) ~ substr $str, 1, chars($str) - 1;
}

sub ucfirst (Str $str) returns Str is builtin is primitive {
    uc(substr $str, 0, 1) ~ substr $str, 1, chars($str) - 1;
}

sub shift (@a) is builtin is primitive {
    my $top = +@a -1;
    return undef if $top < 0;
    my $e = @a[0];
    my $i = 0;
    while $i < $top {
        @a[$i++] = @a[$i];
    }
    pop(@a);
    return $e;
}

sub splice (@a is rw, ?$offset=0, ?$length, *@list) is builtin is primitive {
    my $off = +$offset;
    my $len = $length;
    my $size = +@a;

    $off += $size if $off < 0;
    if $off > $size {
        warn "splice() offset past end of array\n";
        $off = $size;
    }
    # $off is now ready

    $len = +$len if defined($len);
    $len = $size - $off if !defined($len);
    $len = $size + $len - $off if $len < 0;
    $len = 0 if $len < 0;
    # $len is now ready

    my $listlen = +@list;
    my $size_change = $listlen - $len;
    my @result;

    if 1 {
        my $i = $off;
        my $stop = $off + $len;
        while $i < $stop {
            push(@result,@a[$i]);
            $i++;
        }
    }

    if $size_change > 0 {
        my $i = $size + $size_change -1;
        my $final = $off + $size_change;
        while $i >= $final {
            @a[$i] = @a[$i-$size_change];
            $i--;
        }
    } elsif $size_change < 0 {
        my $i = $off;
        my $final = $size + $size_change -1;
        while $i <= $final {
            @a[$i] = @a[$i-$size_change];
            $i++;
        }
        # +@a = $size + $size_change;
        #   doesnt exist yet, so...
        my $n = 0;
        while $n-- > $size_change {
            pop(@a);
        }
    }

    if $listlen > 0 {
        my $i = 0;
        while $i < $listlen {
            @a[$off+$i] = @list[$i];
            $i++;
        }
    }

    #  want.List ?? *@result !! pop(@result)
    #  want.List ?? *@result !! +@result ?? @result[-1] !! undef;
    #  *@result;
    @result;
}
