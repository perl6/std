module Prelude-0.0.1;

use v6;

# There are a couple of things going on here.
# 
# * These are perl6 implementations of "builtins"
# * They sometimes use Pugs internals to do the job
# * Some of this isn't specced yet (need S29 work).

class File;

# Simple file open. Unlike perl5 open, it isn't very dwimmy.

### XXX: NOTE ###
# this is intended to eventually replace Prim.hs's open (or be replaced by
# it, since that would probably be faster)

# also, the signature for this sub is nowhere near finalized; it should
# support asynch/exclusive/other stuff. For lots of discussion but no final
# spec, see the thread rooted at <20050502192508.GF24107@sike.forum2.org>
# on p6-l.

multi sub open (Str $filename, Str +$layer, Bool +$r, Bool +$w, Bool +$rw, Bool +$a) returns IO is primitive {
    die "fancy open modes not supported yet" if $a & any($r, $w, $rw);
    my $mode;
    $mode = "a" if $a;
    $mode = "w" if $w;
    $mode = "rw" if $rw || $r & $w;
    $mode ||= "r";

    # XXX failures
    my $fh = Pugs::Internals::openFile($filename, $mode);

    # XXX layers :)
    Pugs::Internals::hSetBinaryMode($fh, bool::true) if
        $layer ~~ rx:P5/:raw\b/;

    $fh;
}


class Pipe;

# Easy to use, unidirectional pipe. Uses the shell.

multi sub open (Str $command, Bool +$r is copy, Bool +$w) returns IO is primitive {
    die "Pipe::open is unidirectional" if all($r, $w);
    $r = bool::true if none($r, $w);
    my ($in, $out, $err, undef) =
        Pugs::Internals::runInteractiveCommand($command);
    close $err;
    close  ($r ?? $in :: $out);
    ($r ?? $out :: $in);
}

# Bidirectional pipe. Potenially dangerous. Uses the shell.

multi sub open2 (Str $command) returns List is primitive {
    my ($in, $out, $err, $pid) =
        Pugs::Internals::runInteractiveCommand($command);
    close $err;
    ($in, $out, $pid);
}

# Tridirectional pipe. Potenially dangerous. Uses the shell.

multi sub open3 (Str $command) returns List is primitive {
    my ($in, $out, $err, $pid) =
        Pugs::Internals::runInteractiveCommand($command);
    ($in, $out, $err, $pid);
}
