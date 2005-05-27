module Prelude-0.0.1;

use v6;

=kwid

There are a couple of things going on here.

* These are perl6 implementations of "builtins"
* They sometimes use Pugs internals to do the job
* Some of this isn't specced yet (need S29 work).

=cut

class File;

# Simple file open. Unlike perl5 open, it isn't very dwimmy.

### XXX: NOTE ###
# this is intended to eventually replace Prim.hs's open (or be replaced by
# it, since that would probably be faster)

# also, the signature for this sub is nowhere near finalized; it should
# support asynch/exclusive/other stuff. For lots of discussion but no final
# spec, see the thread rooted at <20050502192508.GF24107@sike.forum2.org>
# on p6-l.

# FIXME: rename to "open" once namespaces work better
sub openfile (Str $filename, Str +$layer, Bool +$r, Bool +$w, Bool +$rw, Bool +$a) returns IO {
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

    return $fh;
}


class Pipe;

# Easy to use, unidirectional pipe. Uses the shell.

# FIXME: rename to "open" once namespaces work better
sub openpipe(Str $command, Bool +$r, Bool +$w) returns IO {
    die "Pipe::open is unidirectional" if all($r, $w);
    $r = bool::true if none($r, $w);
    my ($in, $out, $err, undef) =
        Pugs::Internals::runInteractiveCommand($command);
    close $err;
    close  ($r ?? $in :: $out);
    return ($r ?? $out :: $in);
}



