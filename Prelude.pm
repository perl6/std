#module Prelude-0.0.1;

=kwid

There are a couple of things going on here.

* These are perl6 implementations of "builtins"
* They sometimes use Pugs internals to do the job
* Some of this isn't specced yet (need S29 work).

=cut

package Pipe;

# Easy to use, unidirectional pipe. Uses the shell.

# FIXME: rename to "open" once namespaces work better
sub openpipe(Str $command, +$r is Bool, +$w is Bool) {
    die "Pipe::open is unidirectional" if all($r, $w);
    $r = bool::true if none($r, $w);
    my ($in, $out, $err, undef) =
        Pugs::Internals::runInteractiveCommand($command);
    close $err;
    close  ($r ?? $in :: $out);
    return ($r ?? $out :: $in);
}

1;

