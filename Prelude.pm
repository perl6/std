module Prelude-0.0.1;

use v6;

# There are a couple of things going on here.
# 
# * These are perl6 implementations of "builtins"
# * They sometimes use Pugs internals to do the job
# * Some of this isn't specced yet (need S29 work).

class File {
    # Simple file open. Unlike perl5 open, it isn't very dwimmy.

    ### XXX: NOTE ###
    # this is intended to eventually replace Prim.hs's open (or be replaced by
    # it, since that would probably be faster)

    # also, the signature for this sub is nowhere near finalized; it should
    # support asynch/exclusive/other stuff. For lots of discussion but no final
    # spec, see the thread rooted at <20050502192508.GF24107@sike.forum2.org>
    # on p6-l.

    multi sub open (Str $filename, Str +$layer, Bool +$r, Bool +$w, Bool +$rw, Bool +$a) returns IO is primitive is unsafe is builtin {
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
}


class Pipe {
    # Easy to use, unidirectional pipe. Uses the shell.

    multi sub open (Str $command, Bool +$r is copy, Bool +$w) returns IO is primitive is unsafe {
        die "Pipe::open is unidirectional" if all($r, $w);
        $r = bool::true if none($r, $w);
        my ($in, $out, $err, undef) =
            Pugs::Internals::runInteractiveCommand($command);
        close $err;
        close  ($r ?? $in :: $out);
        ($r ?? $out :: $in);
    }

    # Bidirectional pipe. Potenially dangerous. Uses the shell.

    multi sub open2 (Str $command) returns List is primitive is unsafe {
        my ($in, $out, $err, $pid) =
            Pugs::Internals::runInteractiveCommand($command);
        close $err;
        ($in, $out, $pid);
    }

    # Tridirectional pipe. Potenially dangerous. Uses the shell.
    # Please remember to update t/pugsrun/11-safemode.t if you change the fully
    # qualified name of open3.

    multi sub open3 (Str $command) returns List is primitive is unsafe {
        my ($in, $out, $err, $pid) =
            Pugs::Internals::runInteractiveCommand($command);
        ($in, $out, $err, $pid);
    }
}

class Control::Basic {
    # multi-lingual eval.

    # S29:
    # Execute C<$code> as if it were code written in C<$lang>. C<Perl6> is the
    # only required language, but supporting C<Perl5> is I<strongly>
    # recommended.

    # safety of the individual methods is defined in Pugs.Prim.hs
    # (maybe :lang<YAML> doesn't quite belong here?)
    multi sub eval (Str ?$code = $CALLER::_, Str +$lang = 'Perl6')
            is primitive is safe is builtin {
        given lc $lang {
            when 'perl6'   { Pugs::Internals::eval($code) };
            when 'perl5'   { Pugs::Internals::eval_perl5($code) };
            when 'haskell' { Pugs::Internals::eval_haskell($code) };
            when 'parrot'  { Pugs::Internals::eval_parrot($code) };
            when 'pir'     { Pugs::Internals::eval_parrot($code) };
            when 'yaml'    { Pugs::Internals::eval_yaml($code) };

            die "Language \"$lang\" unknown.";
        }
    }


    # S29:
    # Behaves like, and replaces Perl 5 C<do EXPR>, with optional C<$lang>
    # support.
    multi sub evalfile (Str $filename : Str +$lang = 'Perl6')
            is primitive is unsafe {
        eval(slurp $filename, $lang);
    }
}


class Control::Caller {
    has Str $.package;
    has Str $.file;
    has Int $.line;
    has Str $.subname;
    has Str $.subtype;
    has Code $.sub;
    has Str $.params;   # FIXME: needs attention; don't use yet.

    multi sub caller (Class ?$kind = Any, Int +$skip = 0, Str +$label) returns Control::Caller is primitive is builtin is safe {
        my @caller = Pugs::Internals::caller($kind, $skip, $label);

        # FIXME: why doesn't this work?
        # this is here just because of an icky pugsbug.
        #my %idx = <package file line subname subtype params> Y 0 .. 5; # ugly.
        #Control::Caller.new( map { ; $_ => @caller[ %idx{$_} ] }, keys %idx );
        #( map { say( $_ => @caller[ %idx{$_} ] ) }, keys %idx );

        @caller.elems ?? Control::Caller.new(
            'package' => @caller[0],
            'file'    => @caller[1],
            'line'    => @caller[2],
            'subname' => @caller[3],
            'subtype' => @caller[4],
            'sub'     => @caller[5],
        ) :: undef;
    }
}


class Carp {
    # Please remember to update t/pugsrun/11-safemode.t if you change the fully
    # qualified name of longmess.
    multi sub longmess (: ?$e = '') returns Str is primitive is safe {
        my($mess, $i);
        $mess = "$e at $?CALLER::POSITION";

        #while Control::Caller::caller(++$i) -> $caller {
        #   $mess ~= "\n\t{$caller.package}::{$caller.subname}() at {$caller.file} line {$caller.line}";
        #}
        loop {
            my $caller = Control::Caller::caller(skip => $i++) err last;
            $mess ~= "\n\t{$caller.package}::{$caller.subname}() at {$caller.file} line {$caller.line}";
        }
        $mess;
    }
}

# Support for =$fh
class IO {
    method shift () is primitive { ./readline() }
}

# Support for ="some_file"
# I'm not sure where this is specced, but when I migrated &prefix:<=> from
# meaning the same as &readline to $obj.shift(), ="some_file" worked, so I
# added the .shift() declaration here.  --iblech
class Str {
    method shift ($self: ) is primitive { =open($self) }
}

# BEGIN { open "..." } should die, as compile-time IO handles are invalid at
# runtime.
# Please remember to edit Pugs.Parser, too, if you rename this sub.
sub Pugs::Internals::check_for_io_leak (Code $usersub) is primitive is safe {
    my $ret = $usersub();
    die "BEGIN and CHECK blocks may not return IO handles, as they'd be\n" ~
        "invalid at runtime." if $ret.isa(IO);
    $ret;
}

sub Pugs::Internals::but_block ($obj, Code $code) is primitive is safe {
    $code($obj);
    $obj;
}

# (Unspecced) facade class supplying localtime
class Time::Local {

    has Int  $.year;    # eg., 2005; "pre-Gregorian dates are inaccurate" - GHC System.Time
    has Int  $.month;   # 1 to 12 - NOTE 1-based
    has Int  $.day;     # 1 to 31
    has Int  $.hour;    # 0 to 23
    has Int  $.min;     # 0 to 59
    has Int  $.sec;     # 0 to 61 (up to two leap seconds)
    has Int  $.picosec;
    has Int  $.wday;    # 1 to 7, Sunday == 1
    has Int  $.yday;    # 0 to 365 (up to one leap day)
    has Str  $.tzname;  # string, eg, JDT
    has Int  $.tz;      # variation from UTC in seconds
    has Bool $.is_dst;

    multi sub localtime(Rat ?$when = time) returns Time::Local is primitive is builtin is safe {
        localtime(int $when, ($when - int $when) * 10**12);
    }
    
    multi sub localtime(Int $sec, Int ?$pico = 0) returns Time::Local is primitive is builtin is safe {
        my $res;
        # XXX: waiting on a better want
        #if want ~~ rx:P5/^Scalar/ {
        #    $res = Pugs::Internals::localtime(bool::true, $sec, $pico);
        #} else {
            my @tm = Pugs::Internals::localtime(bool::false, $sec, $pico);

            # FIXME: this is how it oughta look, with @ids being class level.
            #my @ids = <year month day hour min sec picosec wday yday tzname tz is_dst>; # XXX: this should be a class variable!
            #Time::Local.new( pairs zip @ids, @tm );

            $res = Time::Local.new(
                year    => @tm[0],
                month   => @tm[1],
                day     => @tm[2],
                hour    => @tm[3],
                min     => @tm[4],
                sec     => @tm[5],
                picosec => @tm[6],
                wday    => @tm[7],
                yday    => @tm[8],
                tzname  => @tm[9],
                tz      => @tm[10],
                is_dst  => @tm[11],
            );
        #}
        $res;
    }
}
