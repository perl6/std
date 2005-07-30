module Prelude-0.0.1;

use v6;

=kwid

There are a couple of things going on here.

* These are perl6 implementations of /builtins/
* They sometimes use Pugs internals to do the job
* Some of this had not been specced yet (need S29 work).

All functions in this file need to have the `is primitive`
When writing primitives, please do *not* use &return, because
that messes up PIR generation.  To return a value, arrange
it to be the last evaluated expression.

Please declare either `is safe` or `is unsafe`, too.

For functions exported to the global `*` namespace, please use
the `is builtin` trait.

=cut

class File {
    my $SEEK_START = 0;
    my $SEEK_CUR   = 1;
    my $SEEK_END   = 2;

    # Simple file open. Unlike perl5 open, it isn't very dwimmy.

    ### XXX: NOTE ###
    # this is intended to eventually replace Prim.hs's open (or be replaced by
    # it, since that would probably be faster)

    # also, the signature for this sub is nowhere near finalized; it should
    # support asynch/exclusive/other stuff. For lots of discussion but no final
    # spec, see the thread rooted at <20050502192508.GF24107@sike.forum2.org>
    # on p6-l.

    multi sub open (Str $filename, Str +$layer, Bool +$r, Bool +$w, Bool +$rw,
            Bool +$a) returns IO is primitive is unsafe is builtin {
        die "fancy open modes not supported yet" if $a and ($r or $w or $rw);

        my $mode;
        $mode = "a" if $a;
        $mode = "w" if $w;
        $mode = "rw" if $rw or ($r and $w);
        $mode //= "r";

        # XXX failures
        my $fh = Pugs::Internals::openFile($filename, $mode);

        # XXX layers :)
        Pugs::Internals::hSetBinaryMode($fh, bool::true) if
            $layer ~~ rx:P5/:raw\b/;

        $fh;
    }

    multi method seek ($self: Int $position, Int ?$whence = $File::SEEK_START)
            returns Bool is primitive is unsafe is builtin {
        Pugs::Internals::hSeek($seek, $position, $whence);
    }
}


class Pipe {
    # Easy to use, unidirectional pipe. Uses the shell.

    multi sub open (Str $command, Bool +$r is copy, Bool +$w) returns IO
            is primitive is unsafe {
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
    multi sub evalfile (Str $filename: Str +$lang = 'Perl6')
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

    multi sub caller (Class ?$kind = Any, Int +$skip = 0, Str +$label)
            returns Control::Caller is primitive is builtin is safe {
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

role Iter {
    multi sub prefix:<=> (Iter $self: ) { $self.shift() }
    
    method shift   () { ... }
    method next    () { ... }
    method current () { ... }
}

# Support for =$fh
class IO does Iter {
    method shift () is primitive { ./readline() }
    method next  () is primitive { ./shift() }
}

# Support for ="some_file"
# I'm not sure where this is specced, but when I migrated &prefix:<=> from
# meaning the same as &readline to $obj.shift(), ="some_file" worked, so I
# added the .shift() declaration here.  --iblech
class Str does Iter {
    method shift ($self: ) is primitive { =open($self) }

    method trans (Str $self: *%intable) is primitive is safe {
        my sub expand(Str $string) {
            if ($string ~~ m:P5/([^-]+)\-([^-]+)/) {
                (~ $0)..(~ $1);
            } else {
                $string;
            }
        }

        # If in doubt use brute force.
        my %transtable = map {;zip(split('',$_.key),split('',$_.value))}
                         map {;expand $_.key => expand $_.value}
                         %intable;
    
        [~] map { %transtable{$_} // $_ } $self.split('');
    }
}

sub Pugs::Internals::but_block ($obj, Code $code) is primitive is safe {
    $code($obj);
    $obj;
}

# (Unspecced) facade class supplying localtime
class Time::Local {
    has Int  $.year;    # eg., 2005; "pre-Gregorian dates are inaccurate" - GHC
                        # System.Time
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

    multi sub localtime(Num ?$when = time) returns Time::Local
            is primitive is builtin is safe {
        my $res;
        my $sec = int $when;
        my $pico = ($when - int $when) * 10**12;
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

class Num {
    multi sub round_gen(Int $n, Code $corner) returns Int is safe {
        $n
    }
    multi sub round_gen(Num $n, Code $corner) returns Int is safe {
        (int($n) == $n) ?? int($n) :: $corner($n);
    }

    sub do_round($n) is primitive is safe {
        ($n < 0) ?? int( $n - 0.5) :: int($n + 0.5);
    }
    sub round($n) is primitive is safe {
        round_gen($n, &do_round)
    }

    sub truncate($n) is primitive is safe {
        int($n)
    }
    our &trunc ::= &truncate;

    sub do_ceil($n) is primitive is safe {
        ($n < 0) ?? (-int(-$n)) :: int($n + 1)
    }
    sub ceiling($n) is primitive is safe {
        round_gen($n, &do_ceil)
    }
    our &ceil ::= &ceiling;

    sub do_floor($n) is primitive is safe {
        ($n < 0) ?? (-int(1-$n)) :: int($n)
    }
    sub floor($n) is primitive is safe {
        round_gen($n, &do_floor)
    }
}

sub sprintf ($fmt, *@args) is primitive is builtin is safe {
    my $flen = $fmt.chars;
    my $fi = 0;
    my $ai = 0;
    my $str = "";
    while ($fi < $flen) {
	# optional non-conversion text
	my $idx = index($fmt,"%",$fi);
	if $idx < 0 {
	    $str ~= substr($fmt,$fi);
	    last;
	} else {
	    my $len = $idx - $fi;
	    $str ~= substr($fmt,$fi, $len) if $len > 0;
	    $fi = $idx;
	}

	# a conversion
	my $start = $fi;
	$fi++;
	while !(substr($fmt,$fi,1)
		~~ any<% c s d u o x e f g X E G b p n i D U O F>) {
	    $fi++;
	}
	my $specifier = substr($fmt,$fi,1); $fi++;
	my $conversion = substr($fmt,$start,$fi - $start);

	# FIXME -- when next; works, do if $spec eq "%" { ...; next; }
	my $arg;
	if $specifier ne '%' {
	    die "Insufficient arguments to sprintf" if $ai >= +@args;
	    $arg = @args[$ai];
	    $ai++;
	}

	given $specifier {
	    when any(<c d u o x b i>) {
		$str ~= Pugs::Internals::sprintf($conversion,int($arg));
	    }
	    when 's' {
		$str ~= Pugs::Internals::sprintf($conversion,"$arg");
	    }
	    when any(<e f g>) {
		$str ~= Pugs::Internals::sprintf($conversion,1.0*$arg);
	    }
	    when any(<X D U O>) {
		$str ~= uc Pugs::Internals::sprintf(lc($conversion),int($arg));
	    }
	    when any(<E G F>) {
		$str ~= uc Pugs::Internals::sprintf(lc($conversion),1.0*$arg);
	    }
	    when '%' {
		$str ~= '%';
	    }
	    default {
		die "sprintf does not yet implement %{$specifier}";
	    }
	}
    }
    $str;
}

sub Scalar::as ($obj, $fmt) is primitive is safe {
    sprintf($fmt,$obj);
}


# These are unspecced, and will likely go away at some point.
sub Rule::pattern(Rule $obj) is primitive is safe {
    Pugs::Internals::rule_pattern($obj);
}
sub Rule::adverbs(Rule $obj) is primitive is safe {
    Pugs::Internals::rule_adverbs($obj);
}
