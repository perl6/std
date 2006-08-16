use v6-alpha;

module Prelude-0.0.1;


=kwid

There are a couple of things going on here.

* These are perl6 implementations of /builtins/
* They sometimes use Pugs internals to do the job
* Some of this had not been specced yet (need S29 work).

When writing primitives, please do *not* use `return()`, because
that messes up PIR generation.  To return a value, arrange for
it to be the last evaluated expression.

All functions (but not macros) in this file need to have the
`is primitive` trait.

Please also declare whether the function can be used in a safe
context (such as the IRC evalbot) using `is safe` or `is unsafe`.

For functions exported to the global `*` namespace, please use
the `is builtin` trait.

=cut


class Process {
    multi sub exec($prog, @args) returns Bool is builtin is primitive is unsafe {
        # say "e:prog args"; # XXX delme
        Pugs::Internals::exec($prog, Bool::False, @args);
    }
    multi sub exec(@args) returns Bool is builtin is primitive is unsafe {
        # say "e:args:" ~ @args.perl; # XXX delme
        Pugs::Internals::exec(@args[0], Bool::True, @args);
    }
    multi sub exec($string) returns Bool is builtin is primitive is unsafe {
        # say "e:string"; # XXX delme
        # so, why do these two not actually split?
        # my @args = $string.split(rx:Perl5/\s+/);
        # my @args = split(rx:Perl5/\s+/, $string);
        
        my @args = $string.split;
        exec(@args);
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
    multi sub eval (Str $code = $CALLER::_, Str :$lang = 'Perl6')
            is primitive is safe is builtin {
        given lc $lang {
            when 'perl6'   { Pugs::Internals::eval_perl6($code) };
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
    multi sub evalfile (Str $filename; Str :$lang = 'Perl6')
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
}

multi sub caller (Class $kind = Any, Int :$skip = 0, Str :$label)
        returns Control::Caller is primitive is builtin is safe {
    my @caller = Pugs::Internals::caller($kind, $skip, $label);

    # FIXME: why doesn't this work?
    # this is here just because of an icky pugsbug.
    #my %idx = <package file line subname subtype params> Y 0 .. 5; # ugly.
    #Control::Caller.new( map { ; $_ => @caller[ %idx{$_} ] }, keys %idx );
    #( map { say( $_ => @caller[ %idx{$_} ] ) }, keys %idx );

    @caller.elems ?? Control::Caller.new(
        package => @caller[0],
        file    => @caller[1],
        line    => @caller[2],
        subname => @caller[3],
        subtype => @caller[4],
        sub     => @caller[5],
    ) !! undef;
}

class fatal {
    # pragma to dispense of the holy war about whether to raise
    # exceptions or return false values to signal error conditions
    # in library code. Do either! Let your caller decide!
    # instead of "die             'file not found';"
    #         or "return ENOENT; # file not found"
    #  say this: "fail            'file not found';"
    
    # make 'use fatal' not try to load a module. XXX: add a real inc class to
    # interface %*INC, instead of this hack.
    # %*INC<fatal> = { filename => "fatal", resname => "<precompiled>", };

    $fatal::DEFAULT_FATALITY = 1;
    
    sub import {
        Pugs::Internals::install_pragma_value($?CLASS, 1);
    }

    sub unimport {
        Pugs::Internals::install_pragma_value($?CLASS, 0);
    }

    # XXX: here's my doubt. In Preluded code, do I need
    #      current_pragma_value or caller_pragma_value?
    #      I'm guessing "current" because this "is primitive".
    #      If I'm wrong then the above two subs might not work?
    # XXX2: "fail" clashes with Test's &fail.
    sub __fail($e = "failed") is primitive is builtin is safe {
        if Pugs::Internals::current_pragma_value($?CLASS) //
                $fatal::DEFAULT_FATALITY {
            die $e;
        } else {
            $! = $e;
            return undef; # this is probably the one place we can return
                          # in the Prelude: we want to exit from the
                          # *caller*'s scope.
        }
    }
}

class Carp {
    # Please remember to update t/pugsrun/11-safemode.t if you change the fully
    # qualified name of longmess.
    multi sub longmess (; $e = '') returns Str is primitive is safe {
        my($mess, $i);
        $mess = "$e at $?CALLER::POSITION";

        #while Control::Caller::caller(+:$i) -> $caller {
        #   $mess ~= "\n\t{$caller.package}::{$caller.subname}() at {$caller.file} line {$caller.line}";
        #}
        loop (;;) {
            my $caller = Control::Caller::caller(skip => $i++) err last;
            $mess ~= "\n\t{$caller.package}::{$caller.subname}() at {$caller.file} line {$caller.line}";
        };

        $mess;
    }
}


=kwid

role Rule {}
class Pugs::Internals::VRule does Rule {}
class Rul does Rule is builtin {
    has $.f;
}
# This infix:<~~> doesnt work yet.
multi sub infix:<~~> ($x, Rul $r) is primitive is safe is builtin {$r.f.($x)}
multi sub infix:<~~> (Rul $r, $x) is primitive is safe is builtin {$r.f.($x)}


sub rx_common_($hook,%mods0,$pat0,$qo,$qc) is builtin is safe {
    state(%modifiers_known, %modifiers_supported_p6, %modifiers_supported_p5);
    START {
        %modifiers_known = map {;($_ => 1)},
        <perl5 Perl5 P5 i ignorecase s sigspace g global c continue p pos
        once bytes codes graphs langs x nth ov overlap ex exhaustive
        rw keepall e each any parsetree stringify>;
        %modifiers_supported_p6 = map {;($_ => 1)},
        <i ignorecase s sigspace g global  stringify>;
        %modifiers_supported_p5 = map {;($_ => 1)},
        <perl5 Perl5 P5 i ignorecase g global  stringify>;
    }
    my $pat = $pat0;
    my %mods = %mods0;
    my $p5 = %mods{"perl5"} || %mods{"Perl5"} || %mods{"P5"};
    #my sub warning($e){warn(Carp::longmess($e))};# XXX doesnt work yet.
    my sub warning($e){warn("Warning: $e\n")};
    for %mods.keys -> $k {
        if %modifiers_known.{$k} {
            if $p5 && !%modifiers_supported_p5.{$k} {
                warning "Modifier :$k is not (yet?) supported by :perl5 regexps.";
            } elsif !$p5 && !%modifiers_supported_p6.{$k} {
                warning "Modifier :$k is not yet supported by PGE/pugs.";
            }
        }
        elsif ($k.chars > 1 && substr($k,-1,1) eq "x"
               && internals_m:perl5/\A(\d+)x\Z/) {
            my $n = +$0;
            %mods.delete($k);
            %mods{'x'} = $n;
        }
        elsif ($k.chars > 2 && substr($k,-2,2) eq ("th"|"st"|"nd"|"rd")
                 && pugs_internals_m:perl5/\A(\d+)(?:th|st|nd|rd)\Z/) {
            my $n = +$0;
            %mods.delete($k);
            %mods{'nth'} = $n;
        }
        else {
            my $msg = "Unknown modifier :$k will probably be ignored.";
            $msg ~= "  Perhaps you meant :i:s ?" if $k eq ("is"|"si");
            warning $msg;
        }
    }
    if !$p5 {
        my $pre = "";
        if %mods<i> || %mods<ignorecase> {      
            $pre ~= ":i";
            %mods.delete("i"); # avoid haskell handling it.
            %mods.delete("ignorecase");
#           warning "PGE doesn't actually do :ignorecase yet.";
        }
        if %mods<s> || %mods<sigspace> {      
            $pre ~= ":s";
            %mods.delete("s"); # avoid haskell handling it.
            %mods.delete("sigspace");
        }
        if $pre ne "" {
            $pre ~= "::" if substr($pat,0,1) ne (":"|"#");
            $pat = $pre ~ $pat;
        }
    }
    my $g = %mods{'g'} || %mods{'global'};
    my $ov = %mods{'ov'} || %mods{'overlap'};
    my $ex = %mods{'ex'} || %mods{'exhaustive'};
    my $adverbs = join("",map {":"~$_}, %mods.keys);
    if $ov && 0 { # XXX disabled until Rul works.
        my($str,$pos,$re,$m,$m0,$a,$s,$at,$prev) =
        ('$_str_','$_pos_','$_re_','$_m_','$_m0_',
         '$_a_','$_s_','$_at_','$_prev_');
        my $code = "Rul.new(:f(sub($str)\{
           my $re = $hook$adverbs$qo$pat$qc;
           my $pos = 0;  my $a = []; my $prev = -1; my $m;
           while 1 \{
              my $s = substr($str,$pos) // last;
              $m = $s ~~ $re or last;
              my $m0 = {$m}[0];
              my $at = $pos + $m0.from;
              {$a}.push($m0) if $at > $prev; $prev = $at;
              $pos += {$m}.from + 1;
           }
           # want.Item
           0 ?? ([|] \@$a) !! $a }))";
        return $code;
    }
    # Use of Rul awaits working infix:<~~> .
    #'Rul.new(:f(sub($_s_){$_s_ ~~ '~"$hook$adverbs$qo$pat$qc}))";
    "$hook$adverbs$qo$pat$qc";
}

# These macros cannot be "is primitive".
macro rxbare_ ($pat) is builtin is safe {
    rx_common_("pugs_internals_rxbare",hash(),$pat,"/","/");
}
macro rx_ (%mods,$pat,$qo,$qc) is builtin is safe {
    rx_common_("pugs_internals_rx",%mods,$pat,$qo,$qc);
}
macro m_ (%mods,$pat,$qo,$qc) is builtin is safe {
    rx_common_("pugs_internals_m",%mods,$pat,$qo,$qc);
}

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

    multi sub open (Str $filename, Str :$layer, Bool :$r, Bool :$w, Bool :$rw,
            Bool :$a) returns IO is primitive is unsafe is builtin {
        die "fancy open modes not supported yet" if $a and ($r or $w or $rw);

        my $mode;
        $mode = "a" if $a;
        $mode = "w" if $w;
        $mode = "rw" if $rw or ($r and $w);
        $mode //= "r";

        # XXX failures
        my $fh = Pugs::Internals::openFile($filename, $mode);

        # XXX layers :)
        Pugs::Internals::hSetBinaryMode($fh, Bool::True) if
            $layer ~~ rx:P5/:raw\b/;

        $fh;
    }

    multi method seek (Int $position, Int $whence = $File::SEEK_START)
            returns Bool is primitive is unsafe is builtin {
        Pugs::Internals::hSeek(self, $position, $whence);
    }
}


class Pipe {
    # Easy to use, unidirectional pipe. Uses the shell.

    multi sub open (Str $command, Bool :$r is copy, Bool :$w) returns IO
            is primitive is unsafe {
        die "Pipe::open is unidirectional" if all($r, $w);
        $r = Bool::True if none($r, $w);
        my ($in, $out, $err) =
            Pugs::Internals::runInteractiveCommand($command);
        close $err;
        close  ($r ?? $in !! $out);
        ($r ?? $out !! $in);
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


role Iter {
    multi sub prefix:<=> () is primitive { self.shift() }
    
    method shift   () { ... }
    method next    () { ... }
    method current () { ... }
}

# Support for =$fh
class IO does Iter {
    method shift   () is primitive { self.readline() }
    method next    () is primitive { self.shift() }
}

# Support for ="some_file"
# I'm not sure where this is specced, but when I migrated &prefix:<=> from
# meaning the same as &readline to $obj.shift(), ="some_file" worked, so I
# added the .shift() declaration here.  --iblech
class Str does Iter {
    method shift () is primitive { =open(self) }

    method trans (Pair *@intable) is primitive is safe {
        # Motto: If in doubt use brute force!
        my sub expand (Str $string is copy) {
            my @rv;

            my $add_dash;
            my $idx;

            if (substr($string,0,1) eq '-') {
                push @rv, '-';
                $string = substr($string,1);
            }

            if (substr($string,-1,1) eq '-') {
                $add_dash = 1;
                $string = substr($string,0,-1)
            }

            while (($idx = index($string,'-')) != -1) {
                my $pre = substr($string,0,$idx-1);
                my $start = substr($string,$idx-1,1);
                my $end = substr($string,$idx+1,1);

                push @rv, $pre.split('');
                push @rv, (~ $start)..(~ $end);

                $string = substr($string,$idx+2);
            }

            push @rv, $string.split('');
            push @rv, '-' if $add_dash;

            @rv;
        }

        my %transtable;
        for @intable -> Pair $pair {
            my ($k, $v) = $pair.kv;
            # $k is stringified by the => operator.
            my @ks = $k.isa(Str) ?? expand($k) !! $k.values;
            my @vs = $v.isa(Str) ?? expand($v) !! $v.values;
            %transtable{@ks} = @vs;
        }

        [~] map { %transtable{$_} // $_ }, self.split('');
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
}

multi sub localtime(Num $when = time) returns Time::Local
        is primitive is builtin is safe {
    my $res;
    my $sec = int $when;
    my $pico = ($when - int $when) * 10**12;
    # XXX: waiting on a better want
    #if want ~~ rx:P5/^Item/ {
    #    $res = Pugs::Internals::localtime(Bool::True, $sec, $pico);
    #} else {
        my @tm = Pugs::Internals::localtime(Bool::False, $sec, $pico);

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

class Num {
    multi sub round_gen(Int $n, Code $corner) returns Int is primitive is safe {
        $n
    }
    multi sub round_gen(Num $n, Code $corner) returns Int is primitive is safe {
        (int($n) == $n) ?? int($n) !! $corner($n);
    }

    sub do_round($n) is primitive is safe {
        ($n < 0) ?? int( $n - 0.5) !! int($n + 0.5);
    }
    sub round($n) is primitive is safe {
        Num::round_gen($n, &Num::do_round)
    }

    sub truncate($n) is primitive is safe {
        int($n)
    }
    our &trunc ::= &truncate;

    sub do_ceil($n) is primitive is safe {
        ($n < 0) ?? (-int(-$n)) !! int($n + 1)
    }
    sub ceiling($n) is primitive is safe {
        Num::round_gen($n, &Num::do_ceil)
    }
    our &ceil ::= &ceiling;

    sub do_floor($n) is primitive is safe {
        ($n < 0) ?? (-int(1-$n)) !! int($n)
    }
    sub floor($n) is primitive is safe {
        Num::round_gen($n, &Num::do_floor)
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
                ~~ any(<% c s d u o x e f g X E G b p n i D U O F>)) {
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

multi shift (@array) is builtin is primitive { List::shift(@array) };
multi shift ($array) is builtin is primitive { die "Cannot 'shift' scalar"; };

multi pop (@array) is builtin is primitive { List::pop(@array) };
multi pop ($array) is builtin is primitive { die "Cannot 'pop' scalar"; };

multi as (Scalar $obj; $fmt) is builtin is primitive is safe {
    sprintf($fmt,$obj);
}
multi as (List $obj; $fmt, $comma) is builtin is primitive is safe {
    join($comma, map -> $v { sprintf($fmt,$v) }, @$obj );
}
multi as (Hash $obj; $fmt, $comma) is builtin is primitive is safe {
    join($comma, map -> $k,$v { sprintf($fmt,$k,$v) }, $obj.kv );
}

sub PIL2JS::Internals::use_jsan_module_imp (*@whatever) {
    die "Can't load JSAN modules when not running under PIL2JS!";
}
our &PIL2JS::Internals::use_jsan_module_noimp := &PIL2JS::Internals::use_jsan_module_imp;

sub PIL2JS::Internals::use_perl5_module_imp (*@whatever) {
    die "Can't load perl5 modules via js when not running under PIL2JS!";
}
our &PIL2JS::Internals::use_perl5_module_noimp := &PIL2JS::Internals::use_perl5_module_imp;


# In src/perl6/Prelude.pm, prefix:<-M> doesn't work. :(
multi prefix_M ($file) is builtin is primitive is unsafe {
  if not -e $file {
    undef;
  }
  elsif $file ~~ rx:perl5/[^-_a-zA-Z0-9\.\/\\\:]/ {
    warn "-M bug: avoided $file";
    undef;
  }
  else {
    my $cmd = %?CONFIG<perl5path>~q{ -e 'print join("\n", map {-M}, @ARGV,"")' }~$file;
    my $p = Pipe::open($cmd);
    my $m = slurp($p);  $p.close;
    # In src/perl6/Prelude.pm, regexs dont work. :(
    #die "-M bug $m" if not $m ~~ rx:perl5/\A[-e0-9\.]+\Z/;    
    +$m;
  }
}



sub Pugs::Internals::Disabled::use ($module=$+_) is builtin is unsafe {
    #Pugs::Internals::use($module);
    Pugs::Internals::require_use_helper(Bool::True,$module);
}
sub Pugs::Internals::Disabled::require ($module=$+_) is builtin is unsafe {
    #Pugs::Internals::require($module);
    Pugs::Internals::require_use_helper(Bool::False,$module);
}
sub Pugs::Internals::require_use_helper ($use_,$module) is builtin is unsafe {
  my sub opRequire() { 
    $use_
      ?? Pugs::Internals::use($module)
      !! Pugs::Internals::require($module);
  }
  my sub find() {
    my $file = join(%?CONFIG<file_sep>,split("::",$module)) ~ ".pm";
    for @*INC -> $dir {
      my $path = $dir ~ %?CONFIG<file_sep> ~ $file;
      next if !(-e $path);
      my $yml = "$path.yml";
      if -e $yml && prefix_M($yml) < prefix_M($path) {
      }
      else {
        if 1 {
          Pugs::Internals::compile_file_to_yml($path);
        }
      }
      return opRequire();
    }
    die "Can't find " ~ $file ~ ' in @*INC (@*INC contains: ' ~
        join(' ',@*INC) ~ ")."; #XXX - should be fail
  }
  my $seen = %*INC{$module};
  if $seen {
    opRequire();
  }
  else {
    find();
  }
}
sub Pugs::Internals::compile_file_to_yml($file) is builtin is unsafe {
    # XXX - re-enable this when Parse-YAML supports closures correctly!
    return() if index($file, 'Test.pm') == -1;
    say "Attempting to compile $file ...";
    system($*EXECUTABLE_NAME~" -CParse-YAML $file > $file.yml");
    say "back.";
}
