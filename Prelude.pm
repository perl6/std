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

use Math::Basic :GLOBAL<pi>;

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
    multi sub eval (Str $code, Str :$lang = 'Perl6') is primitive is safe is builtin {
        &::("Pugs::Internals::eval_"~lc($lang))($code);
        # die "Language \"$lang\" unknown.";
    }


    # S29:
    # Behaves like, and replaces Perl 5 C<do EXPR>, with optional C<$lang>
    # support.
    multi sub evalfile (Str $filename; Str :$lang = 'Perl6')
            is primitive is unsafe {
        &eval(slurp $filename, $lang);
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
    #my %idx = <package file line subname subtype params> Z 0 .. 5; # ugly.
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
    # Please remember to update t/run/11-safemode.t if you change the fully
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

#    multi method comb (Regex $rx = /\S+/) is primitive {
#       list self ~~ rx:g/<$rx>/;
#    }

    multi method comb () is primitive is safe {
        list self ~~ rx:P5:g/\S+/;
    }

    multi method comb ($rx) is primitive {
        given $rx {
            when Str { list self ~~ rx:P5:g/\Q$rx\E/; }
            when Regex {    # XXX kludge absence of /<$rx>/ above
                my $str = ~self;
                gather {
                    while $str ~~ $rx {
                        take ~$();
                        substr($str, 0, $/.to) = "";
                    }
		    # XXX Pugs would fail if removed the ; in the next line.
                };
            }
        }
    }

    # XXX: This seems to basically work but it is 
    # a) wrong
    # b) slow
    # Sadly we can't use <$rule> in s/// because it is unimplemented in current pugs
    # so we just rebuild a regexen from it's source and  adverbs.
    # Adverbs may have arguments, but since we just recreate them via String manipulation
    # they only work if eval("$arg") == $arg.
    # A Rule/Regex constructor that takes a string and an Hash could be a useful thing?

    # FIXME: this should be the definition, but with this if I call 
    # 'hello'.subst(/h/,'f')
    # $rule is empty (???)

    # method subst(Str|Regex $rule, $replacement) is primitive is safe {

    method subst($rule, $replacement) is primitive is safe {
        my ($rgx,$adv,$dup = self);
        # can't use ~~ because /foo/ ~~ Str matches
        if $rule.isa(Str) {
          $rgx =  "<'$rule'>";
        }
        else {
          $rgx = Pugs::Internals::rule_pattern($rule);
          $adv = Pugs::Internals::rule_adverbs($rule);
        }

        # in the map body I can't use a single Str because otherwise interpolation goes wrong
        my $advstr = $adv.map: { ":$_[0]"~"($_[1])"}.join if $adv;
        my $apply = '()' if $replacement ~~ Code;
        my $op="s$advstr/$rgx/\$replacement$apply/";

        eval('$dup ~~ '~$op);
        $dup
    }

    method match(Regex $rule) is primitive is safe{
        self ~~ $rule
    }

    method trans (Pair *@intable) is primitive {
        # Motto: If in doubt use brute force!
        my sub expand (Str $string is copy) {
            my @rv;

            my $idx;

            $string ~~ s:P5:g/\s+//;
            my $delim = '!';
            while index($string,$delim) != -1 {
                $delim = chr(ord($delim)+1);    # XXX still spoofable
            }
            $string = eval "qb$delim$string$delim";
            while (($idx = index($string,'..')) != -1) {
                my $pre = substr($string,0,$idx-1);
                my $start = substr($string,$idx-1,1);
                my $end = substr($string,$idx+2,1);

                push @rv, $pre.split('');
                push @rv, (~ $start)..(~ $end);

                $string = substr($string,$idx+3);
            }

            push @rv, $string.split('');

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

    method graphs(Str $s:) returns Int is primitive is safe {
        # a list of all unicode combining chars
        my Int @combining = (0x0300, 0x0301, 0x0302, 0x0303,
            0x0304, 0x0305, 0x0306, 0x0307, 0x0308, 0x0309,
            0x030A, 0x030B, 0x030C, 0x030D, 0x030E, 0x030F,
            0x0310, 0x0311, 0x0312, 0x0313, 0x0314, 0x0315,
            0x0316, 0x0317, 0x0318, 0x0319, 0x031A, 0x031B,
            0x031C, 0x031D, 0x031E, 0x031F, 0x0320, 0x0321,
            0x0322, 0x0323, 0x0324, 0x0325, 0x0326, 0x0327,
            0x0328, 0x0329, 0x032A, 0x032B, 0x032C, 0x032D,
            0x032E, 0x032F, 0x0330, 0x0331, 0x0332, 0x0333,
            0x0334, 0x0335, 0x0336, 0x0337, 0x0338, 0x0339,
            0x033A, 0x033B, 0x033C, 0x033D, 0x033E, 0x033F,
            0x0340, 0x0341, 0x0342, 0x0343, 0x0344, 0x0345,
            0x0346, 0x0347, 0x0348, 0x0349, 0x034A, 0x034B,
            0x034C, 0x034D, 0x034E, 0x034F, 0x0350, 0x0351,
            0x0352, 0x0353, 0x0354, 0x0355, 0x0356, 0x0357,
            0x0358, 0x0359, 0x035A, 0x035B, 0x035C, 0x035D,
            0x035E, 0x035F, 0x0360, 0x0361, 0x0362, 0x0363,
            0x0364, 0x0365, 0x0366, 0x0367, 0x0368, 0x0369,
            0x036A, 0x036B, 0x036C, 0x036D, 0x036E, 0x036F,
            0x0483, 0x0484, 0x0485, 0x0486, 0x0488, 0x0489,
            0x135F, 0x1DC0, 0x1DC1, 0x1DC2, 0x1DC3, 0x20D0,
            0x20D1, 0x20D2, 0x20D3, 0x20D4, 0x20D5, 0x20D6,
            0x20D7, 0x20D8, 0x20D9, 0x20DA, 0x20DB, 0x20DC,
            0x20DD, 0x20DE, 0x20DF, 0x20E0, 0x20E1, 0x20E2,
            0x20E3, 0x20E4, 0x20E5, 0x20E6, 0x20E7, 0x20E8,
            0x20E9, 0x20EA, 0x20EB, 0x3099, 0x309A, 0xFE20,
            0xFE21, 0xFE22, 0xFE23, 0x1D165, 0x1D166, 0x1D167,
            0x1D168, 0x1D169, 0x1D16D, 0x1D16E, 0x1D16F, 0x1D170,
            0x1D171, 0x1D172, 0x1D17B, 0x1D17C, 0x1D17D, 0x1D17E,
            0x1D17F, 0x1D180, 0x1D181, 0x1D182, 0x1D185, 0x1D186,
            0x1D187, 0x1D188, 0x1D189, 0x1D18A, 0x1D18B, 0x1D1AA,
            0x1D1AB, 0x1D1AC, 0x1D1AD, 0x1D242, 0x1D243, 0x1D244);
        my Int $nc = $s.codes;
        my Int $ng = $nc;
        loop(my $i = 0; $i < $nc; $i++) {
            # Currently, substr treats its numerical arguments as though
            # they were in units of codepoints, but it should probably
            # default to graphemes.  So this should really be
            # substr($s, $i.as(Codes), 1.as(Codes))  S02:620
            # but that isn't implemented yet.  This unitless version
            # may (should) stop working in the future.
            $ng-- if substr($s, $i, 1).ord == any(@combining);
        }
        $ng;
    }

    method chars(Str $s:) returns Int is primitive is safe {
        $s.graphs;
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

    # Not Public API

    multi sub round_gen(Int $n, Code $corner) returns Int is primitive is safe {
        $n
    }
    multi sub round_gen(Num $n, Code $corner) returns Int is primitive is safe {
        (int($n) == $n) ?? int($n) !! $corner($n);
    }

    sub do_round($n) is primitive is safe {
        ($n < 0) ?? int( $n - 0.5) !! int($n + 0.5);
    }
    sub do_ceil($n) is primitive is safe {
        ($n < 0) ?? (-int(-$n)) !! int($n + 1)
    }
    sub do_floor($n) is primitive is safe {
        ($n < 0) ?? (-int(1-$n)) !! int($n)
    }

    # Public API (but signatures are not spec)

    sub round($n) is primitive is safe is builtin {
        Num::round_gen($n, &Num::do_round)
    }

    sub truncate($n) is primitive is safe is builtin { $n.int }
    sub trunc($n) is primitive is safe is builtin { $n.int }

    sub ceiling($n) is primitive is safe is builtin { Num::round_gen($n, &Num::do_ceil) }
    sub ceil($n) is primitive is safe is builtin { Num::round_gen($n, &Num::do_ceil) }

    sub floor($n) is primitive is safe is builtin {
        Num::round_gen($n, &Num::do_floor)
    }
}

# *pi is non-spec (S29);  Should require use Math::Basic :constants;
# use Math::Basic :GLOBAL<pi>; fails to define *pi, so...
sub pi() is primitive is builtin is safe {Math::Basic::pi}


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
            when any(<c d u o x i>) {
                $str ~= Pugs::Internals::sprintf($conversion,int($arg));
            }

            ##
            # No "%b" in Haskell Printf libraries
            # This may not be 100% compatible with C sprintf,
            # or even the rest of Perl 6's sprintf
            when 'b' {
                my Bool @num;
                for (0..~int($arg).bytes*8-1) -> $bit {
                    push @num, int($arg) +& ( 1 ~ (0 x ($bit))) ?? 1 !! 0;
                }

                my $converted = int(@num.reverse.join(""));

                $conversion ~~ m:P5/(\d+)/;
                my $formatter = ~$0;
   
                my $length = int($formatter) - $converted.bytes;
   
                my $ret;
                if ($length < 0) {
                    $ret = int(@num.reverse.join(""));
                }
                else {
                    given $formatter {
                        when rx:P5/^0/ {
                            $ret = (('0' x $length) ~ $converted);
                        }
                        default {
                            $ret = ((' ' x $length) ~ $converted);
                        }
                    }
                }
                $str ~= $ret;

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

multi fmt ($_; $fmt) is builtin is primitive is safe {
    when Pair { sprintf($fmt, .kv) }
    default { sprintf($fmt, $_) }
}
#multi fmt (Pair $obj; $fmt) is builtin is primitive is safe {
#    sprintf($fmt,$obj.kv);
#}
# $comma defaults chosen per L<S02/Literals/"interpolate an entire array">.
multi fmt (@obj; $fmt, $comma = ' ') is builtin is primitive is safe {
    join($comma, map -> $v { sprintf($fmt, $v.isa(Pair) ?? $v.kv !! $v) }, @obj );
}
multi fmt (%obj; $fmt, $comma = "\n") is builtin is primitive is safe {
    join($comma, map -> $k,$v { sprintf($fmt,$k,$v) }, %obj.kv );
}

sub PIL2JS::Internals::use_jsan_module_imp (*@whatever) {
    die "Can't load JSAN modules when not running under PIL2JS!";
}
our &PIL2JS::Internals::use_jsan_module_noimp ::= &PIL2JS::Internals::use_jsan_module_imp;

sub PIL2JS::Internals::use_perl5_module_imp (*@whatever) {
    die "Can't load perl5 modules via js when not running under PIL2JS!";
}
our &PIL2JS::Internals::use_perl5_module_noimp ::= &PIL2JS::Internals::use_perl5_module_imp;


# In src/perl6/Prelude.pm, prefix:<-M> doesn't work. :(
multi prefix_M ($file) is builtin is primitive is unsafe {
  if $file ~~ :!e {
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

# These are used by t/xx-xx-uncategorized/prelude_test.t
sub  *prelude_test_1(){'test 1'}
sub   prelude_test_2_helper(){'test 2'}
&*prelude_test_2 ::= &prelude_test_2_helper;
sub   prelude_test_3_helper(){'test 3'}
&*prelude_test_3 :=  &prelude_test_3_helper;
multi prelude_test_4(Str $x) is builtin {'test 4'}
multi *prelude_test_5(Str $x) {'test 5'}

=begin END

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
      next if $path ~~ :!e;
      my $yml = "$path.yml";
      if (($yml ~~ :e) && prefix_M($yml) < prefix_M($path)) {
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
