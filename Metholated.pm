grammar Metholated;

# XXX still full of ASCII assumptions

has $.targ;

our class MCont does Hash {
    has Metholated $.matcher;
    has Bool $.bool is rw = 1;
    has StrPos $.from;
    has StrPos $.to;
    has $!item;

    # XXX for some pugsian reason can't make these work as ordinary attributes.
    method setname($k) {
        self{'hash'}{$k} = self;
        self;
    }

    method setpos($k) {
        self{'array'}[$k] = self;
        self;
    }

    method item {
        if not defined $!item {
            $!item = substr($.matcher.targ, $.from, $.to - $.from);
        }
        $!item;
    }

    submethod BUILD (:$!matcher, :$!from, :$!to, :$capt) {
        self{'hash'} = {} unless self.exists('hash');
        self{'array'} = [] unless self.exists('array');
        if defined $capt {
#            say "BUILDING with ", $capt.WHAT;
            if $capt ~~ MCont {
                for $capt.{'hash'}.kv -> $k, $v {
                    say "Building $k";
                    self<hash>{$k} = $v;  # XXX need to add push semantics here?
                }
                for $capt.{'array'}.kv -> $k, $v {
                    say "Building $k";
                    self<array>{$k} = $v;
                }
            }
        }
    }
}

method capture (MCont $from, StrPos $tpos) {
    MCont.new(
        :matcher(self),
        :from($from.to // 0),
        :to($tpos),
        :capt($from),
        #:positional($from.positional);
    );
}

method capture_rev (StrPos $fpos, MCont $to) {
    MCont.new(
        :matcher(self),
        :from($fpos),
        :to($to.from),
        :capt($to),
        #:positional($to.positional);
    );
}

my $LVL is context = 0;

sub callm ($¢) is export {
    my $lvl = 0;
    while Pugs::Internals::caller(Any,$lvl,"") { $lvl++ }
    say ' ' x $lvl, substr(caller.subname,1), " called at $¢.to()";
    $lvl;
}

sub whats (MCont $r) {
    if $r<hash>.keys {
        say "Named:";
        for $r<hash>.kv -> $k, $v { say " $k =\t{$v.from} {$v.to} {$v.item}" }
    }
    if $r<array>.keys {
        say "Positional:";
        for $r<array>.kv -> $k, $v { say " $k =\t{$v.from} {$v.to} {$v.item}" }
    }
}

sub retm (MCont $r) is export {
    say ' ' x $+LVL, substr(caller.subname,1), " returning $r.from()..$r.to()";
    whats($r);
    $r;
}

method _STARf ($¢, &block) {
    my $LVL is context = callm($¢);

    map { retm($_) },
        self.capture($¢, $¢.to),
        self._PLUSf($¢, &block);
}

method _STARg ($¢, &block) {
    my $LVL is context = callm($¢);

    map { retm($_) }, reverse
        #XXX voodoo fix to prevent bogus stringification
        map { .perl.say; $_ },
            self.capture($¢, $¢.to),
            self._PLUSf($¢, &block);
}

method _STARr ($¢, &block) {
    my $LVL is context = callm($¢);
    my $to = $¢;
    my @all = gather {
        loop {
            my @matches = block($to);  # XXX shouldn't read whole list
            say @matches.perl;
        last unless @matches;
            my $first = @matches[0];  # no backtracking into block on ratchet
            take $first;
            $to = $first;
        }
    }
    my $r = self.capture($¢, $to.to);
    retm($r);
}

method _PLUSf ($¢, &block) {
    my $LVL is context = callm($¢);
    my $x = $¢;

    map { retm($_) },
    gather {
        for block($x) -> $x {
            take map { self.capture($¢, $_.to) }, $x, self._PLUSf($x, &block);
        }
    }
}

method _PLUSg ($¢, &block) {
    my $LVL is context = callm($¢);

    #XXX extra parens to prevent string reverse
    reverse (self._PLUSf($¢, &block),);
}

method _PLUSr ($¢, &block) {
    my $LVL is context = callm($¢);
    my $to = $¢;
    my @all = gather {
        loop {
            my @matches = block($to);  # XXX shouldn't read whole list
        last unless @matches;
            my $first = @matches[0];  # no backtracking into block on ratchet
            #say $matches.perl;
            take $first;
            $to = $first;
        }
    }
    return () unless @all;
    my $r = self.capture($¢, $to.to);
    retm($r);
}

method _OPTr ($¢, &block) {
    my $LVL is context = callm($¢);
    my $x = block($¢)[0]; # ratchet
    my $r = $x // self.capture($¢,$¢.to);
    retm($r);
}

method _OPTg ($¢, &block) {
    my $LVL is context = callm($¢);
    my @x = block($¢);
    map { retm($_) },
        block($¢),
        self.capture($¢,$¢.to);
}

method _OPTf ($¢, &block) {
    my $LVL is context = callm($¢);
    map { retm($_) },
        self.capture($¢,$¢.to),
        block($¢);
}

method _BRACKET ($¢, &block) {
    my $LVL is context = callm($¢);
    map { retm($_) },
        block($¢);
}

method _PAREN ($¢, &block) {
    my $LVL is context = callm($¢);
    map { retm($_) },
        block($¢);
}

method _NOTBEFORE ($¢, &block) {
    my $LVL is context = callm($¢);
    my @all = block($¢);
    return () if @all;  # XXX loses continuation
    return retm(self.capture($¢, $¢.to));
}

method before ($¢, &block) {
    say $¢.WHICH;
    my $LVL is context = callm($¢);
    my @all = block($¢);
    if (@all and @all[0].bool) {
        say "true";
        whats($¢);
        say $¢.WHICH;
        return retm(self.capture($¢, $¢.to));
    }
    return ();
}

method after ($¢, &block) {
    my $LVL is context = callm($¢);
    my $end = self.capture($¢, $¢.to);
    my @all = block($end);          # Make sure .from == .to
    if (@all and @all[0].bool) {
        return retm($end);
    }
    return ();
}

method null ($¢) {
    my $LVL is context = callm($¢);
    return retm(self.capture($¢, $¢.to));
}

method _ASSERT ($¢, &block) {
    my $LVL is context = callm($¢);
    my @all = block($¢);
    if (@all and @all[0].bool) {
        return retm(self.capture($¢, $¢.to));
    }
    return ();
}

method _BINDVAR ($¢, $var is rw, &block) {
    my $LVL is context = callm($¢);
    map { $var := $_; retm($_) },  # XXX doesn't "let"
        block($¢);
}

method _BINDPOS ($¢, $var is rw, &block) {
    my $LVL is context = callm($¢);
    map { retm(.setpos($var)) },
        block($¢);
}

method _BINDNAMED ($¢ is rw, $var is rw, &block) {
    my $LVL is context = callm($¢);
    map { retm(.setname($var)) },
        block($¢);
}

method _EQ ($¢, $s) {
    my $from = $¢.to;
    my $len = $s.chars;
    return True if substr($!targ, $from, $len) eq $s;
    return ();
}

method _EXACT ($¢, $s) {
    my $LVL is context = callm($¢);
    my $from = $¢.to;
    my $len = $s.chars;
    if substr($!targ, $from, $len) eq $s {
        my $r = self.capture($¢, $from+$len);
        retm($r);
    }
    else {
        say "EXACT $s didn't match {substr($!targ,$from,$len)} at $from $len";
        return ();
    }
}

method _EXACT_rev ($¢, $s) {
    my $LVL is context = callm($¢);
    my $len = $s.chars;
    my $from = $¢.from - $len;
    if $from >= 0 and substr($!targ, $from, $len) eq $s {
        my $r = self.capture_rev($from, $¢);
        retm($r);
    }
    else {
        say "vEXACT $s didn't match {substr($!targ,$from,$len)} at $from $len";
        return ();
    }
}

method _DIGIT ($¢) {
    my $LVL is context = callm($¢);
    my $from = $¢.to;
    my $char = substr($!targ, $from, 1);
    if "0" le $char le "9" {
        my $r = self.capture($¢, $from+1);
        return retm($r);
    }
    else {
        say "DIGIT didn't match $char at $from";
        return ();
    }
}

method _DIGIT_rev ($¢) {
    my $LVL is context = callm($¢);
    my $from = $¢.from - 1;
    if $from < 0 {
        say "vDIGIT didn't match $char at $from";
        return ();
    }
    my $char = substr($!targ, $from, 1);
    if "0" le $char le "9" {
        my $r = self.capture_rev($from, $¢);
        return retm($r);
    }
    else {
        say "vDIGIT didn't match $char at $from";
        return ();
    }
}

method _ALNUM ($¢) {
    my $LVL is context = callm($¢);
    my $from = $¢.to;
    my $char = substr($!targ, $from, 1);
    if "0" le $char le "9" or 'A' le $char le 'Z' or 'a' le $char le 'z' or $char eq '_' {
        my $r = self.capture($¢, $from+1);
        return retm($r);
    }
    else {
        say "ALNUM didn't match $char at $from";
        return ();
    }
}

method _ALNUM_rev ($¢) {
    my $LVL is context = callm($¢);
    my $from = $¢.from - 1;
    if $from < 0 {
        say "vALNUM didn't match $char at $from";
        return ();
    }
    my $char = substr($!targ, $from, 1);
    if "0" le $char le "9" or 'A' le $char le 'Z' or 'a' le $char le 'z' or $char eq '_' {
        my $r = self.capture_rev($from, $¢);
        return retm($r);
    }
    else {
        say "vALNUM didn't match $char at $from";
        return ();
    }
}

method alpha ($¢) {
    my $LVL is context = callm($¢);
    my $from = $¢.to;
    my $char = substr($!targ, $from, 1);
    if 'A' le $char le 'Z' or 'a' le $char le 'z' or $char eq '_' {
        my $r = self.capture($¢, $from+1);
        return retm($r);
    }
    else {
        say "alpha didn't match $char at $from";
        return ();
    }
}

method _SPACE ($¢) {
    my $LVL is context = callm($¢);
    my $from = $¢.to;
    my $char = substr($!targ, $from, 1);
    if $char eq " " | "\t" | "\r" | "\n" | "\f" {
        my $r = self.capture($¢, $from+1);
        return retm($r);
    }
    else {
        say "SPACE didn't match $char at $from";
        return ();
    }
}

method _SPACE_rev ($¢) {
    my $LVL is context = callm($¢);
    my $from = $¢.from - 1;
    if $from < 0 {
        say "vSPACE didn't match $char at $from";
        return ();
    }
    my $char = substr($!targ, $from, 1);
    if $char eq " " | "\t" | "\r" | "\n" | "\f" {
        my $r = self.capture_rev($from, $¢);
        return retm($r);
    }
    else {
        say "vSPACE didn't match $char at $from";
        return ();
    }
}

method _HSPACE ($¢) {
    my $LVL is context = callm($¢);
    my $from = $¢.to;
    my $char = substr($!targ, $from, 1);
    if $char eq " " | "\t" | "\r" {
        my $r = self.capture($¢, $from+1);
        return retm($r);
    }
    else {
        say "HSPACE didn't match $char at $from";
        return ();
    }
}

method _HSPACE_rev ($¢) {
    my $LVL is context = callm($¢);
    my $from = $¢.from - 1;
    if $from < 0 {
        say "vHSPACE didn't match $char at $from";
        return ();
    }
    my $char = substr($!targ, $from, 1);
    if $char eq " " | "\t" | "\r" {
        my $r = self.capture_rev($from, $¢);
        return retm($r);
    }
    else {
        say "vHSPACE didn't match $char at $from";
        return ();
    }
}

method _VSPACE ($¢) {
    my $LVL is context = callm($¢);
    my $from = $¢.to;
    my $char = substr($!targ, $from, 1);
    if $char eq "\n" | "\f" {
        my $r = self.capture($¢, $from+1);
        return retm($r);
    }
    else {
        say "VSPACE didn't match $char at $from";
        return ();
    }
}

method _VSPACE_rev ($¢) {
    my $LVL is context = callm($¢);
    my $from = $¢.from - 1;
    if $from < 0 {
        say "vVSPACE didn't match $char at $from";
        return ();
    }
    my $char = substr($!targ, $from, 1);
    if $char eq "\n" | "\f" {
        my $r = self.capture_rev($from, $¢);
        return retm($r);
    }
    else {
        say "vVSPACE didn't match $char at $from";
        return ();
    }
}

method _ANY ($¢) {
    my $LVL is context = callm($¢);
    my $from = $¢.to;
    if $from < $!targ.chars {
        retm(self.capture($¢, $from+1));
    }
    else {
        say "ANY didn't match anything at $from";
        return ();
    }
}

method _BOS ($¢) {
    my $LVL is context = callm($¢);
    my $from = $¢.to;
    if $from == 0 {
        retm(self.capture($¢, $¢.to));
    }
    else {
        return ();
    }
}

method _BOL ($¢) {
    my $LVL is context = callm($¢);
    my $from = $¢.to;
    # XXX should define in terms of BOL or after vVSPACE
    if $from == 0 or substr($!targ, $from-1, 1) eq "\n" or substr($!targ, $from-2, 2) eq "\r\n" {
        retm(self.capture($¢, $¢.to));
    }
    else {
        return ();
    }
}

method _EOS ($¢) {
    my $LVL is context = callm($¢);
    my $from = $¢.to;
    if $from == $!targ.chars {
        retm(self.capture($¢, $¢.to));
    }
    else {
        return ();
    }
}

method _REDUCE ($¢, $tag) {
    my $LVL is context = callm($¢);
    say "Success $tag from $+FROM.to() to $¢.to()";
    whats($¢);
    $¢;
#    self.capture($¢, $¢.to);
}

method _COMMITBRANCH ($¢) {
    my $LVL is context = callm($¢);
    say "Commit branch to $¢.to()\n";
#    self.capture($¢, $¢);  # XXX currently noop
    $¢;
}

method _COMMITRULE ($¢) {
    my $LVL is context = callm($¢);
    say "Commit rule to $¢.to()\n";
#    self.capture($¢, $¢);  # XXX currently noop
    $¢;
}

method commit ($¢) {
    my $LVL is context = callm($¢);
    say "Commit match to $¢.to()\n";
#    self.capture($¢, $¢);  # XXX currently noop
    $¢;
}

method fail ($¢, $m) { die $m }

