grammar Metholated;

has $.targ;

our class MCont {
    has Metholated $.matcher;
    has Bool $.bool = 1;
    has StrPos $.from;
    has StrPos $.to;
    has $!item;
    has @.list;
    has %.hash;

    method item {
        if not defined $!item {
            $!item = substr($.matcher.targ, $.from, $.to - $.from);
        }
        $!item;
    }
}

method capture ($fpos, $tpos) {
    my $from = $fpos ~~ MCont ?? $fpos.to !! $fpos;
    my $to   = $tpos ~~ MCont ?? $tpos.to !! $tpos;
    MCont.new(
        :matcher(self),
        :from($from),
        :to($to),
    );
}

my $LVL is context = 0;

sub callm ($¢) is export {
    my $lvl = 0;
    while Pugs::Internals::caller(Any,$lvl,"") { $lvl++ }
    say ' ' x $lvl, substr(caller.subname,1), " called at $¢.to()";
    $lvl;
}

sub retm (Match $r) is export {
    say ' ' x $+LVL, substr(caller.subname,1), " returning $r.from()..$r.to()";
    $r;
}

method _STARf ($¢, &block) {
    my $LVL is context = callm($¢);

    map { retm($_) },
        self.capture($¢, $¢),
        self.PLUSf($¢, &block);
}

method _STARg ($¢, &block) {
    my $LVL is context = callm($¢);

    map { retm($_) }, reverse
        self.capture($¢, $¢),
        self.PLUSf($¢, &block);
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
    my $r = self.capture($¢, $to);
    retm($r);
}

method _PLUSf ($¢, &block) {
    my $LVL is context = callm($¢);
    my $x = $¢;

    map { retm($_) },
    gather {
        for block($x) -> $x {
            take map { self.capture($¢, $_) }, $x, self.PLUSf($x, &block);
        }
    }
}

method _PLUSg ($¢, &block) {
    my $LVL is context = callm($¢);

    reverse self.PLUSf($¢, &block);
}

method _PLUSr ($¢, &block) {
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
    return () unless @all;
    my $r = self.capture($¢, $to);
    retm($r);
}

method _OPTr ($¢, &block) {
    my $LVL is context = callm($¢);
    my $x = block($¢)[0]; # ratchet
    my $r = $x // self.capture($¢,$¢);
    retm($r);
}

method _OPTg ($¢, &block) {
    my $LVL is context = callm($¢);
    my @x = block($¢);
    map { retm($_) },
        block($¢),
        self.capture($¢,$¢);
}

method _OPTf ($¢, &block) {
    my $LVL is context = callm($¢);
    map { retm($_) },
        self.capture($¢,$¢),
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
    return () if @all;
    return retm(self.capture($¢, $¢));
}

method before ($¢, &block) {
    my $LVL is context = callm($¢);
    my @all = block($¢);
    if (@all and @all[0].bool) {
        return retm(self.capture($¢, $¢));
    }
    return ();
}

method _ASSERT ($¢, &block) {
    my $LVL is context = callm($¢);
    my @all = block($¢);
    if (@all and @all[0].bool) {
        return retm(self.capture($¢, $¢));
    }
    return ();
}

method _BIND ($¢, $var is rw, &block) {
    my $LVL is context = callm($¢);
    map { $var := $_; retm($_) },
        block($¢);
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

method _DIGIT ($¢) {
    my $LVL is context = callm($¢);
    my $from = $¢.to;
    my $char = substr($!targ, $from, 1);
    if "0" le $char le "9" {
        my $r = self.capture($¢, $from+1);
        return retm($r);
    }
    else {
        say "DIGIT didn't match {substr($!targ,$from,1)} at $from";
        return ();
    }
}

method _ANY ($¢) {
    my $LVL is context = callm($¢);
    my $from = $¢.to;
    if $from < $!targ.chars {
        retm(self.capture($from, $from+1));
    }
    else {
        say "ANY didn't match anything at $from";
        return ();
    }
}

method _REDUCE ($¢, $tag) {
    my $LVL is context = callm($¢);
    say "Success $tag from $+FROM to $¢.to()\n";
    self.capture($¢, $¢);
}

