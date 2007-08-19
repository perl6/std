grammar Metholated;

has $.targ;

our class Match {
    has Bool $.bool = 1;
    has StrPos $.from;
    has StrPos $.to;
    has $.item;
    has @.list;
    has %.hash;
}

method capture ($from, $to) {
    Match.new(
        :from($from),
        :to($to),
        :item(substr($!targ,$from,$to - $from))
    );
}

my $LVL is context = 0;

sub callm ($¢) is export {
    my $lvl = 0;
    while Pugs::Internals::caller(Any,$lvl,"") { $lvl++ }
    say ' ' x $lvl, substr(caller.subname,1), " called at $¢";
    $lvl;
}

sub retm (Match $r) is export {
    say ' ' x $+LVL, substr(caller.subname,1), " returning $r.from()..$r.to()";
    $r;
}

method STARf ($¢, &block) {
    my $LVL is context = callm($¢);

    map { retm($_) },
        self.capture($¢, $¢),
        self.PLUSf($¢, &block);
}

method STARg ($¢, &block) {
    my $LVL is context = callm($¢);

    map { retm($_) }, reverse
        self.capture($¢, $¢),
        self.PLUSf($¢, &block);
}

method STARr ($¢, &block) {
    my $LVL is context = callm($¢);
    my $to = $¢;
    my @all = gather {
        loop {
            my @matches = block($to);  # XXX shouldn't read whole list
            say @matches.perl;
        last unless @matches;
            my $first = @matches[0];  # no backtracking into block on ratchet
            take $first;
            $to = $first.to;
        }
    }
    my $r = self.capture($¢, $to);
    retm($r);
}

method PLUSf ($¢, &block) {
    my $LVL is context = callm($¢);
    my $x = $¢;

    map { retm($_) },
    gather {
        for block($x) -> $x {
            take map { self.capture($¢, $_.to) }, $x, self.PLUSf($x.to, &block);
        }
    }
}

method PLUSg ($¢, &block) {
    my $LVL is context = callm($¢);

    reverse self.PLUSf($¢, &block);
}

method PLUSr ($¢, &block) {
    my $LVL is context = callm($¢);
    my $to = $¢;
    my @all = gather {
        loop {
            my @matches = block($to);  # XXX shouldn't read whole list
            say @matches.perl;
        last unless @matches;
            my $first = @matches[0];  # no backtracking into block on ratchet
            take $first;
            $to = $first.to;
        }
    }
    return () unless @all;
    my $r = self.capture($¢, $to);
    retm($r);
}

method OPTr ($¢, &block) {
    my $LVL is context = callm($¢);
    my $x = block($¢)[0]; # ratchet
    my $r = $x // self.capture($¢,$¢);
    retm($r);
}

method OPTg ($¢, &block) {
    my $LVL is context = callm($¢);
    my @x = block($¢);
    map { retm($_) },
        block($¢),
        self.capture($¢,$¢);
}

method OPTf ($¢, &block) {
    my $LVL is context = callm($¢);
    map { retm($_) },
        self.capture($¢,$¢),
        block($¢);
}

method BRACKET ($¢, &block) {
    my $LVL is context = callm($¢);
    map { retm($_) },
        block($¢);
}

method PAREN ($¢, &block) {
    my $LVL is context = callm($¢);
    map { retm($_) },
        block($¢);
}

method NOTBEFORE ($¢, &block) {
    my $LVL is context = callm($¢);
    my @all = block($¢);
    return () if @all;  # XXX loses captures?
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

method ASSERT ($¢, &block) {
    my $LVL is context = callm($¢);
    my @all = block($¢);
    if (@all and @all[0].bool) {
        return retm(self.capture($¢, $¢));
    }
    return ();
}

method BIND ($¢, $var is rw, &block) {
    my $LVL is context = callm($¢);
    map { $var := $_; retm($_) },
        block($¢);
}

method EXACT ($¢, $s) {
    my $LVL is context = callm($¢);
    my $len = $s.chars;
    if substr($!targ, $¢, $len) eq $s {
        my $r = self.capture($¢, $¢+$len);
        retm($r);
    }
    else {
        say "EXACT $s didn't match {substr($!targ,$¢,$len)} at $¢ $len";
        return ();
    }
}

method DIGIT ($¢) {
    my $LVL is context = callm($¢);
    my $char = substr($!targ, $¢, 1);
    if "0" le $char le "9" {
        my $r = self.capture($¢, $¢+1);
        return retm($r);
    }
    else {
        say "DIGIT didn't match {substr($!targ,$¢,1)} at $¢";
        return ();
    }
}

method ANY ($¢) {
    my $LVL is context = callm($¢);
    if $¢ < $!targ.chars {
        retm(self.capture($¢, $¢+1));
    }
    else {
        say "ANY didn't match anything at $¢";
        return ();
    }
}

method REDUCE ($¢, $tag) {
    my $LVL is context = callm($¢);
    say "Success $tag from $+FROM to $¢\n";
    self.capture($¢, $¢);
}

