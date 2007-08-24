grammar Metholated;

my $VERBOSE = 0;

# XXX still full of ASCII assumptions

has $.targ;

our class MCont does Hash {
    has Metholated $.matcher;
    has Bool $.bool is rw = 1;
    has StrPos $.from = 0;
    has StrPos $.to = 0;
    has MCont $.prior;
    has Str $.name;
    has $!item;

    method setname($k) {
        $!name = ~$k;
        self;
    }

    method item {
        if not defined $!item {
            $!item = substr($.matcher.targ, $.from, $.to - $.from);
        }
        $!item;
    }

}

method capture_all (MCont $prior, StrPos $fpos, StrPos $tpos) {
    MCont.new(
        :matcher(self),
        :from($fpos),
        :to($tpos),
        :prior(defined $prior.name ?? $prior !! $prior.prior),
    );
}

method capture (MCont $prior, StrPos $tpos) {
    MCont.new(
        :matcher(self),
        :from($prior.to // 0),
        :to($tpos),
        :prior(defined $prior.name ?? $prior !! $prior.prior),
    );
}

method capture_rev (StrPos $fpos, MCont $prior) {
    MCont.new(
        :matcher(self),
        :from($fpos),
        :to($prior.from),
        :prior(defined $prior.name ?? $prior !! $prior.prior),
    );
}

my $LVL is context = 0;

sub callm ($/) is export {
    my $¢ = $/.to;
    my $lvl = 0;
    while Pugs::Internals::caller(Any,$lvl,"") { $lvl++ }
    say ' ' x $lvl, substr(caller.subname,1), " called at $¢";
    ($¢, $lvl);
}

sub whats (MCont $r) {
    say $r.perl if $VERBOSE;
}

sub retm (MCont $r) is export {
    say ' ' x $+LVL, substr(caller.subname,1), " returning $r.from()..$r.to()";
    whats($r);
    $r;
}

method _STARf ($/, &block) {
    my ($¢, $LVL is context) = callm($/);

    map { retm($_) },
        self.capture($/, $¢),
        self._PLUSf($/, &block);
}

method _STARg ($/, &block) {
    my ($¢, $LVL is context) = callm($/);

    map { retm($_) }, reverse
        #XXX voodoo fix to prevent bogus stringification
        map { .perl.say; $_ },
            self.capture($/, $¢),
            self._PLUSf($/, &block);
}

method _STARr ($/, &block) {
    my ($¢, $LVL is context) = callm($/);
    my $to = $/;
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
    my $r = self.capture($/, $to.to);
    retm($r);
}

method _PLUSf ($/, &block) {
    my ($¢, $LVL is context) = callm($/);
    my $x = $/;

    map { retm($_) },
    gather {
        for block($x) -> $x {
            take map { self.capture($/, $_.to) }, $x, self._PLUSf($x, &block);
        }
    }
}

method _PLUSg ($/, &block) {
    my ($¢, $LVL is context) = callm($/);

    reverse self._PLUSf($/, &block);
}

method _PLUSr ($/, &block) {
    my ($¢, $LVL is context) = callm($/);
    my $to = $/;
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
    my $r = self.capture($/, $to.to);
    retm($r);
}

method _OPTr ($/, &block) {
    my ($¢, $LVL is context) = callm($/);
    my $x = block($/)[0]; # ratchet
    my $r = $x // self.capture($/,$¢);
    retm($r);
}

method _OPTg ($/, &block) {
    my ($¢, $LVL is context) = callm($/);
    my @x = block($/);
    map { retm($_) },
        block($/),
        self.capture($/,$¢);
}

method _OPTf ($/, &block) {
    my ($¢, $LVL is context) = callm($/);
    map { retm($_) },
        self.capture($/,$¢),
        block($/);
}

method _BRACKET ($/, &block) {
    my ($¢, $LVL is context) = callm($/);
    map { retm($_) },
        block($/);
}

method _PAREN ($/, &block) {
    my ($¢, $LVL is context) = callm($/);
    map { retm($_) },
        block($/);
}

method _NOTBEFORE ($/, &block) {
    my ($¢, $LVL is context) = callm($/);
    my @all = block($/);
    return () if @all;  # XXX loses continuation
    return retm(self.capture($/, $¢));
}

method before ($/, &block) {
    say $¢.WHICH;
    my ($¢, $LVL is context) = callm($/);
    my @all = block($/);
    if (@all and @all[0].bool) {
        say "true";
        whats($/);
        say $¢.WHICH;
        return retm(self.capture($/, $¢));
    }
    return ();
}

method after ($/, &block) {
    my ($¢, $LVL is context) = callm($/);
    my $end = self.capture($/, $¢);
    my @all = block($end);          # Make sure .from == .to
    if (@all and @all[0].bool) {
        return retm($end);
    }
    return ();
}

method null ($/) {
    my ($¢, $LVL is context) = callm($/);
    return retm(self.capture($/, $¢));
}

method _ASSERT ($/, &block) {
    my ($¢, $LVL is context) = callm($/);
    my @all = block($/);
    if (@all and @all[0].bool) {
        return retm(self.capture($/, $¢));
    }
    return ();
}

method _BINDVAR ($/, $var is rw, &block) {
    my ($¢, $LVL is context) = callm($/);
    map { $var := $_; retm($_) },  # XXX doesn't "let"
        block($/);
}

method _BINDPOS ($/, $var is rw, &block) {
    my ($¢, $LVL is context) = callm($/);
    map { retm(.setname($var)) },
        block($/);
}

method _BINDNAMED ($/, $var is rw, &block) {
    my ($¢, $LVL is context) = callm($/);
    map { retm(.setname($var)) },
        block($/);
}

# fast rejection of current prefix
method _EQ ($¢, $s) {
    my $len = $s.chars;
    return True if substr($!targ, $¢, $len) eq $s;
    return ();
}

method _EXACT ($/, $s) {
    my ($¢, $LVL is context) = callm($/);
    my $len = $s.chars;
    if substr($!targ, $¢, $len) eq $s {
        my $r = self.capture($/, $¢+$len);
        retm($r);
    }
    else {
        say "EXACT $s didn't match {substr($!targ,$¢,$len)} at $¢ $len";
        return ();
    }
}

method _EXACT_rev ($/, $s) {
    my ($¢, $LVL is context) = callm($/);
    my $len = $s.chars;
    my $from = $/.from - $len;
    if $from >= 0 and substr($!targ, $from, $len) eq $s {
        my $r = self.capture_rev($from, $/);
        retm($r);
    }
    else {
        say "vEXACT $s didn't match {substr($!targ,$from,$len)} at $from $len";
        return ();
    }
}

method _DIGIT ($/) {
    my ($¢, $LVL is context) = callm($/);
    my $char = substr($!targ, $¢, 1);
    if "0" le $char le "9" {
        my $r = self.capture($/, $¢+1);
        return retm($r);
    }
    else {
        say "DIGIT didn't match $char at $¢";
        return ();
    }
}

method _DIGIT_rev ($/) {
    my ($¢, $LVL is context) = callm($/);
    my $from = $/.from - 1;
    if $from < 0 {
        say "vDIGIT didn't match $char at $from";
        return ();
    }
    my $char = substr($!targ, $from, 1);
    if "0" le $char le "9" {
        my $r = self.capture_rev($from, $/);
        return retm($r);
    }
    else {
        say "vDIGIT didn't match $char at $from";
        return ();
    }
}

method _ALNUM ($/) {
    my ($¢, $LVL is context) = callm($/);
    my $char = substr($!targ, $¢, 1);
    if "0" le $char le "9" or 'A' le $char le 'Z' or 'a' le $char le 'z' or $char eq '_' {
        my $r = self.capture($/, $¢+1);
        return retm($r);
    }
    else {
        say "ALNUM didn't match $char at $¢";
        return ();
    }
}

method _ALNUM_rev ($/) {
    my ($¢, $LVL is context) = callm($/);
    my $from = $/.from - 1;
    if $from < 0 {
        say "vALNUM didn't match $char at $from";
        return ();
    }
    my $char = substr($!targ, $from, 1);
    if "0" le $char le "9" or 'A' le $char le 'Z' or 'a' le $char le 'z' or $char eq '_' {
        my $r = self.capture_rev($from, $/);
        return retm($r);
    }
    else {
        say "vALNUM didn't match $char at $from";
        return ();
    }
}

method alpha ($/) {
    my ($¢, $LVL is context) = callm($/);
    my $char = substr($!targ, $¢, 1);
    if 'A' le $char le 'Z' or 'a' le $char le 'z' or $char eq '_' {
        my $r = self.capture($/, $¢+1);
        return retm($r);
    }
    else {
        say "alpha didn't match $char at $¢";
        return ();
    }
}

method _SPACE ($/) {
    my ($¢, $LVL is context) = callm($/);
    my $char = substr($!targ, $¢, 1);
    if $char eq " " | "\t" | "\r" | "\n" | "\f" {
        my $r = self.capture($/, $¢+1);
        return retm($r);
    }
    else {
        say "SPACE didn't match $char at $¢";
        return ();
    }
}

method _SPACE_rev ($/) {
    my ($¢, $LVL is context) = callm($/);
    my $from = $/.from - 1;
    if $from < 0 {
        say "vSPACE didn't match $char at $from";
        return ();
    }
    my $char = substr($!targ, $from, 1);
    if $char eq " " | "\t" | "\r" | "\n" | "\f" {
        my $r = self.capture_rev($from, $/);
        return retm($r);
    }
    else {
        say "vSPACE didn't match $char at $from";
        return ();
    }
}

method _HSPACE ($/) {
    my ($¢, $LVL is context) = callm($/);
    my $char = substr($!targ, $¢, 1);
    if $char eq " " | "\t" | "\r" {
        my $r = self.capture($/, $¢+1);
        return retm($r);
    }
    else {
        say "HSPACE didn't match $char at $¢";
        return ();
    }
}

method _HSPACE_rev ($/) {
    my ($¢, $LVL is context) = callm($/);
    my $from = $/.from - 1;
    if $from < 0 {
        say "vHSPACE didn't match $char at $from";
        return ();
    }
    my $char = substr($!targ, $from, 1);
    if $char eq " " | "\t" | "\r" {
        my $r = self.capture_rev($from, $/);
        return retm($r);
    }
    else {
        say "vHSPACE didn't match $char at $from";
        return ();
    }
}

method _VSPACE ($/) {
    my ($¢, $LVL is context) = callm($/);
    my $¢ = $¢;
    my $char = substr($!targ, $¢, 1);
    if $char eq "\n" | "\f" {
        my $r = self.capture($/, $¢+1);
        return retm($r);
    }
    else {
        say "VSPACE didn't match $char at $¢";
        return ();
    }
}

method _VSPACE_rev ($/) {
    my ($¢, $LVL is context) = callm($/);
    my $from = $/.from - 1;
    if $from < 0 {
        say "vVSPACE didn't match $char at $from";
        return ();
    }
    my $char = substr($!targ, $from, 1);
    if $char eq "\n" | "\f" {
        my $r = self.capture_rev($from, $/);
        return retm($r);
    }
    else {
        say "vVSPACE didn't match $char at $from";
        return ();
    }
}

method _ANY ($/) {
    my ($¢, $LVL is context) = callm($/);
    if $¢ < $!targ.chars {
        retm(self.capture($/, $¢+1));
    }
    else {
        say "ANY didn't match anything at $¢";
        return ();
    }
}

method _BOS ($/) {
    my ($¢, $LVL is context) = callm($/);
    if $¢ == 0 {
        retm(self.capture($/, $¢));
    }
    else {
        return ();
    }
}

method _BOL ($/) {
    my ($¢, $LVL is context) = callm($/);
    # XXX should define in terms of BOL or after vVSPACE
    if $¢ == 0 or substr($!targ, $¢-1, 1) eq "\n" or substr($!targ, $¢-2, 2) eq "\r\n" {
        retm(self.capture($/, $¢));
    }
    else {
        return ();
    }
}

method _EOS ($/) {
    my ($¢, $LVL is context) = callm($/);
    if $¢ == $!targ.chars {
        retm(self.capture($/, $¢));
    }
    else {
        return ();
    }
}

method _REDUCE ($/, $tag) {
    my ($¢, $LVL is context) = callm($/);
    say "Success $tag from $+FROM to $¢";
    whats($/);
    $/;
#    self.capture($/, $¢);
}

method _COMMITBRANCH ($/) {
    my ($¢, $LVL is context) = callm($/);
    say "Commit branch to $¢\n";
#    self.capture($/, $¢);  # XXX currently noop
    $/;
}

method _COMMITRULE ($/) {
    my ($¢, $LVL is context) = callm($/);
    say "Commit rule to $¢\n";
#    self.capture($/, $¢);  # XXX currently noop
    $/;
}

method commit ($/) {
    my ($¢, $LVL is context) = callm($/);
    say "Commit match to $¢\n";
#    self.capture($/, $¢);  # XXX currently noop
    $/;
}

method fail ($/, $m) { die $m }

