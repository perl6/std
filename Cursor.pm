class Cursor;

my $VERBOSE = 1;

# XXX still full of ASCII assumptions

has $.targ;

has Bool $.bool is rw = 1;
has StrPos $.from = 0;
has StrPos $.to = 0;
has StrPos $.cursor = 0;
has Matcher $.prior;
has %.mykeys;
has Str $.name;
has $!item;

method setname($k) {
    $!name = ~$k;
    self;
}

method item {
    if not defined $!item {
        $!item = substr($.targ, $.from, $.to - $.from);
    }
    $!item;
}

method matchify {
    my %bindings;
    my Matcher $m;
    my $next;
    loop ($m = $!prior; $m; $m = $next) {
        $next = $m.prior;
        undefine $m.prior;
        my $n = $m.name;
        if not %bindings{$n} {
            %bindings{$n} = [];
#                %.mykeys = Hash.new unless defined %!mykeys;
            %.mykeys{$n}++;
        }
        unshift %bindings{$n}, $m;
    }
    for %bindings.kv -> $k, $v {
        self{$k} = $v;
        # XXX alas, gets lost in the next capture...
        say "copied $k ", $v;
    }
    undefine $!prior;
    self.item;
    self;
}

method capture_all (StrPos $fpos, StrPos $tpos) {
    self.new(
        :targ(self.targ),
        :from($fpos),
        :to($tpos),
        :cursor($tpos),
        :prior(defined self.name ?? self !! self.prior),
        :mykeys(hash(self.mykeys.pairs)),
    );
}

method capture (StrPos $tpos) {
    self.new(
        :targ(self.targ),
        :from(self.cursor // 0),
        :to($tpos),
        :cursor($tpos),
        :prior(defined self.name ?? self !! self.prior),
        :mykeys(hash(self.mykeys.pairs)),
    );
}

method capture_rev (StrPos $fpos) {
    self.new(
        :targ(self.targ),
        :cursor($fpos),
        :from($fpos),
        :to(self.from),
        :prior(defined self.name ?? self !! self.prior),
        :mykeys(hash(self.mykeys.pairs)),
    );
}

my $LVL is context = 0;

method callm () is export {
    my $lvl = 0;
    while Pugs::Internals::caller(Any,$lvl,"") { $lvl++ }
    say ' ' x $lvl, substr(caller.subname,1), " called at {self.cursor}";
    $lvl;
}

method whats () {
    my @k = self.mykeys.keys;
    say "  $.WHICH ===> $.from $.to $.item (@k[])";
    for @k -> $k {
        say "   $k => {self{$k}.perl}";
    }
}

method retm () {
    say "Returning non-Cursor: self.WHAT" unless self ~~ Cursor;
    say ' ' x $+LVL, substr(caller.subname,1), " returning {self.from}..{self.to}";
    self.whats();
    self;
}

method _STARf (&block) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;

    map { .retm },
        self.capture($¢),
        self._PLUSf(&block);
}

method _STARg (&block) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;

    map { .retm }, reverse
        #XXX voodoo fix to prevent bogus stringification
        map { .perl.say; $_ },
            self.capture($¢),
            self._PLUSf(&block);
}

method _STARr (&block) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
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
    my $r = self.capture($to.to);
    $r.retm;
}

method _PLUSf (&block) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    my $x = $/;

    map { .retm },
    gather {
        for block($x) -> $x {
            take map { self.capture($_.to) }, $x, self._PLUSf($x, &block);
        }
    }
}

method _PLUSg (&block) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;

    reverse self._PLUSf(&block);
}

method _PLUSr (&block) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
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
    my $r = self.capture($to.to);
    $r.retm;
}

method _OPTr (&block) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    my $x = block($/)[0]; # ratchet
    my $r = $x // self.capture($¢);
    $r.retm;
}

method _OPTg (&block) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    my @x = block($/);
    map { .retm },
        block($/),
        self.capture($¢);
}

method _OPTf (&block) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    map { .retm },
        self.capture($¢),
        block($/);
}

method _BRACKET (&block) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    map { .retm },
        block($/);
}

method _PAREN (&block) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    map { .matchify.retm },
        block($/);
}

method _NOTBEFORE (&block) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    my @all = block($/);
    return () if @all;  # XXX loses continuation
    return self.capture($¢).retm;
}

method before (&block) {
    say $¢.WHICH;
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    my @all = block($/);
    if (@all and @all[0].bool) {
        say "true";
        self.whats;
        say $¢.WHICH;
        return self.capture($¢).retm;
    }
    return ();
}

method after (&block) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    my $end = self.capture($¢);
    my @all = block($end);          # Make sure .from == .to
    if (@all and @all[0].bool) {
        return $end.retm;
    }
    return ();
}

method null () {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    return self.capture($¢).retm;
}

method _ASSERT (&block) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    my @all = block($/);
    if (@all and @all[0].bool) {
        return self.capture($¢).retm;
    }
    return ();
}

method _BINDVAR ($var is rw, &block) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    map { $var := $_; .retm },  # XXX doesn't "let"
        block($/);
}

method _BINDPOS ($var is rw, &block) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    map { .setname($var).retm },
        block($/);
}

method _BINDNAMED ($var is rw, &block) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    map { .setname($var).retm },
        block($/);
}

# fast rejection of current prefix
method _EQ ($¢, $s) {
    my $len = $s.chars;
    return True if substr($!targ, $¢, $len) eq $s;
    return ();
}

method _EXACT ($s) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    my $len = $s.chars;
    if substr($!targ, $¢, $len) eq $s {
        say "EXACT $s matched {substr($!targ,$¢,$len)} at $¢ $len";
        my $r = self.capture($¢+$len);
        $r.retm;
    }
    else {
        say "EXACT $s didn't match {substr($!targ,$¢,$len)} at $¢ $len";
        return ();
    }
}

method _EXACT_rev ($s) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    my $len = $s.chars;
    my $from = $/.from - $len;
    if $from >= 0 and substr($!targ, $from, $len) eq $s {
        my $r = self.capture_rev($from);
        $r.retm;
    }
    else {
        say "vEXACT $s didn't match {substr($!targ,$from,$len)} at $from $len";
        return ();
    }
}

method _DIGIT () {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    my $char = substr($!targ, $¢, 1);
    if "0" le $char le "9" {
        my $r = self.capture($¢+1);
        return $r.retm;
    }
    else {
        say "DIGIT didn't match $char at $¢";
        return ();
    }
}

method _DIGIT_rev () {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    my $from = $/.from - 1;
    if $from < 0 {
        say "vDIGIT didn't match $char at $from";
        return ();
    }
    my $char = substr($!targ, $from, 1);
    if "0" le $char le "9" {
        my $r = self.capture_rev($from);
        return $r.retm;
    }
    else {
        say "vDIGIT didn't match $char at $from";
        return ();
    }
}

method _ALNUM () {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    my $char = substr($!targ, $¢, 1);
    if "0" le $char le "9" or 'A' le $char le 'Z' or 'a' le $char le 'z' or $char eq '_' {
        my $r = self.capture($¢+1);
        return $r.retm;
    }
    else {
        say "ALNUM didn't match $char at $¢";
        return ();
    }
}

method _ALNUM_rev () {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    my $from = $/.from - 1;
    if $from < 0 {
        say "vALNUM didn't match $char at $from";
        return ();
    }
    my $char = substr($!targ, $from, 1);
    if "0" le $char le "9" or 'A' le $char le 'Z' or 'a' le $char le 'z' or $char eq '_' {
        my $r = self.capture_rev($from);
        return $r.retm;
    }
    else {
        say "vALNUM didn't match $char at $from";
        return ();
    }
}

method alpha () {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    my $char = substr($!targ, $¢, 1);
    if 'A' le $char le 'Z' or 'a' le $char le 'z' or $char eq '_' {
        my $r = self.capture($¢+1);
        return $r.retm;
    }
    else {
        say "alpha didn't match $char at $¢";
        return ();
    }
}

method _SPACE () {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    my $char = substr($!targ, $¢, 1);
    if $char eq " " | "\t" | "\r" | "\n" | "\f" {
        my $r = self.capture($¢+1);
        return $r.retm;
    }
    else {
        say "SPACE didn't match $char at $¢";
        return ();
    }
}

method _SPACE_rev () {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    my $from = $/.from - 1;
    if $from < 0 {
        say "vSPACE didn't match $char at $from";
        return ();
    }
    my $char = substr($!targ, $from, 1);
    if $char eq " " | "\t" | "\r" | "\n" | "\f" {
        my $r = self.capture_rev($from);
        return $r.retm;
    }
    else {
        say "vSPACE didn't match $char at $from";
        return ();
    }
}

method _HSPACE () {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    my $char = substr($!targ, $¢, 1);
    if $char eq " " | "\t" | "\r" {
        my $r = self.capture($¢+1);
        return $r.retm;
    }
    else {
        say "HSPACE didn't match $char at $¢";
        return ();
    }
}

method _HSPACE_rev () {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    my $from = $/.from - 1;
    if $from < 0 {
        say "vHSPACE didn't match $char at $from";
        return ();
    }
    my $char = substr($!targ, $from, 1);
    if $char eq " " | "\t" | "\r" {
        my $r = self.capture_rev($from);
        return $r.retm;
    }
    else {
        say "vHSPACE didn't match $char at $from";
        return ();
    }
}

method _VSPACE () {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    my $char = substr($!targ, $¢, 1);
    if $char eq "\n" | "\f" {
        my $r = self.capture($¢+1);
        return $r.retm;
    }
    else {
        say "VSPACE didn't match $char at $¢";
        return ();
    }
}

method _VSPACE_rev () {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    my $from = $/.from - 1;
    if $from < 0 {
        say "vVSPACE didn't match $char at $from";
        return ();
    }
    my $char = substr($!targ, $from, 1);
    if $char eq "\n" | "\f" {
        my $r = self.capture_rev($from);
        return $r.retm;
    }
    else {
        say "vVSPACE didn't match $char at $from";
        return ();
    }
}

method _ANY () {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    if $¢ < $!targ.chars {
        self.capture($¢+1).retm;
    }
    else {
        say "ANY didn't match anything at $¢";
        return ();
    }
}

method _BOS () {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    if $¢ == 0 {
        self.capture($¢).retm;
    }
    else {
        return ();
    }
}

method _BOL () {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    # XXX should define in terms of BOL or after vVSPACE
    if $¢ == 0 or substr($!targ, $¢-1, 1) eq "\n" or substr($!targ, $¢-2, 2) eq "\r\n" {
        self.capture($¢).retm;
    }
    else {
        return ();
    }
}

method _EOS () {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    if $¢ == $!targ.chars {
        self.capture($¢).retm;
    }
    else {
        return ();
    }
}

method _REDUCE ($tag) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    say "Success $tag from $+FROM to $¢";
    self.whats;
    $/;
#    self.capture($¢);
}

method _COMMITBRANCH () {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    say "Commit branch to $¢\n";
#    self.capture($¢);  # XXX currently noop
    $/;
}

method _COMMITRULE () {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    say "Commit rule to $¢\n";
#    self.capture($¢);  # XXX currently noop
    $/;
}

method commit () {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.cursor;
    say "Commit match to $¢\n";
#    self.capture($¢);  # XXX currently noop
    $/;
}

method fail ($m) { die $m }

