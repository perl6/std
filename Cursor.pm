class Cursor;

my $VERBOSE = 1;

# XXX still full of ASCII assumptions

has $.targ;

has Bool $.bool is rw = 1;
has StrPos $.from = 0;
has StrPos $.to = 0;
has StrPos $.pos = 0;
has Cursor $.prior;
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
    my Cursor $m;
    my $next;
    say "MATCHIFY";
    loop ($m = $!prior; $m; $m = $next) {
        $next = $m.prior;
        undefine $m.prior;
        my $n = $m.name;
        say "MATCHIFY $n";
        if not %bindings{$n} {
            %bindings{$n} = [];
#                %.mykeys = Hash.new unless defined %!mykeys;
            %.mykeys{$n}++;
        }
        unshift %bindings{$n}, $m;
    }
    for %bindings.kv -> $k, $v {
        self{$k} = $v;
        # XXX alas, gets lost in the next cursor...
        say "copied $k ", $v;
    }
    undefine $!prior;
    self.item;
    self;
}

method cursor_all (StrPos $fpos, StrPos $tpos) {
    my $r = self.new(
        :targ(self.targ),
        :from($fpos),
        :to($tpos),
        :pos($tpos),
        :prior(defined self!name ?? self !! self.prior),
        :mykeys(hash(self.mykeys.pairs)),
    );
    say "PRIOR ", $r.prior.name if defined $r.prior;
    $r;
}

method cursor (StrPos $tpos) {
    self.new(
        :targ(self.targ),
        :from(self.pos // 0),
        :to($tpos),
        :pos($tpos),
        :prior(defined self!name ?? self !! self.prior),
        :mykeys(hash(self.mykeys.pairs)),
    );
}

method cursor_rev (StrPos $fpos) {
    self.new(
        :targ(self.targ),
        :pos($fpos),
        :from($fpos),
        :to(self.from),
        :prior(defined self!name ?? self !! self.prior),
        :mykeys(hash(self.mykeys.pairs)),
    );
}

my $LVL is context = 0;

method callm () is export {
    my $lvl = 0;
    while Pugs::Internals::caller(Any,$lvl,"") { $lvl++ }
    say ' ' x $lvl, substr(caller.subname,1), " called at {self.pos}";
    $lvl;
}

method whats () {
    my @k = self.mykeys.keys;
    say "  $.WHICH ===> $.from $.to $.item (@k[])";
    for @k -> $k {
        say "   $k => {self{$k}.perl}";
    }
}

method retm ($bind?) {
    say "Returning non-Cursor: self.WHAT" unless self ~~ Cursor;
    my $binding;
    if defined $bind {
        $!name = $bind;
        $binding = "      :bind<$bind>";
    }
    say ' ' x $+LVL, substr(caller.subname,1), " returning {self.from}..{self.to}$binding";
    self.whats();
    self;
}

method _STARf (&block, :$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;

    map { .retm($bind) },
        self.cursor($¢),
        self._PLUSf(&block);
}

method _STARg (&block, :$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;

    map { .retm($bind) }, reverse
        #XXX voodoo fix to prevent bogus stringification
        map { .perl.say; $_ },
            self.cursor($¢),
            self._PLUSf(&block);
}

method _STARr (&block, :$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
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
    my $r = self.cursor($to.to);
    $r.retm($bind);
}

method _PLUSf (&block, :$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    my $x = $/;

    map { .retm($bind) },
    gather {
        for block($x) -> $x {
            take map { self.cursor($_.to) }, $x, self._PLUSf($x, &block);
        }
    }
}

method _PLUSg (&block, :$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;

    reverse self._PLUSf(&block);
}

method _PLUSr (&block, :$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
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
    my $r = self.cursor($to.to);
    $r.retm($bind);
}

method _OPTr (&block, :$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    my $x = block($/)[0]; # ratchet
    my $r = $x // self.cursor($¢);
    $r.retm($bind);
}

method _OPTg (&block, :$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    my @x = block($/);
    map { .retm($bind) },
        block($/),
        self.cursor($¢);
}

method _OPTf (&block, :$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    map { .retm($bind) },
        self.cursor($¢),
        block($/);
}

method _BRACKET (&block, :$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    map { .retm($bind) },
        block($/);
}

method _PAREN (&block, :$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    map { .matchify.retm($bind) },
        block($/);
}

method _NOTBEFORE (&block, :$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    my @all = block($/);
    return () if @all;  # XXX loses continuation
    return self.cursor($¢).retm($bind);
}

method before (&block, :$bind) {
    say $¢.WHICH;
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    my @all = block($/);
    if (@all and @all[0].bool) {
        say "true";
        self.whats;
        say $¢.WHICH;
        return self.cursor($¢).retm($bind);
    }
    return ();
}

method after (&block, :$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    my $end = self.cursor($¢);
    my @all = block($end);          # Make sure .from == .to
    if (@all and @all[0].bool) {
        return $end.retm($bind);
    }
    return ();
}

method null (:$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    return self.cursor($¢).retm($bind);
}

method _ASSERT (&block, :$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    my @all = block($/);
    if (@all and @all[0].bool) {
        return self.cursor($¢).retm($bind);
    }
    return ();
}

method _BINDVAR ($var is rw, &block, :$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    map { $var := $_; .retm($bind) },  # XXX doesn't "let"
        block($/);
}

method _BINDPOS (&block, :$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    map { .retm($bind) },
        block($/);
}

method _BINDNAMED (&block, :$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    map { .retm($bind) },
        block($/);
}

# fast rejection of current prefix
method _EQ ($¢, $s, :$bind) {
    my $len = $s.chars;
    return True if substr($!targ, $¢, $len) eq $s;
    return ();
}

method _EXACT ($s, :$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    my $len = $s.chars;
    if substr($!targ, $¢, $len) eq $s {
        say "EXACT $s matched {substr($!targ,$¢,$len)} at $¢ $len";
        my $r = self.cursor($¢+$len);
        $r.retm($bind);
    }
    else {
        say "EXACT $s didn't match {substr($!targ,$¢,$len)} at $¢ $len";
        return ();
    }
}

method _EXACT_rev ($s, :$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    my $len = $s.chars;
    my $from = $/.from - $len;
    if $from >= 0 and substr($!targ, $from, $len) eq $s {
        my $r = self.cursor_rev($from);
        $r.retm($bind);
    }
    else {
        say "vEXACT $s didn't match {substr($!targ,$from,$len)} at $from $len";
        return ();
    }
}

method _DIGIT (:$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    my $char = substr($!targ, $¢, 1);
    if "0" le $char le "9" {
        my $r = self.cursor($¢+1);
        return $r.retm($bind);
    }
    else {
        say "DIGIT didn't match $char at $¢";
        return ();
    }
}

method _DIGIT_rev (:$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    my $from = $/.from - 1;
    if $from < 0 {
        say "vDIGIT didn't match $char at $from";
        return ();
    }
    my $char = substr($!targ, $from, 1);
    if "0" le $char le "9" {
        my $r = self.cursor_rev($from);
        return $r.retm($bind);
    }
    else {
        say "vDIGIT didn't match $char at $from";
        return ();
    }
}

method _ALNUM (:$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    my $char = substr($!targ, $¢, 1);
    if "0" le $char le "9" or 'A' le $char le 'Z' or 'a' le $char le 'z' or $char eq '_' {
        my $r = self.cursor($¢+1);
        return $r.retm($bind);
    }
    else {
        say "ALNUM didn't match $char at $¢";
        return ();
    }
}

method _ALNUM_rev (:$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    my $from = $/.from - 1;
    if $from < 0 {
        say "vALNUM didn't match $char at $from";
        return ();
    }
    my $char = substr($!targ, $from, 1);
    if "0" le $char le "9" or 'A' le $char le 'Z' or 'a' le $char le 'z' or $char eq '_' {
        my $r = self.cursor_rev($from);
        return $r.retm($bind);
    }
    else {
        say "vALNUM didn't match $char at $from";
        return ();
    }
}

method alpha (:$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    my $char = substr($!targ, $¢, 1);
    if 'A' le $char le 'Z' or 'a' le $char le 'z' or $char eq '_' {
        my $r = self.cursor($¢+1);
        return $r.retm($bind);
    }
    else {
        say "alpha didn't match $char at $¢";
        return ();
    }
}

method _SPACE (:$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    my $char = substr($!targ, $¢, 1);
    if $char eq " " | "\t" | "\r" | "\n" | "\f" {
        my $r = self.cursor($¢+1);
        return $r.retm($bind);
    }
    else {
        say "SPACE didn't match $char at $¢";
        return ();
    }
}

method _SPACE_rev (:$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    my $from = $/.from - 1;
    if $from < 0 {
        say "vSPACE didn't match $char at $from";
        return ();
    }
    my $char = substr($!targ, $from, 1);
    if $char eq " " | "\t" | "\r" | "\n" | "\f" {
        my $r = self.cursor_rev($from);
        return $r.retm($bind);
    }
    else {
        say "vSPACE didn't match $char at $from";
        return ();
    }
}

method _HSPACE (:$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    my $char = substr($!targ, $¢, 1);
    if $char eq " " | "\t" | "\r" {
        my $r = self.cursor($¢+1);
        return $r.retm($bind);
    }
    else {
        say "HSPACE didn't match $char at $¢";
        return ();
    }
}

method _HSPACE_rev (:$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    my $from = $/.from - 1;
    if $from < 0 {
        say "vHSPACE didn't match $char at $from";
        return ();
    }
    my $char = substr($!targ, $from, 1);
    if $char eq " " | "\t" | "\r" {
        my $r = self.cursor_rev($from);
        return $r.retm($bind);
    }
    else {
        say "vHSPACE didn't match $char at $from";
        return ();
    }
}

method _VSPACE (:$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    my $char = substr($!targ, $¢, 1);
    if $char eq "\n" | "\f" {
        my $r = self.cursor($¢+1);
        return $r.retm($bind);
    }
    else {
        say "VSPACE didn't match $char at $¢";
        return ();
    }
}

method _VSPACE_rev (:$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    my $from = $/.from - 1;
    if $from < 0 {
        say "vVSPACE didn't match $char at $from";
        return ();
    }
    my $char = substr($!targ, $from, 1);
    if $char eq "\n" | "\f" {
        my $r = self.cursor_rev($from);
        return $r.retm($bind);
    }
    else {
        say "vVSPACE didn't match $char at $from";
        return ();
    }
}

method _ANY (:$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    if $¢ < $!targ.chars {
        self.cursor($¢+1).retm($bind);
    }
    else {
        say "ANY didn't match anything at $¢";
        return ();
    }
}

method _BOS (:$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    if $¢ == 0 {
        self.cursor($¢).retm($bind);
    }
    else {
        return ();
    }
}

method _BOL (:$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    # XXX should define in terms of BOL or after vVSPACE
    if $¢ == 0 or substr($!targ, $¢-1, 1) eq "\n" or substr($!targ, $¢-2, 2) eq "\r\n" {
        self.cursor($¢).retm($bind);
    }
    else {
        return ();
    }
}

method _EOS (:$bind) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    if $¢ == $!targ.chars {
        self.cursor($¢).retm($bind);
    }
    else {
        return ();
    }
}

method _REDUCE ($tag) {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    say "Success $tag from $+FROM to $¢";
    self.whats;
    $/;
#    self.cursor($¢);
}

method _COMMITBRANCH () {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    say "Commit branch to $¢\n";
#    self.cursor($¢);  # XXX currently noop
    $/;
}

method _COMMITRULE () {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    say "Commit rule to $¢\n";
#    self.cursor($¢);  # XXX currently noop
    $/;
}

method commit () {
    my $LVL is context = self.callm;
    my $/ := self;
    my $¢ = self.pos;
    say "Commit match to $¢\n";
#    self.cursor($¢);  # XXX currently noop
    $/;
}

method fail ($m) { die $m }

