class Cursor;

my $VERBOSE = 1;
my $callouts = 1;

# XXX still full of ASCII assumptions

# most cursors just copy forward the previous value of the following two items:
has $.orig;        # per match, the original string we are matching against
our %lexers;       # per language, the cache of lexers, keyed by (|) location

has Bool $.bool is rw = 1;
has StrPos $.from = 0;
has StrPos $.to = 0;
has StrPos $.pos = 0;
has Cursor $.prior;
has %.mykeys;
has Str $.name;
has $!item;

method lexers { %lexers }   # XXX should be different per language, sigh

my $fakepos = 1;

method _AUTOLEXpeek ($key) {
    die "Null key" if $key eq '';
    if %.lexers{$key} {
        if %+AUTOLEXED{$key} {   # no left recursion allowed in lexer!
            die "Left recursion in $key" if $fakepos == %+AUTOLEXED{$key};
            warn "Suppressing lexer recursion on $key";
            return -> $¢ { '' };  # (but if we advanced just assume a :: here)
        }
        elsif %.lexers{$key}.WHAT eq Hash {
            return %.lexers{$key}<lexer> // -> $¢ { '' };
        }
        else {
            say "oops ", $key.WHAT;
        }
    }
    my $ast = eval("tmpyaml/$key.yml".slurp, :lang<yaml>);
    %.lexers{$key} = hash(:$ast, :lexer(-> $¢ { '' }));
    my $lexer = self._AUTOLEXgen($key,$ast);
    %.lexers{$key}<lexer> = $lexer;
    return $lexer;
}

method _AUTOLEXnow ($key) {
    if %.lexers.exists($key) {
        return %.lexers{$key}<lexer>;
    }
    my %AUTOLEXED is context<rw>;
    my $ast = eval("tmpyaml/$key.yml".slurp, :lang<yaml>);
    %.lexers{$key} = hash(:$ast, :lexer(undef));
    my $lexer = self._AUTOLEXgen($key,$ast);
    %.lexers{$key}<lexer> = $lexer;
    return $lexer;
}

method _AUTOLEXgen ($key,$ast) {
    my $oldfakepos = %+AUTOLEXED{$key} // 0;
    %+AUTOLEXED{$key} = $fakepos;
    my $lexer = $ast.lexer(self);
    %+AUTOLEXED{$key} = $oldfakepos;
    sub ($¢) {
        1 while $lexer ~~ s:P5/\n[\x20\t]*\n/\n/;
        $lexer;
    }
}

method setname($k) {
    $!name = ~$k;
    self;
}

method item {
    if not defined $!item {
        $!item = substr($.orig, $.from, $.to - $.from);
    }
    $!item;
}

method matchify {
    my %bindings;
    my Cursor $m;
    my $next;
    say "MATCHIFY", self.WHAT;
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
        :orig(self.orig),
        #:lexers(self.lexers),
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
        :orig(self.orig),
        #:lexers(self.lexers),
        :from(self.pos // 0),
        :to($tpos),
        :pos($tpos),
        :prior(defined self!name ?? self !! self.prior),
        :mykeys(hash(self.mykeys.pairs)),
    );
}

method cursor_rev (StrPos $fpos) {
    self.new(
        :orig(self.orig),
        #:lexers(self.lexers),
        :pos($fpos),
        :from($fpos),
        :to(self.from),
        :prior(defined self!name ?? self !! self.prior),
        :mykeys(hash(self.mykeys.pairs)),
    );
}

my $CTX is context = { lvl => 0 };

method callm ($arg?) {
    my $lvl = 0;
    while Pugs::Internals::caller(Any,$lvl,"") { $lvl++ }
    my $caller = caller;
    my $name = substr($caller.subname,1);
    if defined $arg { 
        $name ~= " " ~ $arg;
    }
    my $pos = '?';
    $pos = self.pos if defined self.orig;
    say $pos,"\t", ':' x $lvl, ' ', $name, " [", $caller.file, ":", $caller.line, "]";
    {lvl => $lvl};
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
    if defined $bind and $bind ne '' {
        $!name = $bind;
        $binding = "      :bind<$bind>";
    }
    say self.pos, "\t", ':' x $+CTX<lvl>, ' ', substr(caller.subname,1), " returning {self.from}..{self.to}$binding";
#    self.whats();
    self;
}

method _MATCHIFY ($bind, *@results) {
    map { .cursor_all(self.pos, .to).retm($bind) },
    map { .matchify }, @results;
}

method _SEQUENCE (@array, &block) {
    map &block, @array;
}

method _STARf (&block, :$bind) {
    my $CTX is context = self.callm;

    map { .retm($bind) },
        self.cursor(self.pos),
        self._PLUSf(&block);
}

method _STARg (&block, :$bind) {
    my $CTX is context = self.callm;

    map { .retm($bind) }, reverse
        #XXX voodoo fix to prevent bogus stringification
        map { .perl.say; $_ },
            self.cursor(self.pos),
            self._PLUSf(&block);
}

method _STARr (&block, :$bind) {
    my $CTX is context = self.callm;
    my $to = self;
    my @all = gather {
        loop {
            my @matches = block($to);  # XXX shouldn't read whole list
#            say @matches.perl;
        last unless @matches;
            my $first = @matches[0];  # no backtracking into block on ratchet
            take $first;
            $to = $first;
        }
    };
#    self.cursor($to.to).retm($bind);
    self.cursor($to.to);  # XXX .retm blows up pugs for unfathomable reasons
}

method _PLUSf (&block, :$bind) {
    my $CTX is context = self.callm;
    my $x = self;

    map { .retm($bind) },
    gather {
        for block($x) -> $x {
            take map { self.cursor($_.to) }, $x, self._PLUSf($x, &block);
        }
    }
}

method _PLUSg (&block, :$bind) {
    my $CTX is context = self.callm;

    reverse self._PLUSf(&block);
}

method _PLUSr (&block, :$bind = '') {
    my $CTX is context = self.callm;
    my $to = self;
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
    my $CTX is context = self.callm;

    my $x = block(self)[0]; # ratchet
    my $r = $x // self.cursor(self.pos);
    $r.retm($bind);
}

method _OPTg (&block, :$bind) {
    my $CTX is context = self.callm;
    my @x = block(self);
    map { .retm($bind) },
        block(self),
        self.cursor(self.pos);
}

method _OPTf (&block, :$bind) {
    my $CTX is context = self.callm;
    map { .retm($bind) },
        self.cursor(self.pos),
        block(self);
}

method _BRACKET (&block, :$bind) {
    my $CTX is context = self.callm;
    map { .retm($bind) },
        block(self);
}

method _PAREN (&block, :$bind) {
    my $CTX is context = self.callm;
    map { .matchify.retm($bind) },
        block(self);
}

method _NOTBEFORE (&block, :$bind) {
    my $CTX is context = self.callm;
    my @all = block(self);
    return () if @all;  # XXX loses continuation
    return self.cursor(self.pos).retm($bind);
}

method before (&block, :$bind) {
    my $CTX is context = self.callm;
    my @all = block(self);
    if (@all and @all[0].bool) {
        return self.cursor(self.pos).retm($bind);
    }
    return ();
}

method after (&block, :$bind) {
    my $CTX is context = self.callm;
    my $end = self.cursor(self.pos);
    my @all = block($end);          # Make sure .from == .to
    if (@all and @all[0].bool) {
        return $end.retm($bind);
    }
    return ();
}

method null (:$bind) {
    my $CTX is context = self.callm;
    return self.cursor(self.pos).retm($bind);
}

method _ASSERT (&block, :$bind) {
    my $CTX is context = self.callm;
    my @all = block(self);
    if (@all and @all[0].bool) {
        return self.cursor(self.pos).retm($bind);
    }
    return ();
}

method _BINDVAR ($var is rw, &block, :$bind) {
    my $CTX is context = self.callm;
    map { $var := $_; .retm($bind) },  # XXX doesn't "let"
        block(self);
}

method _BINDPOS (&block, :$bind) {
    my $CTX is context = self.callm;
    map { .retm($bind) },
        block(self);
}

method _BINDNAMED (&block, :$bind) {
    my $CTX is context = self.callm;
    map { .retm($bind) },
        block(self);
}

# fast rejection of current prefix
method _EQ ($P, $s, :$bind) {
    my $len = $s.chars;
    return True if substr($!orig, $P, $len) eq $s;
    return ();
}

method _EXACT ($s, :$bind) {
    my $CTX is context = self.callm($s);
    my $P = self.pos;
    my $len = $s.chars;
    if substr($!orig, $P, $len) eq $s {
#        say "EXACT $s matched {substr($!orig,$P,$len)} at $P $len";
        my $r = self.cursor($P+$len);
        $r.retm($bind);
    }
    else {
#        say "EXACT $s didn't match {substr($!orig,$P,$len)} at $P $len";
        return ();
    }
}

method _EXACT_rev ($s, :$bind) {
    my $CTX is context = self.callm;
    my $len = $s.chars;
    my $from = self.from - $len;
    if $from >= 0 and substr($!orig, $from, $len) eq $s {
        my $r = self.cursor_rev($from);
        $r.retm($bind);
    }
    else {
#        say "vEXACT $s didn't match {substr($!orig,$from,$len)} at $from $len";
        return ();
    }
}

method _DIGIT (:$bind) {
    my $CTX is context = self.callm;
    my $P = self.pos;
    my $char = substr($!orig, $P, 1);
    if "0" le $char le "9" {
        my $r = self.cursor($P+1);
        return $r.retm($bind);
    }
    else {
#        say "DIGIT didn't match $char at $P";
        return ();
    }
}

method _DIGIT_rev (:$bind) {
    my $CTX is context = self.callm;
    my $from = self.from - 1;
    if $from < 0 {
#        say "vDIGIT didn't match $char at $from";
        return ();
    }
    my $char = substr($!orig, $from, 1);
    if "0" le $char le "9" {
        my $r = self.cursor_rev($from);
        return $r.retm($bind);
    }
    else {
#        say "vDIGIT didn't match $char at $from";
        return ();
    }
}

method _ALNUM (:$bind) {
    my $CTX is context = self.callm;
    my $P = self.pos;
    my $char = substr($!orig, $P, 1);
    if "0" le $char le "9" or 'A' le $char le 'Z' or 'a' le $char le 'z' or $char eq '_' {
        my $r = self.cursor($P+1);
        return $r.retm($bind);
    }
    else {
#        say "ALNUM didn't match $char at $P";
        return ();
    }
}

method _ALNUM_rev (:$bind) {
    my $CTX is context = self.callm;
    my $from = self.from - 1;
    if $from < 0 {
#        say "vALNUM didn't match $char at $from";
        return ();
    }
    my $char = substr($!orig, $from, 1);
    if "0" le $char le "9" or 'A' le $char le 'Z' or 'a' le $char le 'z' or $char eq '_' {
        my $r = self.cursor_rev($from);
        return $r.retm($bind);
    }
    else {
#        say "vALNUM didn't match $char at $from";
        return ();
    }
}

method alpha (:$bind) {
    my $CTX is context = self.callm;
    my $P = self.pos;
    my $char = substr($!orig, $P, 1);
    if 'A' le $char le 'Z' or 'a' le $char le 'z' or $char eq '_' {
        my $r = self.cursor($P+1);
        return $r.retm($bind);
    }
    else {
#        say "alpha didn't match $char at $P";
        return ();
    }
}

method _SPACE (:$bind) {
    my $CTX is context = self.callm;
    my $P = self.pos;
    my $char = substr($!orig, $P, 1);
    if $char eq " " | "\t" | "\r" | "\n" | "\f" {
        my $r = self.cursor($P+1);
        return $r.retm($bind);
    }
    else {
#        say "SPACE didn't match $char at $P";
        return ();
    }
}

method _SPACE_rev (:$bind) {
    my $CTX is context = self.callm;
    my $from = self.from - 1;
    if $from < 0 {
#        say "vSPACE didn't match $char at $from";
        return ();
    }
    my $char = substr($!orig, $from, 1);
    if $char eq " " | "\t" | "\r" | "\n" | "\f" {
        my $r = self.cursor_rev($from);
        return $r.retm($bind);
    }
    else {
#        say "vSPACE didn't match $char at $from";
        return ();
    }
}

method _HSPACE (:$bind) {
    my $CTX is context = self.callm;
    my $P = self.pos;
    my $char = substr($!orig, $P, 1);
    if $char eq " " | "\t" | "\r" {
        my $r = self.cursor($P+1);
        return $r.retm($bind);
    }
    else {
#        say "HSPACE didn't match $char at $P";
        return ();
    }
}

method _HSPACE_rev (:$bind) {
    my $CTX is context = self.callm;
    my $from = self.from - 1;
    if $from < 0 {
#        say "vHSPACE didn't match $char at $from";
        return ();
    }
    my $char = substr($!orig, $from, 1);
    if $char eq " " | "\t" | "\r" {
        my $r = self.cursor_rev($from);
        return $r.retm($bind);
    }
    else {
#        say "vHSPACE didn't match $char at $from";
        return ();
    }
}

method _VSPACE (:$bind) {
    my $CTX is context = self.callm;
    my $P = self.pos;
    my $char = substr($!orig, $P, 1);
    if $char eq "\n" | "\f" {
        my $r = self.cursor($P+1);
        return $r.retm($bind);
    }
    else {
#        say "VSPACE didn't match $char at $P";
        return ();
    }
}

method _VSPACE_rev (:$bind) {
    my $CTX is context = self.callm;
    my $from = self.from - 1;
    if $from < 0 {
#        say "vVSPACE didn't match $char at $from";
        return ();
    }
    my $char = substr($!orig, $from, 1);
    if $char eq "\n" | "\f" {
        my $r = self.cursor_rev($from);
        return $r.retm($bind);
    }
    else {
#        say "vVSPACE didn't match $char at $from";
        return ();
    }
}

method _ANY (:$bind) {
    my $CTX is context = self.callm;
    my $P = self.pos;
    if $P < $!orig.chars {
        self.cursor($P+1).retm($bind);
    }
    else {
#        say "ANY didn't match anything at $P";
        return ();
    }
}

method _BOS (:$bind) {
    my $CTX is context = self.callm;
    my $P = self.pos;
    if $P == 0 {
        self.cursor($P).retm($bind);
    }
    else {
        return ();
    }
}

method _BOL (:$bind) {
    my $CTX is context = self.callm;
    my $P = self.pos;
    # XXX should define in terms of BOL or after vVSPACE
    if $P == 0 or substr($!orig, $P-1, 1) eq "\n" or substr($!orig, $P-2, 2) eq "\r\n" {
        self.cursor($P).retm($bind);
    }
    else {
        return ();
    }
}

method _EOS (:$bind) {
    my $CTX is context = self.callm;
    my $P = self.pos;
    if $P == $!orig.chars {
        self.cursor($P).retm($bind);
    }
    else {
        return ();
    }
}

method _REDUCE ($tag) {
    my $CTX is context = self.callm($tag);
#    my $P = self.pos;
#    say "Success $tag from $+FROM to $P";
#    self.whats;
    self;
#    self.cursor($P);
}

method _COMMITBRANCH () {
    my $CTX is context = self.callm;
    my $P = self.pos;
    say "Commit branch to $P\n";
#    self.cursor($P);  # XXX currently noop
    self;
}

method _COMMITRULE () {
    my $CTX is context = self.callm;
    my $P = self.pos;
    say "Commit rule to $P\n";
#    self.cursor($P);  # XXX currently noop
    self;
}

method commit () {
    my $CTX is context = self.callm;
    my $P = self.pos;
    say "Commit match to $P\n";
#    self.cursor($P);  # XXX currently noop
    self;
}

method fail ($m) { die $m }

#############################################################3

my sub indent (Str $s is copy) {
    $s ~~ s:P5:g/\n/\n  /;
    "  " ~ $s;
}

my sub here ($arg?) {
    my $lvl = 0;
    while Pugs::Internals::caller(Any,$lvl,"") { $lvl++ }
    my $caller = caller;
    my $package = $caller.package;
    my $name = $package;   # ~ '::' ~ substr($caller.subname,1);
    if defined $arg { 
        $name ~= " " ~ $arg;
    }
    say ':' x $lvl, ' ', $name, " [", $caller.file, ":", $caller.line, "]";
}

our class REbase {
    method lexer ($¢) { here "UNIMPL {self.WHAT}"; self.WHAT.substr(3).uc }
}

our class RE is REbase {
    method lexer ($¢) {
        here;
        my $PURE is context<rw> = 1;
        self.<re>.lexer($¢);
    }
}

our class RE_adverb is REbase {
    #method lexer ($¢) { ... }
}

our class RE_assertion is REbase {
    method lexer ($¢) {
        given self.<assert> {
            when '?' {
                my $re = self.<re>;
                if $re.<name> eq 'before' {
                    my $result = $re.lexer($¢);
                    $+PURE = 0;
                    return $result;
                }
            }
        }
        '[]';
    }
}

our class RE_assertvar is REbase {
    method lexer ($¢) {
        $+PURE = 0;
        '';
    }
}

our class RE_block is REbase {
    method lexer ($¢) {
        $+PURE = 0;
        '';
    }
}

our class RE_bindvar is REbase {
    method lexer ($¢) { here; self.<atom>.lexer($¢) }
}

our class RE_bindnamed is REbase {
    method lexer ($¢) { here; self.<atom>.lexer($¢) }
}

our class RE_bindpos is REbase {
    method lexer ($¢) { here; self.<atom>.lexer($¢) }
}

our class RE_bracket is REbase {
    method lexer ($¢) { here; indent("\n(\n" ~ indent(self.<re>.lexer($¢)) ~ "\n)") }
}

our class RE_cclass is REbase {
    method lexer ($¢) {
        here ~self.<text>;
        $fakepos++;
        my $cc = self.<text>;
        $cc ~~ s:P5/^\-\[/[^/;
        $cc ~~ s:P5/^\+\[/[/;
        $cc ~~ s:P5:g/\s*\.\.\s*/-/;
        $cc ~~ s:P5:g/\s*//;
        $cc;
    }
}

our class RE_decl is REbase {
    method lexer ($¢) { '[]' }
}

our class RE_double is REbase {
    # XXX inadequate for "\n" without interpolation
    method lexer ($¢) {
        $+PURE = 0;
        '';
    }
}

our class RE_meta is REbase {
    method lexer ($¢) {
        my $text = self.<text>;
        here $text;
        given $text {
            when '^' | '$' | '.' | '\\w' | '\\s' | '\\d' {
                return $text;
            }
            when '\\h' {
                return '[\\x20\\t]';
            }
            when '::' {
                $+PURE = 0;
                return '';
            }
            default {
                return "META[$text]"
            }
        }
    }
}

our class RE_method_noarg is REbase {
    method lexer ($¢) {
        my $name = self.<name>;
        here $name;
        given $name {
            when 'null' {
                return '';
            }
            when '' {
                $+PURE = 0;
                return '';
            }
            when 'ws' {
                $+PURE = 0;
                return '';
            }
            when 'EXPR' {
                $+PURE = 0;
                return '';
            }
            when 'sym' {
                $fakepos++;
                return quotemeta(self.<sym>);
            }
            when 'alpha' {
                $fakepos++;
                return '[a-z_A-Z]';
            }
            default {
                my $lexer = $¢.$name('?')[0];
                return $lexer($¢);
            }
        }
    }
}

our class RE_method_internal is REbase {
    method lexer ($¢) {
        $+PURE = 0;
        '';
    }
}

our class RE_method_re is REbase {
    method lexer ($¢) {
        my $name = self.<name>;
        here $name;
        my $re = self.<re>;
        given $name {
            when '' {
                $+PURE = 0;
                return '';
            }
            when 'after' {
                return '[]';
            }
            when 'before' {
                my $result = $re.lexer($¢);
                $+PURE = 0;
                return $result;
            }
            default {
                my $lexer = $¢.$name($re, '?')[0];
                return $lexer($¢);
            }
        }
    }
}

our class RE_method_str is REbase {
    method lexer ($¢) {
        my $name = self.<name>;
        here $name;
        my $str = self.<str>;
        given $name {
            when 'lex1' {
                return '[]';
            }
            when 'panic' | 'obs' {
                $+PURE = 0;
                return '';
            }
            default {
                my $lexer = $¢.$name($str, '?')[0];
                return $lexer($¢);
            }
        }
    }
}

our class RE_method is REbase {
    method lexer ($¢) {
        $+PURE = 0;
        '';
    }
}

our class RE_noop is REbase {
    method lexer ($¢) {
        '[]';
    }
}

our class RE_ordered_conjunction is REbase {
    method lexer ($¢) {
        $+PURE = 0;
        '';
    }
}

our class RE_ordered_disjunction is REbase {
    method lexer ($¢) {
        my $alts := self.<zyg>;
        here +@$alts;
        my @result;
        for @$alts -> $alt {
            my $pat = $alt.lexer($¢);
            $pat ~= ':' ~ $alt.<alt> if $callouts;
            push @result, $pat;
            last;
        }
        my $result = @result[0] ~ "|(LATER)";
        say $result;
        $result;
    }
}

our class RE_paren is REbase {
    method lexer ($¢) { here; indent("\n(\n" ~ indent(self.<re>.lexer($¢)) ~ "\n)") }
}

our class RE_quantified_atom is REbase {
    method lexer ($¢) {
        here;
        my $oldfakepos = $fakepos++;
        my $atom = self.<atom>.lexer($¢);
        if self.<quant>[0] eq '+' {
            return "$atom+";
        }
        elsif self.<quant>[0] eq '*' {
            $fakepos = $oldfakepos;
            return "$atom*";
        }
        elsif self.<quant>[0] eq '?' {
            $fakepos = $oldfakepos;
            return "$atom?";
        }
        elsif self.<quant>[0] eq '**' {
            my $x = self.<quant>[2];
            $x ~~ s:P5/\.\./,/;
            $x ~~ s:P5/\*//;
            $fakepos = $oldfakepos if $x ~~ m:P5/^0/;
            return $atom ~ '{' ~ $x ~ '}';
        }
        else {
            $+PURE = 0;
            return '';
        }
    }
}

our class RE_qw is REbase {
    method lexer ($¢) {
        my $text = self.<text>;
        here $text;
        $fakepos++;
        $text ~~ s:P5/^<\s*//;
        $text ~~ s:P5/\s*>$//;
        $text ~~ s:P5/\s+/|/;
        $text;
    }
}

our class RE_sequence is REbase {
    method lexer ($¢) {
        my $PURE is context<rw> = 1;
        my $c := self.<zyg>;
        my @chunks = @$c;
        here +@chunks;
        my @result;
        my $last;
#        while @chunks and
#            $last = @chunks[-1] and  # XXX sb *-1 someday
#            not $last.<min>
#        {
#                pop @chunks;
#        }
        for @chunks {
            my $next = .lexer($¢);
            last if $next eq '';
            $next = '' if $next eq '[]';
            push @result, $next;
            last unless $PURE;
        }
        join ' ', @result;
    }
}

our class RE_string is REbase {
    # XXX needs quoting
    method lexer ($c) {
        here ~self.<text>;
        my $text = self.<text>;
        $fakepos++ if self.<min>;
        quotemeta($text);
    }
}

our class RE_submatch is REbase {
    #method lexer ($¢) { ... }
}

our class RE_unordered_conjunction is REbase {
    method lexer ($¢) {
        $+PURE = 0;
        '';
    }
}

our class RE_unordered_disjunction is REbase {
    method lexer ($¢) {
        my $alts := self.<zyg>;
        here +@$alts;
        my @result;
        my $oldfakepos = $fakepos;
        my $minfakepos = $fakepos + 1;
        for @$alts -> $alt {
            $fakepos = $oldfakepos;
            my $pat = $alt.lexer($¢);
            $pat ~= ' -> ' ~ $alt.<alt> if $callouts;
            push @result, $pat;
            $minfakepos = $oldfakepos if $fakepos == $oldfakepos;
        }
        my $result = "\n(\n  " ~ indent(join "\n| ", @result) ~ "\n)";
        say $result;
        $fakepos = $minfakepos;  # Did all branches advance?
        $result;
    }
}

our class RE_var is REbase {
    #method lexer ($¢) { ... }
    method lexer ($¢) {
        $+PURE = 0;
        '';
    }
}


