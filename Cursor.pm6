# Cursor.pm
#
# Copyright 2007-2010, Larry Wall
#
# You may copy this software under the terms of the Artistic License,
#     version 2.0 or later.

use CursorBase;
class Cursor is CursorBase;
our $BLUE    = $CursorBase::BLUE;
our $GREEN   = $CursorBase::GREEN;
our $CYAN    = $CursorBase::CYAN;
our $MAGENTA = $CursorBase::MAGENTA;
our $YELLOW  = $CursorBase::YELLOW;
our $RED     = $CursorBase::RED;
our $CLEAR   = $CursorBase::CLEAR;

method panic (Str $s) {
    self.deb("panic $s") if $*DEBUG;
    my $m;
    my $here = self;

    $m ~= $s;
    $m ~= $here.locmess;
    $m ~= "\n" unless $m ~~ /\n$/;

    note $Cursor::RED, '===', $Cursor::CLEAR, 'SORRY!', $Cursor::RED, '===', $Cursor::CLEAR, "\n";
    note $m;

    die "Parse failed\n";
}

method worry (Str $s) {
    my $m = $s ~ self.locmess;
    push @*WORRIES, $m unless %*WORRIES{$s}++;
    self;
}

method sorry (Str $s) {
    self.deb("sorry $s") if $*DEBUG;
    note $Cursor::RED, '===', $Cursor::CLEAR, 'SORRY!', $Cursor::RED, '===', $Cursor::CLEAR, "\n"
        unless $*IN_SUPPOSE or $*FATALS++;
    if $s {
        my $m = $s;
        $m ~= self.locmess ~ "\n" unless $m ~~ /\n$/;
        if $*FATALS > 10 or $*IN_SUPPOSE {
            die $m;
        }
        else {
            note $m unless %*WORRIES{$m}++;
        }
    }
    self;
}

method locmess () {
    my $pos = self.pos;
    my $line = self.lineof($pos);

    # past final newline?
    if $pos >= @*MEMOS - 1 {
        $pos = $pos - 1;
        $line = $line ~ " (EOF)";
    }

    my $pre = substr($*ORIG, 0, $pos);
    $pre = substr($pre, -40, 40);
    1 while $pre ~~ s!.*\n!!;
    $pre = '<BOL>' if $pre eq '';
    my $post = substr($*ORIG, $pos, 40);
    1 while $post ~~ s!(\n.*)!!;
    $post = '<EOL>' if $post eq '';
    " at " ~ $*FILE<name> ~ " line $line:\n------> " ~ $Cursor::GREEN ~ $pre ~ $Cursor::YELLOW ~ $*PERL6HERE ~ $Cursor::RED ~ 
        "$post$Cursor::CLEAR";
}

method line {
    self.lineof(self.pos);
}

method lineof ($p) {
    return 1 unless defined $p;
    my $line = @*MEMOS[$p]<L>;
    return $line if $line;
    $line = 0;
    my $pos = 0;
    my @text = split(/^/,$*ORIG);   # XXX p5ism, should be ^^
    for @text {
        $line++;
        @*MEMOS[$pos++]<L> = $line
            for 1 .. chars($_);
    }
    @*MEMOS[$pos++]<L> = $line;
    return @*MEMOS[$p]<L> // 0;
}

method SETGOAL { }
method FAILGOAL (Str $stop, Str $name, $startpos) {
    my $s = "'$stop'";
    $s = '"\'"' if $s eq "'''";
    self.panic("Unable to parse $name" ~ $startpos.locmess ~ "\nCouldn't find final $s; gave up");
}
## vim: expandtab sw=4 ft=perl6
