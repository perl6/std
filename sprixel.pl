#!/usr/bin/perl
use strict;
use warnings;
use v5.10;
use Getopt::Long;
use ToJS;
use Time::HiRes qw( gettimeofday );
use Encode;
use File::Slurp;
do 'viv';

sub help {
print <<'HELP';
Usage: perl sprixel.pl [switches] [--] [programfile] [arguments]
  -Cbackend       compile using the compiler backend
                  (valid backends are: html)
HELP
exit;
}
my $PROG = '';
my ($help,$C,$test_mode,$verbose,$pos);
$pos = 0;
Getopt::Long::Parser->new( config => [qw( bundling no_ignore_case pass_through require_order)], )->getoptions(
    "C=s" => \$C,
    "e=s" => \$PROG,
    'h|help' => \$help,
    't|test_mode' => \$test_mode,
    'v|verbose' => \$verbose,
    'pos' => \$pos,
) || help;
help if $help;

my $setting;
if ($test_mode) {
    $setting = 'sprixelTEST';
    unless (-f 'sprixelTEST.setting') {
        my $s_c = scalar(read_file('lib/Test.pm'));
        $s_c =~ s/^proto/my proto/g;
        $s_c =~ s/^class\sTest/ /g;
        my $c_c = scalar(read_file('CORE.setting'));
        $c_c =~ s/YOU_ARE_HERE;/ /;
        write_file( 'sprixelTEST.setting', $s_c.$c_c.'YOU_ARE_HERE;');
        my $out = `./setting sprixelTEST.setting 2>&1`;
    }
}
else {
    $setting = 'CORE';
}
my $r;
if (@ARGV and -f $ARGV[0]) {
    $r = STD->parsefile($ARGV[0], actions => 'Actions', setting => $setting)->{'_ast'};
}
else {
    if (not $PROG) {
        local $/;
        $PROG = <>;
    }
    $PROG = $PROG;
    $r = STD->parse($PROG, actions => 'Actions', setting => $setting)->{'_ast'};
}

my @js = qw[sprixel/libUtils.js sprixel/json2.js sprixel/libBigInt.js
    sprixel/interp.js sprixel/builtins.js
    sprixel/Test.pm.js];

if (defined $C and $C eq 'html') {
    say "<html><head></head><body>";
    for (@js) {
        say "<script src=\"$_\"></script>";
    }
    say "<script>";
    say "say_them = console.debug;";
    
    say 'top_interp('.ToJS::emit_js($r).',p6toplevel);';
    say "</script>";
    say "</body></html>";
} else {
    run_js_interpreter($r, $verbose);
}

sub run_js_interpreter {
    require V8;
    my $ctx = V8::Context->new();
    $ctx->register_method_by_name("say_them");
    $ctx->register_method_by_name("print_them");
    $ctx->execute(scalar(read_file($_))) for @js;
    my $ast = ToJS::emit_js($_[0], $pos);
    say $ast if $_[1];
    my $start = gettimeofday();
    eval { $ctx->execute('top_interp('.$ast.',p6toplevel);') };
    warn $@ if $@;
    #say sprintf "\n\ttime in interpreter: %.6f s", gettimeofday()-$start;
}
sub say_them {
    # TODO: put this in C++ instead as a V8 plugin (or use Print() from d8.cc)
    say map { Encode::decode_utf8($_) } @_;
}
sub print_them {
    # TODO: put this in C++ instead as a V8 plugin (or use Print() from d8.cc)
    print map { Encode::decode_utf8($_) } @_;
}
