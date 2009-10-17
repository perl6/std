#!/usr/bin/perl
use strict;
use warnings;
use v5.10;
use Getopt::Long;
use ToJS;
use Time::HiRes qw( gettimeofday );
use Encode;
use File::Slurp;
use File::Find;
do 'viv';
no warnings;

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
    $setting = 'sprixelCORETEST';
    unless (-f 'sprixelCORETEST.syml') {
        my $s_c = scalar(read_file('lib/Test.pm'));
        $s_c =~ s/proto/my proto/g;
        $s_c =~ s/^class\sTest;//g;
        my $c_c = scalar(read_file('CORE.setting'));
        $c_c =~ s/YOU_ARE_HERE;//;
        write_file( 'sprixelCORETEST.setting', $c_c.$s_c."YOU_ARE_HERE;\n");
        my $out = `./setting sprixelCORETEST.setting 2>&1`;
    }
}
else {
    $setting = 'sprixelCORE';
    unless (-f 'sprixelCORE.syml') {
        my $s_c = scalar(read_file('lib/Test.pm'));
        $s_c =~ s/^proto/my proto/g;
        $s_c =~ s/^class\sTest;//g;
        $s_c .= "\n  \nproto derive_context is export { }\n".
            "proto get_core is export { }\nproto member is export { }\n".
            "proto what is export { }\nproto jseval is export { }\n";
        my $c_c = scalar(read_file('CORE.setting'));
        $c_c =~ s/YOU_ARE_HERE;//;
        write_file( 'sprixelCORE.setting', $c_c.$s_c."YOU_ARE_HERE;\n");
        my $out = `./setting sprixelCORE.setting 2>&1`;
    }
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
$r->{'stabs'} = $STD::ALL;

# sprixel/json2.js  sprixel/builtins.js sprixel/Test.pm.js
my @js = qw[sprixel/libBigInt.js sprixel/libUtils.js sprixel/Context.js
    sprixel/Act.js];

find sub { push @js, $File::Find::name if !/\/\./ && /\.js$/ },
    'sprixel/control_flow',
    'sprixel/misc';

if (defined $C and $C eq 'html') {
    say "<html><head></head><body>";
    for (@js) {
        say "<script src=\"$_\"></script>";
    }
    say "<script>";
    say "say_them = console.debug;";
    say 'Act.interpret('.ToJS::emit_js($r).');';
    say "</script>";
    say "</body></html>";
} else {
    run_js_interpreter($r, $verbose);
}

my $ctxs = [];
my $ctx_id = 0;

sub run_js_interpreter {
    require V8;
    my $ctx_id = new_ctx();
    my $ctx = $ctxs->[$ctx_id];
    my $js_code = '';
    $js_code .= scalar(read_file($_)) for @js;
    $ctx->execute($js_code);
    my $ast = ToJS::emit_js($_[0], $pos);
    say $ast if $_[1];
    my $start = gettimeofday();
    eval { $ctx->execute('Act.interpret('.$ast.');') };
    warn $@ if $@;
    say sprintf "\n\ttime in interpreter: %.6f s", gettimeofday()-$start;
}
sub say_them {
    say map { Encode::decode_utf8($_) } @_;
}
sub print_them {
    print map { Encode::decode_utf8($_) } @_;
}
sub new_ctx {
    my $ctx = $ctxs->[$ctx_id || 0] = V8::Context->new();
    $ctx->register_method_by_name("say_them");
    $ctx->register_method_by_name("print_them");
    $ctx->register_method_by_name("new_ctx");
    $ctx->register_method_by_name("eval_ctx");
    return $ctx_id++;
}
sub eval_ctx {
    $ctxs->[$_[0]]->execute(Encode::decode_utf8($_[1]))."";
}
sub load_file {
    $ctxs->[$_[0]]->execute(scalar(read_file(Encode::decode_utf8($_[1]))))."";
}

1;