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
do 'viv'; # load the Perl 5 based parser for STD.pm: the Perl 6 Grammar
no warnings;

# put this script's usage hints up front, to also serve as documentation
sub help {
    print <<'HELP';
Usage: perl sprixel.pl [switches] [--] [programfile] [arguments]
options:
  -C<backend>     use a compiler backend (currently valid backend: html)
  -e 'program'    execute Perl 6 code passed as a command line argument
  -h --help       display this help
  -t --test_mode  use setting library in sprixelCORETEST instead of CORE
  -v --verbose    er, say more stuff
HELP
    exit;
}

my $PROG = '';
my ($help,$C,$test_mode,$verbose);
Getopt::Long::Parser->new( config => [qw( bundling no_ignore_case pass_through require_order)], )->getoptions(
    "C=s" => \$C,
    "e=s" => \$PROG,
    'h|help' => \$help,
    't|test_mode' => \$test_mode,
    'v|verbose' => \$verbose,
) || help;
help if $help;

my $setting = 'sprixelCORE';
$ENV{'PERL6LIB'} = './sprixel/setting';
my $out = `./setting sprixelCORE.setting 2>&1`;

# reparse the setting files, temporarily until ./setting persists the ASTs
my $s = '';
find sub { $s .= scalar(read_file($_)).";\n"
    if !/\/\./ && /\.pm$/ }, 'sprixel/setting';

my $r; # receives the result of viv having parsed the Perl 6 code 
if (@ARGV and -f $ARGV[0]) {
    # run the viv parser on Perl 6 code in a file
    $r = STD->parsefile($ARGV[0], actions => 'Actions', setting => $setting)->{'_ast'};
}
else {
    if (not $PROG) {
        local $/;
        $PROG = <>;
    }
    $PROG = $PROG;
    # run the viv parser on Perl 6 code in a string
    $r = STD->parse($PROG, actions => 'Actions', setting => $setting)->{'_ast'};
}
$r->{'stabs'} = $STD::ALL;

# Start building the list of source files to compile into the setting.
# sprixel/json2.js  sprixel/builtins.js sprixel/Test.pm.js
my @js = qw[sprixel/libBigInt.js sprixel/libUtils.js sprixel/Context.js
    sprixel/Act.js];
# Continue building the setting source file list from some directories
find sub { push @js, $File::Find::name if !/\/\./ && /\.js$/ },
    'sprixel/control_flow',
    'sprixel/misc';

# Interpret the AST, either in a browser(html) or console(text) environment
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
} else { # no html, therefore output to a text console
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
    my $ast = ToJS::emit_js($_[0]);
    say $ast if $_[1];
    my $start = gettimeofday();
    eval { $ctx->execute('Act.interpret('.$ast.','.ToJS::emit_js(
        STD->parse($s, actions => 'Actions', setting => $setting)->{'_ast'}
    ).');') };
    warn $@ if $@;
    #say sprintf "\n\ttime in interpreter: %.6f s", gettimeofday()-$start;
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
