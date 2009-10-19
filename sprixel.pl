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
          # do instead of use because viv is a Perl 5 script, not a module
no warnings;

# put this script's usage hints up front, to also serve as documentation
sub help {
    print <<'HELP';
Usage: perl sprixel.pl [options] [--] [programfile] [arguments]
options:
  -C<backend>     use a compiler backend (currently valid backend: html)
  -e 'program'    execute Perl 6 code passed as a command line argument
  -h --help       display this help
with no programfile and no -e option, sprixel.pl reads code from stdin
HELP
    exit;
}
# TODO: decide to either retain or drop --test_mode and --verbose,
#       they currently do nothing.

my $PROG = ''; # the Perl 6 program to execute, assigned either via the
               # -e option here or from standard input.
               # Not used when sprixel.pl runs a program from a file.
my ($help,$C,$test_mode,$verbose);
Getopt::Long::Parser->new( config => [qw( bundling no_ignore_case pass_through require_order)], )->getoptions(
    "C=s" => \$C,
    "e=s" => \$PROG,
    'h|help' => \$help,
    't|test_mode' => \$test_mode,
    'v|verbose' => \$verbose,
) || help;
help if $help;

# Run the setting script to build sprixelCORE.syml and
# sprixelCORE.syml.store from sprixelCORE.setting
my $setting = 'sprixelCORE';
$ENV{'PERL6LIB'} = './sprixel/setting';
my $out = `./setting sprixelCORE.setting 2>&1`;

# concatenate the setting files, temporarily until ./setting persists the ASTs
my $s = '';
find sub { $s .= scalar(read_file($_)).";\n" if !/\/\./ && /\.pm$/ },
    'sprixel/setting';
# temporarily the $s will be passed to the eval in sub run_js_interpreter.

my $r; # receives a Perl 5 object (the AST) from viv having parsed the Perl 6 program
if (@ARGV and -f $ARGV[0]) {
    # run the viv parser on Perl 6 code in a file
    $r = STD->parsefile($ARGV[0], actions => 'Actions', setting => $setting)->{'_ast'};
}
else {
    if (not $PROG) {
        local $/; # replace default input record separator with undef,
                  # causing the following <> to slurp the entire file
        $PROG = <>;
    }
    $PROG = $PROG;
    # run the viv parser on Perl 6 code in a string
    $r = STD->parse($PROG, actions => 'Actions', setting => $setting)->{'_ast'};
}
$r->{'stabs'} = $STD::ALL;

# Start building the list of JavaScript files to use as runtime library.
# sprixel/json2.js  sprixel/builtins.js sprixel/Test.pm.js
my @js = qw[sprixel/libBigInt.js sprixel/libUtils.js sprixel/Context.js
    sprixel/Act.js];
# Continue building the runtime library file list from some directories
find sub { push @js, $File::Find::name if !/\/\./ && /\.js$/ },
    'sprixel/control_flow',
    'sprixel/misc';

# Interpret the AST, either in a browser(html) or console(text) environment
if (defined $C and $C eq 'html') {
    # TODO: put a comment and the runtime library script elements inside
    # the html <head></head> element.
    say "<html><head></head><body>";
    for (@js) {
        say "<script src=\"$_\"></script>";
    }
    say "<script>";
    say "say_them = console.debug;";
    # TODO: handle setting the same way as is done in run_js_interpreter
    say 'Act.interpret('.ToJS::emit_js($r).');';
    say "</script>";
    say "</body></html>";
} else { # no html, therefore output to a text console
    run_js_interpreter($r, $verbose);
}

my $ctxs = [];  # multiple V8 JavaScript engine contexts
my $ctx_id = 0; # a "handle" for a JavaScript engine context

sub run_js_interpreter {
    require V8;
    my $ctx_id = new_ctx(); # from the V8
    my $ctx = $ctxs->[$ctx_id]; # autovivify - initially undef
    my $runtime_js = ''; # to contain source from all runtime library files
    $runtime_js .= scalar(read_file($_)) for @js; # concatenate the .js source
    $ctx->execute($runtime_js); # load the runtime library functions into the context
    my $setting_js = ToJS::emit_js(
        STD->parse($s, actions => 'Actions', setting => $setting)->{'_ast'}
    );
    my $ast_js = ToJS::emit_js($_[0]); # convert the program AST object to JavaScript
    say $ast_js if $_[1]; # the verbose flag controls debug output
    my $start = gettimeofday(); # prepare to measure execution time
    # Run the Perl 6 interpreter on the AST of the user program.
    # TODO: integrate $setting_js into the setting instead of appending it here.
    eval { $ctx->execute('Act.interpret('.$ast_js.','.$setting_js.');') };
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
