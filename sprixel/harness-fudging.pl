#!/usr/local/bin/perl -w

my %option = get_options();
my %stats;
all_tests_begin(\%stats);
# Test harness outer loop
for my $spectest (@{$option{'tests'}}) {
    $spectest =~ m/ ^ (\S+) (\s+ .+)? $ /x; # extract name, directives
    my $testname = $1;
    my $scriptname = "$option{spectest_base}/$testname";
    one_test_begin($testname); # display the test name and ......
    my @script = lines($scriptname); # read test script as array
    my ($todos,$skips) = get_directives($2); # process the optional directives
    apply_directives(\@script,$todos,$skips); # "fudge" tests known to fail
    my $tap_out = execute_script(\@script,$testname); # run tests
    my %result = read_tap($tap_out); # look for 1..3/ok 1/ok 2/ok 3 etc
    one_test_end(%result); # display the counts from plan .. miss
    update_stats(\%stats,%result);
}
all_tests_end(\%stats); # exits

# Subroutines, in order of first call

sub get_options {
    use Getopt::Long;
    my %option = (
        'exec'          => 'perl sprixel.pl',
        'spectest_data' => 'sprixel/spectest.data',
        'spectest_base' => '../../t/spec');
    my $result = GetOptions(
        "exec=s"          => \$option{'exec'},
        "spectest_data=s" => \$option{'spectest_data'},
        "spectest_base=s" => \$option{'spectest_base'} );
    # after the options, any remaining arguments are test names and directives
    my @spectest_lines;
    if ( @ARGV ) {
        @spectest_lines = "@ARGV";
    }
    else {
        @spectest_lines = grep { / ^ [^#] /x }
            lines($option{'spectest_data'});
    }
    $option{'tests'} = \@spectest_lines;
    return %option;
}

# Imitate the Perl 6 lines() built in function
sub lines {
    my $filename = shift;
    my @lines = ();
    open my $handle, '<', $filename or die "cannot open $filename: $!";
    while ( my $line = <$handle> ) {
        push @lines, $line;
    }
    close $handle;
    chomp @lines;
    return @lines;
}

# Initialize counters and a timer, print column headings
sub all_tests_begin {
    my $stats = shift;
    %$stats = (plan=>0, pass=>0, fail=>0, todopass=>0, todofail=>0,
               skip=>0, miss=>0, files=>0, start=>time);
    print " " x 45, "plan pass fail todo(p f) skip miss\n";
}

# Show the test name and wait on the same line to print the results
sub one_test_begin {
    my $testname = shift;
    print $testname . "." x (44 - length($testname));
}

# Convert a string of directives such as "todo(4,8) skip(2,5..8,10) todo(3)"
# into ( [3,4,8], [2,5,6,7,8,10] )
sub get_directives {
    my $directives = shift;
    return () unless defined($directives);
    my %numbers = ( 'todo'=>{}, 'skip'=>{} );
    for my $directive (split ' ', $directives) {
        if ( $directive =~ / ^ (skip|todo) \= (.*) $ /x ) {
#       if ( $directive =~ / ^ (skip|todo) \( (.*) \) $ /x ) {
            my $todo_or_skip = $1;
            my $ranges = $2;
            for my $range (split ',', $ranges) {
                my ($num1,$num2) = ($range,$range);
                if ( $range =~ / ^ (\d+) \.\. (\d+) $ /x ) { # range m..n
                    ($num1,$num2) = ($1,$2);
                }
                elsif ( $range !~ / ^ \d+ $ /x ) { # a single number
                    die "bad line range: $range\n";
                }
                for my $num ($num1 .. $num2) {
                    $numbers{$todo_or_skip}{$num} = 1;
                }
            }
        }
        else {
            die "bad directive: $directive";
        }
    }
    # delete the todo number for tests that also appear in skip
    for my $num (keys %{$numbers{'skip'}}) {
        delete $numbers{'todo'}{$num} if exists $numbers{'todo'}{$num};
    }
    return( [sort {$a<=>$b} keys %{$numbers{'todo'}}],
            [sort {$a<=>$b} keys %{$numbers{'skip'}}] );
}

# Given a reference to an array of script lines, and a reference to an
# array of test numbers to skip, replace those numbered test calls with
# calls to skip().
# Bug 1: this does a static numbering of test calls, whilst test output
# is numbered dynamically. Tests called within loops will cause the two
# number sequences to differ. There is no proposed remedy for this bug.
sub apply_directives {
    my $refscript = shift; # reference to array of test script lines
    my $todos = shift; # reference to array of test numbers to todo
    my $skips = shift; # reference to array of test numbers to skip
    $todos = [] unless $todos;
    $skips = [] unless $skips;
    my @todos = @$todos; #print "todos:@todos\n";
    my @skips = @$skips; #print "skips:@skips\n";
    return if (@todos==0 and @skips==0); # short circuit
    my @testindex;
    my $current_block = -1;
    my @block;
    my %block_for_test;
    my $linenumber = 1;
    my $testnumber = 1;
    my $testfunctions = join('|', qw{ok is isnt pass flunk isa_ok
        dies_ok lives_ok skip todo force_todo use_ok cmp_ok is_deeply
        like skip_rest unlike eval_dies_ok eval_lives_ok approx
        is_approx throws_ok});
    # Identify the line index for each test number
    for ( $lineindex=0; $lineindex<@$refscript; ++$lineindex ) {
        my $linetext = $$refscript[$lineindex];
        if ( $linetext =~ / ^ \{ $ /x ) {
            if ( $current_block == -1 ) { # if not already in a block
                push @block, [$lineindex+1,-1];
                $current_block = $#block;
            }
        }
        elsif ( $linetext =~ / ^ \} $ /x ) {
            if ( $current_block >= 0 ) { # if still in a block
                $block[$current_block][1] = $lineindex-1;
                $current_block = -1;
            }
        }
        elsif ( $linetext =~ / ^ \s* ($testfunctions) (\(|\s) /x ) {
            $testindex[$testnumber] = $lineindex;
            $block_for_test{$testnumber} = $current_block if $current_block>=0;
            $testnumber++;
        }
    }
    # Identify the blocks containing tests to skip, and tests not in blocks
    my %blocks_to_skip;
    my %single_tests_to_skip;
    for $testnumber (@skips) {
        if ( exists $block_for_test{$testnumber} ) {
            $blocks_to_skip{$block_for_test{$testnumber}} = 1;
        }
        else {
            $single_tests_to_skip{$testnumber} = 1;
        }
    }
    # Within each block to skip, replace every test line with an ok
    # followed by a skip comment, and comment out all the other lines.
    for $current_block (keys %blocks_to_skip) {
        my ( $index1, $index2 ) = @{$block[$current_block]};
        for $lineindex ($index1..$index2) {
            $$refscript[$lineindex] =~ / ^ (\s*) (.*) $ /x;
            my $spaces = $1;
            my $text = $2;
            $linetext = $$refscript[$lineindex];
            if ( $linetext =~ / ^ \s* ($testfunctions) (\(|\s) /x ) {
                $$refscript[$lineindex] = $spaces . "skip('block',1);";
                # this line contains a test
            }
            elsif ( $linetext =~ / ^ \s* $ /x ) {
                # this is a blank line, leave it alone
            }
            elsif ( $linetext =~ / ^ \s* \# /x ) {
                # this line is already a comment, leave it alone
            }
            else {
                # this line contains other code, comment it out
                $$refscript[$lineindex] = $spaces . '# ' . $text;
            }
        }
    }
    # Replace the tests in the skip list
    for $testnumber (keys %single_tests_to_skip) {
        if ( $testnumber <= $#testindex ) {
            $lineindex = $testindex[$testnumber];
            my $linetext = $$refscript[$lineindex];
            # TODO: rewrite to not remove single quotes
            $linetext =~ s/\'//g; # this crashes Test.pm.js :(
            $$refscript[$lineindex] = "skip('$testnumber',1);";
        }
        else {
            warn "cannot skip test $testnumber - highest is $#testindex";
        }
    }
}

sub execute_script {
    my $refscript = shift; # reference to array of test script lines
    my $testname =  shift; # for diagnostics and reporting
    my $script = join( "\n", @$refscript );
    # DEBUG: save $script to a file to show which tests will be run
    $testname =~ s{/}{_}g;
    my $scriptpath_fudged = '/tmp/sprixel_'.$testname;
    $scriptpath_fudged =~ s/ \. t $ /\.sprixel/x;
#   unlink $scriptpath_fudged;
    open my $handle, '>', $scriptpath_fudged or die "cannot open $scriptpath_fudged: $!";
    print $handle $script;
    close $handle;
    # run the test script in a child process
    my $tap_out = qx{$option{exec} <<'SCRIPT_TEXT'\n$script\nSCRIPT_TEXT};
    # DEBUG: save $tap_out to a file to show test results
    my $temp_name = '/tmp/sprixel_'.$testname;
    $temp_name =~ s/ \. t $ /\.sprixel\.out/x;
#   unlink $temp_name;
    open $handle, '>', $temp_name or die "cannot open $temp_name: $!";
    print $handle $tap_out;
    close $handle;
    return $tap_out;
}

sub read_tap {
    my $tap_out = shift;
    my @tap_out = split("\n",$tap_out);
    my $test_number = 0;
    my( $time1, $time2 );
    my %result = ( plan=>0, pass=>0, fail=>0, todopass=>0, todofail=>0,
                   skip=>0, miss=>0, files=>1, time=>-1);
    # Predefine patterns that appear in the TAP output, so that the code
    # below can be more compact and easier to read. 
    my $plan     = '^ 1 \.\. (\d+) $';
    my $skip     = '^ ok \s (\d+) \s \# \s skip (.*) $';
    my $todopass = '^ ok \s (\d+) \s \# \s TODO (.*) $';
    my $todofail = '^ not \s ok \s (\d+) \s \# \s TODO (.*) $';
    my $pass     = '^ ok \s (\d+) (.*) $';
    my $fail     = '^ not \s ok \s (\d+) (.*) $';
    my $time     = '^ \# \s time \s* \: \s* (\d+\.\d+) $';
    my $comment  = '^ \# (.*) $';
    for $tap_line (@tap_out) {
        if    ( $tap_line =~ m/$plan/x     ) { $result{'plan'} = $1; }
        elsif ( $tap_line =~ m/$skip/x     ) { $result{'skip'}++; }
        elsif ( $tap_line =~ m/$todopass/x ) { $result{'todopass'}++; }
        elsif ( $tap_line =~ m/$todofail/x ) { $result{'todofail'}++; }
        elsif ( $tap_line =~ m/$pass/x     ) { $result{'pass'}++; }
        elsif ( $tap_line =~ m/$fail/x     ) { $result{'fail'}++; }
        elsif ( $tap_line =~ m/$time/x     ) { $time2 = $1; }
        elsif ( $tap_line =~ m/$comment/x  ) { ; }
    }
    $result{'miss'} = $result{'plan'} - $result{'pass'} - $result{'fail'}
        - $result{'todopass'} - $result{'todofail'} - $result{'skip'};
    return %result;
}

sub one_test_end {
    my %result = @_;
    if ( $result{'plan'} > 0 and $result{'plan'} == $result{'pass'} ) {
        my @emoticons = ("\\o/", ":-)");
        printf( "%5d%5d  %s\n", $result{'plan'}, $result{'pass'},
            $emoticons[2 * rand] );
    }
    else {
        printf( "%5d%5d%5d%5d%5d%5d%5d\n", $result{'plan'},
            $result{'pass'}, $result{'fail'}, $result{'todopass'},
            $result{'todofail'}, $result{'skip'}, $result{'miss'} );
    }
}

sub update_stats {
    my $refstats = shift;
    my %results = @_;
    for my $key ( qw[plan pass fail todopass todofail skip miss files] ) {
        $refstats->{$key} += $results{$key};
    }
}

sub all_tests_end {
    my $refstats = shift;
    print " " x 44 . " ----" x 7 . "\n";
    my $seconds = time - $refstats->{'start'};
    my $duration;
    if ( $seconds < 120 ) { $duration = sprintf "%d seconds", $seconds; }
    else { $duration = sprintf "%d minutes", $seconds/60; }
    my $label = sprintf "Totals from %d files in %s",
        $refstats->{'files'}, $duration;
    print $label . '.' x (44 - length($label));
    printf( "%5d%5d%5d%5d%5d%5d%5d\n", $refstats->{'plan'},
        $refstats->{'pass'}, $refstats->{'fail'}, $refstats->{'todopass'},
        $refstats->{'todofail'}, $refstats->{'skip'}, $refstats->{'miss'});
    my $all_pass = ($refstats->{'plan'}==$refstats->{'pass'} and
        $refstats->{'plan'}>0);
    print "Result:".($all_pass ? 'PASS' : 'FAIL')."\n";
    exit( $all_pass ? 0 : 1 );
}

=pod

=head1 NAME

harness-fudging - run all or selected specification tests from scripts

=head1 SYNOPSIS

    perl harness-fudging.pl [options] [tests]

Options are described below. The optional [tests] parameters are
formatted like lines out of spectest.data: a relative file name followed
by optional skip and todo directives. For example:

    perl harness-fudging.pl S03-operators/equality.t skip=5..9,14,15

=head2 OPTIONS

All options have default values ideal for sprixel testing.

 --exec='perl sprixel.pl'
 --spectest_data='sprixel/spectest.data'
 --spectest_base='../../t/spec'

The C<--exec> option specifies how to start the script interpreter.

=head1 DESCRIPTION

The harness-fudging.pl script runs test programs using TAP (Test
Anything Protocol).
takes three command line arguments. First, the
command that works like C<perl>, accepting a script via standard input.
Second, the name of a metadata file, see details below.
Third, the base directory of the suite to test.

=head1 DESIGN

This script was written in Perl 5 to help the initial testing of sprixel.
Sprixel is an interpreter for STD parsed Perl 6 written in JavaScript,
and executed by Google's V8 JavaScript compiler. It is written for easy
translation to Perl 6.

=head1 METADATA

The --spectest_data option is the name of the metadata file, for example
F<sprixel/spectest.data>. In this file comment lines beginning with #
are ignored. Regular lines begin with the pathname of a test script,
relative to the base directory of the test suite, and optionally contain
todo or skip directives.

Directives to todo or skip follow the test name, for example:

 S03-operators/equality.t skip=5..9,14,15

=head1 BUGS / LIMITATIONS

The apply_directives() is rather simple, so there will be fudge
situations that other harnesses can handle, that do not work here.
Improving that is an ongoing task ;)

See Bug 1 by sub apply_directives().

The semantics of the Test.pm.js version of skip() are probably
incompatible with other implementations - see skip in Test::Tutorial.

=head1 TODO

Devise better skip formats, for example to skip a single test within a
code block. Trying keying skip positions on test script content.

Scan test scripts for passes not yet in spectest.data.

Try to selectively unskip skipped tests to get more passes or fails.

Support comments at the end of test lines in spectest.data.

=head1 SEE ALSO

Test::Tutorial, Test::More, TAP::Harness

=cut
