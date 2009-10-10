#!/usr/local/bin/perl -w

use Getopt::Long;
my $execute_command = 'perl sprixel.pl';
my $spectest_data   = 'sprixel/spectest.data';
my $spectest_base   = '~/pugs/t/spec';
my $result = GetOptions(
    "execute_command=s"=>\$execute_command,
    "spectest_data=s"  =>\$spectest_data,
    "spectest_base=s"  =>\$spectest_base );

my @spectests = lines($spectest_data);
my %stats;
all_tests_begin(\%stats);
# Test harness outer loop
for my $spectest (@spectests) {
    next if $spectest =~ m/ ^ \s* ( \# .* )? $ /x; # no blanks / comments
    $spectest =~ m/ ^ (\S+) (\s+ .+)? $ /x; # extract name, directives
    my $testname = $1;
    my $scriptname = "$spectest_base/$testname";
    one_test_begin($testname); # display the test name and ......
    my @script = lines($scriptname); # read test script as array
    my ($todos,$skips) = get_directives($2); # process the optional directives
    apply_directives(\@script,$todos,$skips); # "fudge" tests known to fail
    my $tap_out = execute_script(\@script,$testname); # run tests
    my %result = read_tap($tap_out); # look for 1..3/ok 1/ok 2/ok 3 etc
    one_test_end(%result); # display the counts from plan .. miss
    update_stats(\%stats,%result);
}
all_tests_end(\%stats);
exit( $stats{'fail'}==0 ? 0 : 1 );

# Subroutines, in the order of appearance in the main program

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
        if ( $directive =~ / ^ (skip|todo) \( (.*) \) $ /x ) {
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
# Bug 2: some test scripts perform statements that should be skipped, on
# lines not headed with a test function. Rakudo and Pugs have a fudging
# solution that can skip a compound statement enclosed in curly braces.
# This can be applied here as well, but is not yet implemented.
sub apply_directives {
    my $refscript = shift; # reference to array of test script lines
    my $todos = shift; # reference to array of test numbers to todo
    my $skips = shift; # reference to array of test numbers to skip
    $todos = [] unless $todos;
    $skips = [] unless $skips;
    my @todos = @$todos; #print "todos:@todos\n";
    my @skips = @$skips; #print "skips:@skips\n";
#   return if (@todos==0 and @skips==0); # short circuit
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
#print "BLOCK:\n";
#for my $p (@block) { printf "%d:%d\n", $p->[0], $p->[1]; }
#print "BLOCK:\n";
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
#       printf "SKIP BLOCK: %d:%d\n", $index1, $index2;
        for $lineindex ($index1..$index2) {
            $$refscript[$lineindex] =~ / ^ (\s*) (.*) $ /x;
            my $spaces = $1;
            my $text = $2;
            $linetext = $$refscript[$lineindex];
            
            if ( $linetext =~ / ^ \s* ($testfunctions) (\(|\s) /x ) {
                $$refscript[$lineindex] = $spaces . "ok(1,'skip');";
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
#           printf "SKIP LINE: %d: %s\n", $lineindex, $$refscript[$lineindex];
        }
#       printf "SKIP BLOCK: %d:%d\n", $index1, $index2;
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
    my $tap_out = qx{$execute_command <<'SCRIPT_TEXT'\n$script\nSCRIPT_TEXT};
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
    printf( "%5d%5d%5d%5d%5d%5d%5d\n", $result{'plan'}, $result{'pass'},
        $result{'fail'}, $result{'todopass'}, $result{'todofail'},
        $result{'skip'}, $result{'miss'} );
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
    print " " x 43 . " ----" x 7 . "\n";
    printf "Total from%4d files" . "." x 23, $refstats->{'files'};
    printf( "%5d%5d%5d%5d%5d%5d%5d\n", $refstats->{'plan'},
        $refstats->{'pass'}, $refstats->{'fail'}, $refstats->{'todopass'},
        $refstats->{'todofail'}, $refstats->{'skip'}, $refstats->{'miss'});
    print "Result:".($refstats->{'fail'}==0 ? 'PASS' : 'FAIL')."\n";
}

=pod

=head1 NAME

harness-fudging - run all or selected specification tests from scripts

=head1 SYNOPSIS

    perl harness-fudging.pl [options]

=head2 OPTIONS

 --execute_command='perl sprixel.pl'
 --spectest_data='sprixel/spectest.data'
 --spectest_base='../../t/spec'

=head1 DESCRIPTION

This Perl 5 script takes three command line arguments. First, the
command that works like C<perl>, accepting a script via standard input.
Second, the name of a metadata file, see details below.
Third, the base directory of the suite to test.

=head1 METADATA

The --spectest_data option is the name of the metadata file, for example
F<sprixel/spectest.data>. In this file comment lines beginning with #
are ignored. Regular lines begin with the pathname of a test script,
relative to the base directory of the test suite, and optionally contain
todo or skip directives.

=head1 BUGS / LIMITATIONS

The fudge operation is the simplest thing that could possibly work, so
there will be fudge situations that other harnesses can handle, that
do not work here. Improving that is an ongoing task ;)

See Bug 1 and Bug 2 at sub apply_skips().

=head1 DESIGN

This script was written in Perl 5 to help the initial testing of sprixel.
Sprixel is an interpreter for STD parsed Perl 6 written in JavaScript,
and executed by Google's V8 JavaScript compiler. It is written for easy
translation to Perl 6.

=cut
