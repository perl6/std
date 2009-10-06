#!/usr/local/bin/perl -w

use Getopt::Long;
# GetOptions
#my $execute_command = shift; # eg 'perl sprixel.pl'
#my $spectest_data   = shift; # eg 'sprixel/spectest.data'
#my $spectest_base   = shift; # eg '~/pugs/t/spec'

# Get parameters from @ARGV
my $execute_command = shift; # eg 'perl sprixel.pl'
my $spectest_data   = shift; # eg 'sprixel/spectest.data'
my $spectest_base   = shift; # eg '~/pugs/t/spec'
my @spectests = lines($spectest_data);
my %stats;
all_tests_begin(\%stats);
# Test harness outer loop
for my $spectest (@spectests) {
    next if $spectest =~ m/ ^ \s* ( \# .* )? $ /x; # no blanks / comments
    $spectest =~ m/ ^ (\S+) (\s+ skip .+)? $ /x; # get name, directive
    my $testname = $1;
    my $scriptname = "$spectest_base/$testname";
    one_test_begin($testname);
    my @script = lines($scriptname); # read test script as array
    my @skips = get_skip_list($2); # process the optional skip directive
    apply_skips(\@script,\@skips); # "fudge" tests we cannot run ;)
    my $tap_out = execute_script(\@script,$scriptname); # run tests
    my %result = read_tap($tap_out); # look for 1..3/ok 1/ok 2/ok 3 etc
    one_test_end(%result);
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
               skip=>0, start=>time);
    print " " x 41, "plan pass fail todo(p/f) skip\n";
}

# Show the test name and wait on the same line to print the results
sub one_test_begin {
    my $testname = shift;
    print $testname . "." x (40 - length($testname));
}

# Convert for example an optional "skip 2 5..8 10" into (2,5,6,7,8,10)
sub get_skip_list {
    my $skip_directive = shift;
    return () unless defined($skip_directive);
    my @skip_numbers;
    my @skip_words = split ' ', $2;
#   print "SKIP_WORDS:",join( ",", @skip_words ), "\n";
    # TODO: check that the first word is 'skip' and deal with errors
    shift @skip_words;
    for my $skip_word (@skip_words) {
        if ( $skip_word =~ / ^ \d+ $ /x ) { # a single number
            push @skip_numbers, 0 + $skip_word;
        }
        elsif ( $skip_word =~ / ^ (\d+) \.\. (\d+) $ /x ) { # range m..n
            push @skip_numbers, $1..$2;
        }
        else {
            print "BAD SKIP WORD: $skip_word\n";
        }
    }
    return @skip_numbers;
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
sub apply_skips {
    my $refscript = shift; # reference to array of test script lines
    my $refskip = shift; # reference to array of test numbers to skip
    return unless defined($refskip); # no skip list is ok, just a no-op
    my @testline;
    my $linenumber = 1;
    my $testnumber = 1;
    my $testfunctions = join('|', qw{ok is isnt pass flunk isa_ok
        dies_ok lives_ok skip todo force_todo use_ok cmp_ok is_deeply
        like skip_rest unlike eval_dies_ok eval_lives_ok approx
        is_approx throws_ok});
    # Identify the line number for each test number
    for ( $lineindex=0; $lineindex<@$refscript; ++$lineindex ) {
        my $linetext = $$refscript[$lineindex];
#       print "LINE$lineindex:";
        if ( $linetext =~ / ^ \s* ($testfunctions) (\(|\s) /x ) {
            $testline[$testnumber] = $lineindex;
#           print "TEST $testnumber:";
            $testnumber++;
        }
#       print "$linetext\n";
    }
    # Replace the tests in the skip list
    if ( defined($refskip) ) {
        for $testnumber (@$refskip) {
            if ( $testnumber <= $#testline ) {
                $lineindex = $testline[$testnumber];
                my $linetext = $$refscript[$lineindex];
                # TODO: rewrite to not remove single quotes
                $linetext =~ s/\'//g; # this crashes Test.pm.js :(
                $$refscript[$lineindex] = "skip('$testnumber',1);";
            }
            else {
                warn "cannot skip test $testnumber - highest is $#testline";
            }
        }
    }
}

sub execute_script {
    my $refscript = shift; # reference to array of test script lines
    my $scriptpath =  shift; # for diagnostics and reporting
    my $script = join( "\n", @$refscript );
    # DEBUG: save $script to a file to show which tests will be run
    my $scriptpath_fudged = $scriptpath;
    $scriptpath_fudged =~ s/ \. t $ /\.sprixel/x;
    unlink $scriptpath_fudged;
#   open my $handle, '>', $scriptpath_fudged or die "cannot open $filename: $!";
#   print $handle $script;
#   close $handle;
    # run the test script in a child process
    my $tap_out = qx{$execute_command <<'SCRIPT_TEXT'\n$script\nSCRIPT_TEXT};
    # DEBUG: save $tap_out to a file to show test results
    my $temp_name = $scriptpath;
    $temp_name =~ s/ \. t $ /\.sprixel\.out/x;
    unlink $temp_name;
#   open $handle, '>', $temp_name or die "cannot open $filename: $!";
#   print $handle $tap_out;
#   close $handle;
    return $tap_out;
}

sub read_tap {
    my $tap_out = shift;
    my @tap_out = split("\n",$tap_out);
    my $test_number = 0;
    my( $time1, $time2 );
    my %result = (plan=>0,pass=>0,fail=>0,todopass=>0,todofail=>0,skip=>0,time=>-1);
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
    return %result;
}

sub one_test_end {
    my %result = @_;
    printf( "%5d%5d%5d%5d%5d%5d\n", $result{'plan'}, $result{'pass'},
        $result{'fail'}, $result{'todopass'}, $result{'todofail'},
        $result{'skip'}, );
}

sub update_stats {
    my $refstats = shift;
    my %results = @_;
    for my $key ( qw[plan pass fail todopass todofail skip] ) {
        $refstats->{$key} += $results{$key};
    }
}

sub all_tests_end {
    my $refstats = shift;
    print " " x 40 . " ----" x 6 . "\n";
    print "Total" . "." x 35;
    printf( "%5d%5d%5d%5d%5d%5d\n", $refstats->{'plan'}, $refstats->{'pass'}, $refstats->{'fail'}, $refstats->{'todopass'},$refstats->{'todofail'},$refstats->{'skip'});
    print "Result:".($refstats->{'fail'}==0 ? 'PASS' : 'FAIL')."\n";
}

=pod

=head1 NAME

harness-fudging - run all or selected specification tests from scripts

=head1 DESCRIPTION

This Perl 5 script takes three command line arguments. First, the
command that works like C<perl>, accepting a script via standard input.
Second, the name of a metadata file, see details below.
Third, the base directory of the suite to test.

=head1 METADATA

The second command line argument is the name of the metadata file, for
example F<sprixel/spectest.data>. Comment lines beginning with # are
ignored. Regular lines begin with the pathname of a test script,
relative to the base directory of the test suite, and optionally a list
of test numbers to skip.

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

