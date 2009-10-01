#!/usr/local/bin/perl -w

# Get parameters from @ARGV
my $execute_command = shift; # eg 'perl sprixel.pl'
my $spectest_data   = shift; # eg 'sprixel/spectest.data'
my $spectest_base   = shift; # eg '~/pugs/t/spec'
my @spectests = lines($spectest_data);
# Test harness outer loop
for my $spectest (@spectests) {
    # skip blank and pure comment lines
    next if $spectest =~ m/ ^ \s* ( \# .* )? $ /x;
    $spectest =~ m/ ^ (\S+) (\s+ skip .+)? $ /x;
    my @script = lines("$spectest_base/$1");
    my @skips = get_skip_list( $2 );
    apply_skips( \@script, \@skips ); # "fudge" tests we cannot run ;)
    my $script = join( "\n", @script );
    # run the test script in a child process
    my $output = qx{$execute_command <<'SCRIPT_TEXT'\n$script\nSCRIPT_TEXT};
    print "OUTPUT:\n$output\nOUTPUT END\n";
}

# Convert for example an optional "skip 2 5..8 10" into (2,5,6,7,8,10)
sub get_skip_list {
    my $skip_directive = shift;
    return () unless defined($skip_directive);
    my @skip_numbers;
    my @skip_words = split ' ', $2;
    print "SKIP_WORDS:",join( ",", @skip_words ), "\n";
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
            print "BAD WORD: $skip_word\n";
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
# Bug 2: some test scripts perform statements that should be skipped,
# in other 
sub apply_skips {
    my $refscript = shift;
    my $refskip = shift;
    return unless defined($refskip);
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
        print "LINE$lineindex:";
        if ( $linetext =~ / ^ \s* ($testfunctions) (\(|\s) /x ) {
            $testline[$testnumber] = $lineindex;
            print "TEST $testnumber:";
            $testnumber++;
        }
        print "$linetext\n";
    }
    # Replace the tests in the skip list
    if ( defined($refskip) ) {
        for $testnumber (@$refskip) {
            $lineindex = $testline[$testnumber];
            print "CHANGE $testnumber LINE $lineindex:" . $$refscript[$lineindex] . "\n";
            $$refscript[$lineindex] = "ok(True); # skip ".$$refscript[$lineindex];
            print "BECOMES " . $$refscript[$lineindex] . "\n";
        }
    }
}

# Imitate the Perl 6 lines() built in function
sub lines {
    my $filename = shift;
    my @lines = ();
    open my $handle, '<', $filename or die "cannot open $filename: $!";
    while ( my $line = <$handle> ) {
        chomp $line;
        push @lines, $line;
    }
    close $handle;
    return @lines;
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

=head1 LIMITATIONS

The fudge operation is the simplest thing that could possibly work, so
there will be fudge situations that other harnesses can handle, that
do not work here. Improving that is an ongoing task ;)



=head1 DESIGN

This script was written in Perl 5 to help the initial testing of sprixel.
Sprixel is an interpreter for STD parsed Perl 6 written in JavaScript,
and executed by Google's V8 JavaScript compiler. It is written for easy
translation to Perl 6.

=cut

