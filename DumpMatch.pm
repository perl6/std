package DumpMatch;
use Term::ANSIColor;
use strict;
use warnings;

use Exporter;

our @ISA = 'Exporter';
our @EXPORT = qw(traverse_match dump_match);
our @EXPORT_OK = qw(process_events);
our $NOCOLOR;

sub RESET() {$NOCOLOR ? '' : Term::ANSIColor::RESET()};
sub RED()   {$NOCOLOR ? '' : Term::ANSIColor::RED()  };
sub BLUE()  {$NOCOLOR ? '' : Term::ANSIColor::BLUE() };
sub YELLOW()  {$NOCOLOR ? '' : Term::ANSIColor::YELLOW() };

sub process_events {
    my ($orig,$events,$opt) = @_;
    my $str = "";
    my $at = 0;
    my $indent=0;
    # I know, XML sucks, any suggestions? ;)
    for (sort {$a->[0] <=> $b->[0] or $a->[4] <=> $b->[4]} @{$events}) {
        my $text = substr($orig,$at,$_->[0]-$at);
        if ($opt->{vertical}) {
            if ($text) {
                # not sure about that
                $text =~ s/\n/\\n/;
                $str .= " " x $indent . $text . "\n";
            }
        } else {
            $str .= $text;
        }
        $at = $_->[0];
    
        if ($_->[1] eq 'from') {
            if ($opt->{vertical}) {
                $str .= " " x $indent . BLUE.$_->[2].RED.":".RESET."\n";
                $str .= " " x $indent . YELLOW.$opt->{actions}->($_->[3]->{''}).RESET."\n" if $opt->{actions} && $_->[3]->{''};
                $indent++;
            } else {
                $str .= RED."<".BLUE.$_->[2].RED.">".RESET;
            }
        } elsif ($_->[1] eq 'to') {
            if ($opt->{vertical}) {
                $indent--;
            } else {
                $str .=  RED."</".BLUE.$_->[2].RED.">".RESET;
            }
        }
    }
    $str;
}
sub traverse_match {
    my ($r,$label,$depth,$events) = @_;
    return unless ref $r;
     if (defined $r->{_from}) {
         if ($r->{_from} == $r->{_to}) {
            push(@{$events},[$r->{_from},'empty',$label,$r,$depth]);
         }  else {
            push(@{$events},[$r->{_from},'from',$label,$r,$depth]);
            push(@{$events},[$r->{_to},'to',$label,$r,-$depth]);
        }
     }
    for my $name (keys %$r) {
        next if $name eq '';
        my $v = $r->{$name};
        if (ref $v eq 'ARRAY') {
            for my $i (0 .. scalar @{$v}) {
                traverse_match($v->[$i],$name,$depth+1,$events);
            }
        } elsif (ref $v eq 'SCALAR') {
        } elsif (ref $v) {
            traverse_match($v,$name,$depth+1,$events);
        } else {
        }
    }
}
sub dump_match {
    my $name = shift;
    my $r = shift;
    my $opt = shift || {};
    my $events = [];
    traverse_match($r,$name,0,$events);
    process_events(${$r->{_orig}},$events,$opt);
}
1;
