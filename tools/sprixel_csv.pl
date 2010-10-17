#!/usr/bin/perl
use strict;
use v5.010;

use Actions;
use STD;
use Encode;
use Scalar::Util qw( blessed reftype refaddr );
use File::Slurp;
no warnings;

my $PROG = '';
my $setting = 'CORE';

my $r;
if (@ARGV and -f $ARGV[0]) {
  $PROG = read_file($ARGV[0]);
  $r = STD->parsefile($ARGV[0], setting => $setting);
} elsif (@ARGV) {
  $PROG = $ARGV[1] || $ARGV[0];
  $r = STD->parse($PROG, setting => $setting);
} else {
  local $/;
  $PROG = <>;
  $PROG = $PROG;
  $r = STD->parse($PROG, setting => $setting);
}
$r->{'stabs'} = $STD::ALL;

sub str_esc {
  my $out = shift;
  $out =~ s/([\\"])/\\$1/g;
  $out =~ s/\n/\\n/mg;
  return '"' . $out . '"';
}


sub tps {
    my $str = Encode::decode_utf8(shift);
    return '""' unless defined $str;
    return '"'.$1.'"' if $str =~ /\(0x(\w+)/;
    $str =~ s/^VAST:://;
    return $str if (!(shift || 0) && $str =~ /^[\w_]+$/);
    $str =~ s/([\\"])/\\$1/g;
    $str =~ s/\n/\\n/mg;
    return '"'.$str.'"';
}

my %idmap = ();
my $lastid = 2;
my %seen = ();
my %torun = ();

sub emit_csv {
  say "0," . str_esc($PROG) . ",1";
  emit_csv_recurse($_[0]);
}

{ package Array; }

sub get_obj_id {
  return $idmap{refaddr(shift)} //= ++$lastid;
}

sub emit_csv_recurse {
  my ($self, $parent) = @_;
  unless(ref $self) {
    say ++$lastid . ',' . tps($self,1) . ",$parent";
    return;
  }
  
  my $addr = get_obj_id($self);
  if (exists $seen{$addr}) {
    if (defined $parent) {
      say "2,$addr,$parent";
    }
    return;
  }
  $seen{$addr} = 1;
  say "$addr," . (ref $self) . ',' . ($parent // '""');
  for my $prop (keys %$self) {
    next if !defined $self->{$prop} || $prop eq '.' || $prop eq '_xact';
    my $p = $self->{$prop};
    my $reftype;
    if ($reftype = reftype($p)) {
      my $ra = get_obj_id($p);
      if ('ARRAY' eq $reftype) {
        my $z = $p;
        $p = $self->{$prop} = bless { '.' => $p }, 'Array';
        if (scalar @$z) {
          my $oid = get_obj_id($p);
          say '1,' . tps($prop) . ',' . $oid;
          $torun{$oid} = $p;
        }
      } elsif ('HASH' eq $reftype) {
        say '1,' . tps($prop) . ',' . $ra;
        $torun{$ra} = $p;
      } else {
        say '1,' . tps($prop) . ',' . tps($p);
      }
    } else {
      say '0,' . tps($prop) . ',' . tps($p);
    }
  }
  if (defined $self->{'.'}) {
    for my $kid (@{$self->{'.'}}) {
      emit_csv_recurse($kid, $addr);
    }
  }
}

emit_csv( $r );
while (scalar keys %torun) {
  my @tor = keys %torun;
  for my $kidkey (@tor) {
    emit_csv_recurse( $torun{$kidkey} );
    delete $torun{$kidkey};
  }
}


1;
