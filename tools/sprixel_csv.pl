#!/usr/bin/perl
use strict;
use v5.010;

use Actions;
use STD;
use Encode;
use Scalar::Util;
no warnings;

my $PROG = '';
my $setting = 'CORE';

my $r;
if (@ARGV and -f $ARGV[0]) {
    $r = STD->parsefile($ARGV[0], setting => $setting);
} elsif (@ARGV) {
  $r = STD->parse($ARGV[1] || $ARGV[0], setting => $setting);
} else {
    local $/;
    $PROG = <>;
    $PROG = $PROG;
    $r = STD->parse($PROG, setting => $setting);
}
#$r->{'stabs'} = $STD::ALL;

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
my $lastid = 1;
my %seen = ();
my %torun = ();

sub emit_csv {
  emit_csv_recurse($_[0]);
}

{ package Array; }

sub get_obj_id {
  my $o = shift;
  my $a;
  my $addr = Scalar::Util::refaddr($o);
  return $idmap{$addr} //= ++$lastid;
}

sub emit_csv_recurse {
  my ($self, $parent) = @_;
  unless(ref $self) {
    say ++$lastid . ',' . tps($self,1) . ",$parent";
    return;
  }
  
  my $addr = get_obj_id($self);
  return if exists $seen{$addr};
  $seen{$addr} = 1;
  say "$addr," . (ref $self) . ',' . ($parent // '""');
  for my $prop (keys %$self) {
    next if !defined $self->{$prop} || $prop eq '.' || $prop eq '_xact';
    if (ref $self->{$prop}) {
      if ('ARRAY' eq ref ($self->{$prop})) {
        $self->{$prop} = bless { '.' => $self->{$prop} }, 'Array';
        if (scalar @{$self->{$prop}->{'.'}}) {
          say '1,' . tps($prop) . ',' . get_obj_id($self->{$prop});
        }
      } else {
        say '1,' . tps($prop) . ',' . get_obj_id($self->{$prop});
      }
      $torun{get_obj_id($self->{$prop})} = $self->{$prop};
    } else {
      say '0,' . tps($prop) . ',' . tps($self->{$prop});
    }
  }
  if (defined $self->{'.'}) {
    my $idx = 0;
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
