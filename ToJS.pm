use strict;
use warnings;

{ package ToJS;

my $jsk = {};

$jsk->{$_} = 1 for (qw(break continue do for import new this void case
  default else function in return typeof while comment delete export if label
  switch var with abstract implements protected boolean instanceOf public byte
  int short char interface static double long synchronized false native throws
  final null transient float package true goto private catch enum throw class
  extends try const finally debugger super));

sub tps {
    my $str = (shift).'';
    return '"'.$1.'"' if $str =~ /\(0x(\w+)/;
    $str =~ s/^VAST:://;
    return $str if (!(shift || 0) && $str =~ /^[\w_]+$/
        && !exists $jsk->{$str}) || $str =~ /^\d+$/;
    $str =~ s/([\\"])/\\$1/g;
    $str =~ s/\n/\\n/mg;
    return '"'.$str.'"';
}

my $seen = {};
my $jsi = 0;
my $app_act = '';
my $has_top = 0;
my $this_js_parent = '';
my $this_js_member = '';

sub jsind {
    "\n".(' ' x ($jsi += shift || 0))
}

sub emit_js {
    my $self = shift;
    return tps($self,1) unless $self =~ /\(0x/;
    #print("\nat ".ref($self));
    my $last_js_parent = $this_js_parent.'';
    my $text = '';
    my $is_top = 0;
    !$has_top && ($has_top = 1) && ($is_top = 1);
    $text = '(function(){var objs={};return ' if $is_top; # top level object
    if (exists $seen->{$self}) { # seen it before
        if ($seen->{$self}) { # inside it...
            $app_act .= 'objs['.$this_js_parent.']['
                .$this_js_member.']=objs['.tps($self).'];';
            return 'null'; # the circular reference will be injected later
        }
        return 'objs['.tps($self).']'; # it's a reference to a closed object
    }
    $text .= '( objs['.tps($self).'] = '; # never seen this object before
    $seen->{$self} = 1;
    $this_js_parent = tps($self);
    if (ref $self eq 'ARRAY') {
        my $idx = 0;
        my $array_text = '['.jsind(1);
        for my $item (@$self) {
            next if (!defined $item);
            $idx++ && ($array_text .= ','.jsind());
            $this_js_member = $idx;
            $array_text .= emit_js($item);
        }
        my $close_indent = jsind(-1);
        $text .= $array_text =~ /^\[\s+$/m
            ? '[]'
            : $array_text.$close_indent.']';
    }
    else { # it's a hash, probably blessed.
        $text .= "{".jsind(1)."T: ".tps(ref($self),1);
        for my $prop (keys %$self) {
            next if !defined $self->{$prop} || $prop eq '.';
            $text .= ','.jsind().tps($prop).': ';
            $this_js_member = tps($prop);
            $text .= emit_js($self->{$prop});
        }
        if (defined $self->{'.'}) { # not *exists*, *defined*.
            $text .= ','.jsind().'M: ';
            $this_js_member = '"M"';
            $text .= emit_js($self->{'.'});
        }
        $text .= jsind(-1).'} /* '.tps(ref($self)).' */';
    }
    $text .= ' )';
    $seen->{$self} = 0; # unmark "inside"
    $this_js_parent = $last_js_parent; # reset the parent
    if ($is_top) { # append post-declaration assignment actions
        $text .= $app_act.'})()';
    }
    return $text;
}

}

1;