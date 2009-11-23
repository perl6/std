use 5.010;
use strict;
use Stash;
my $id = "MY:file<NULL.pad>:line(1):pos(0)";
my $core = Stash::->new(
    '!id' => [$id],
    '!file' => 'NULL.pad', '!line' => __LINE__ ,
);

sub fixup {
    my $stash = shift;
    return unless $stash;
    for my $k (keys %$stash) {
	next if $k =~ /^\./;
	my $v = $stash->{$k};
	if ($k =~ /^\w/) {
	    next if $k eq 'EXPORT::' or $k =~ /^_/;
	    $stash->{"&$k"} = $stash->{$k} if $k =~ /^\w+$/;
	    fixup($v);
	}
    }
    for my $k (keys %$stash) {
	next if $k =~ /^\!/;
	my $v = $stash->{$k};
	if ($v->{export}) {
	    $stash->{'EXPORT::'}->{ '$v->{export}' . '::' }->{$k} = $v;
	}
    }
}
fixup($core);
my $ALL = {
    'CORE' => $core,
    'MY:file<NULL.pad>' => $core,
    'SETTING' => $core,
    $id => $core,

};
$ALL;
