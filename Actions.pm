package Actions;

# Generic ast translation done via autoload

our $AUTOLOAD;
my $SEQ = 1;

sub AUTOLOAD {
    my $self = shift;
    my $match = shift;
    return if @_;	# not interested in tagged reductions
    return if $match->{_ast}{_specific} and ref($match->{_ast}) =~ /^VAST/;
    print STDERR "AUTOLOAD $AUTOLOAD\n" if $OPT_log;
    my $r = hoistast($match);
    (my $class = $AUTOLOAD) =~ s/^Actions/VAST/;
    $class =~ s/__S_\d\d\d/__S_/ and $r->{_specific} = 1;
    if ($class =~ /::(infix|prefix|postfix|postcircumfix|dotty|regex_infix)__S_/) {
	$r->{_op} = $class;
	$class =~ s/::(infix|prefix|postfix|postcircumfix|dotty|regex_infix)__S_/::SYM_$1__S_/;
    }
    gen_class($class);
    bless $r, $class unless ref($r) =~ /^VAST/;
    $r->{MATCH} = $match if $OPT_match;
    $match->{'_ast'} = $r;
}

# propagate ->{'_ast'} nodes upward
# (untransformed STD nodes in output indicate bugs)

sub hoistast {
    my $node = shift;
    my $text = $node->Str;
    my %r;
    my @all;
    my @fake;
    for my $k (keys %$node) {
	print STDERR $node->{_reduced}, " $k\n" if $OPT_log;
	my $v = $node->{$k};
	if ($k eq 'O') {
	    for my $key (keys %$v) {
		$r{$key} = $$v{$key};
	    }
	}
	elsif ($k eq 'PRE') {
	}
	elsif ($k eq 'POST') {
	}
	elsif ($k eq 'SIGIL') {
	    $r{SIGIL} = $v;
	}
	elsif ($k eq 'sym') {
	    if (ref $v) {
		if (ref($v) eq 'ARRAY') {
		    $r{SYM} = $v;
		}
		elsif (ref($v) eq 'HASH') {
		    $r{SYM} = $v;
		}
		elsif ($v->{_pos}) {
		    $r{SYM} = $v->Str;
		}
		else {
		    $r{SYM} = $v->TEXT;
		}
	    }
	    else {
		$r{SYM} = $v;
	    }
	}
	elsif ($k eq '_arity') {
	    $r{ARITY} = $v;
	}
	elsif ($k eq '~CAPS') {
	    # print "CAPS ref ". ref($v) . "\n";
	    if (ref $v) {
		for (@$v) {
		    next unless ref $_;     # XXX skip keys?
		    push @all, $_->{'_ast'};
		}
	    }
	}
	elsif ($k eq '_from') {
	    $r{BEG} = $v;
	    $r{END} = $node->{_pos};
	    if (exists $::MEMOS[$v]{'ws'}) {
		my $wsstart = $::MEMOS[$v]{'ws'};
		$r{WS} = $v - $wsstart if defined $wsstart and $wsstart < $v
	    }
	}
	elsif ($k =~ /^[a-zA-Z]/) {
	    if ($k eq 'noun') {	# trim off PRE and POST
		$r{BEG} = $v->{_from};
		$r{END} = $v->{_pos};
	    }
	    if (ref($v) eq 'ARRAY') {
		my $zyg = [];
		for my $z (@$v) {
		    if (ref $z) {
			if (ref($z) eq 'ARRAY') {
			    push @$zyg, $z;
			    push @fake, @$z;
			}
			elsif (exists $z->{'_ast'}) {
			    my $zy = $z->{'_ast'};
			    push @fake, $zy;
			    push @$zyg, $zy;
			}
		    }
		    else {
			push @$zyg, $z;
		    }
		}
		$r{$k} = $zyg;
#		    $r{zygs}{$k} = $SEQ++ if @$zyg and $k ne 'sym';
	    }
	    elsif (ref($v)) {
		if (exists $v->{'_ast'}) {
		    push @fake, $v->{'_ast'};
		    $r{$k} = $v->{'_ast'};
		}
		elsif (exists $v->{'_from'}) {
		    $r{$k}{BEG} = $v->{'_from'};
		    $r{$k}{END} = $v->{'_pos'};
		    $r{$k}{TEXT} = $v->Str;
		}
		else {
		    # NAME or decl or sig or...
		    $r{$k} = $v;
		    next;
		}
#		    $r{zygs}{$k} = $SEQ++;
		unless (ref($r{$k}) =~ /^VAST/) {
		    my $class = "VAST::$k";
		    gen_class($class);
		    bless $r{$k}, $class unless ref($r{$k}) =~ /^VAST/;
		}
	    }
	    else {
		$r{$k} = $v;
	    }
	}
    }
    if (@all == 1 and defined $all[0]) {
	$r{'.'} = $all[0];
    }
    elsif (@all) {
	$r{'.'} = \@all;
    }
    elsif (@fake) {
	$r{'.'} = \@fake;
    }
    else {
	$r{TEXT} = $text;
    }
    \%r;
}

sub hoist {
    my $match = shift;

    my %r;
    my $v = $match->{O};
    if ($v) {
	for my $key (keys %$v) {
	    $r{$key} = $$v{$key};
	}
    }
    if ($match->{sym}) {
    #    $r{sym} = $match->{sym};
    }
    if ($match->{ADV}) {
	$r{ADV} = $match->{ADV};
    }
    \%r;
}

sub CHAIN {
    my $self = shift;
    my $match = shift;
    my $r = hoistast($match);

    my $class = 'VAST::Chaining';

    gen_class($class);
    $r = bless $r, $class;
    $match->{'_ast'} = $r;
}

sub LIST {
    my $self = shift;
    my $match = shift;
    my $r = hoist($match);

    my @list = @{$match->{list}};
    my @delims = @{$match->{delims}};
    $r->{'args'} = [ map { $_->{_ast} } @list ];
    my @all;
    while (@delims) {
	my $term = shift @list;
	push @all, $term->{_ast};
	my $infix = shift @delims;
	push @all, $infix->{_ast};
    }
    push @all, $list[0]->{_ast} if @list;
    pop @all while @all and not $all[-1]{END};
    $r->{BEG} = $all[0]{BEG};
    $r->{END} = $all[-1]{END} // $r->{BEG};
    $r->{'infix'} = $all[-2];  # assume final one is most representative
    $r->{'.'} = \@all;

    my $base = ucfirst $match->{O}{dba} // $match->{sym} // 'termish';
    $base =~ s/ /_/g;
    $base =~ s/^/VAST::/;

    my $class =
	$match->{delims}[0]{_ast}{infix}{_op} //
	$match->{delims}[0]{_ast}{regex_infix}{_op} //
	warn ::Dump($match);
    gen_class($class, $base);
    $r = bless $r, $class;
    $match->{'_ast'} = $r;
}

sub POSTFIX {
    my $self = shift;
    my $match = shift;
    my $r = hoist($match);
    my $arg = $match->{arg}->{_ast};
    $r->{'arg'} = $arg;
    $r->{postop} = $match->{postop}{_ast} if exists $match->{postop};
    my $a = $r->{'.'} = [$arg,$match->{_ast}];
    $r->{BEG} = $a->[0]->{BEG} // $match->{_from};
    $r->{END} = $a->[-1]->{END} // $match->{_pos};

    my $base = ucfirst $match->{O}{dba} // $match->{sym} // 'termish';
    $base =~ s/ /_/g;
    $base =~ s/^/VAST::/;

    my $class;
    if ($match->{fake}) {
	$class = $base;
	$base = '';
    }
    else {
	$class =
	    $match->{_ast}{postop}{postfix}{_op} //
	    $match->{_ast}{postop}{postcircumfix}{_op} //
	    $match->{_ast}{dotty}{_op} //
	    warn ::Dump($match);
    }

    gen_class($class, $base);
    $r = bless $r, $class;
    $match->{'_ast'} = $r;
}

sub PREFIX {
    my $self = shift;
    my $match = shift;
    my $r = hoist($match);
    my $arg = $match->{arg}->{_ast};
    $r->{'postop'} = $match->{postop}->{_ast} if exists $match->{postop};
    $r->{'arg'} = $arg;
    my $a = $r->{'.'} = [$match->{_ast},$arg];

    $r->{BEG} = $a->[0]->{BEG} // $match->{_from};
    $r->{END} = $a->[-1]->{END} // $match->{_pos};

    my $base = ucfirst $match->{O}{dba} // $match->{sym} // 'termish';
    $base =~ s/ /_/g;
    $base =~ s/^/VAST::/;

    my $class;
    if ($match->{fake}) {
	$class = $base;
	$base = '';
    }
    else {
	$class = $match->{_ast}{prefix}{_op} // warn ::Dump($match);
    }

    gen_class($class,$base);
    $r = bless $r, $class;
    $match->{'_ast'} = $r;
}

sub INFIX {
    my $self = shift;
    my $match = shift;
    my $r = hoist($match);
    my $left = $match->{left}->{_ast};
    my $right = $match->{right}->{_ast};
    if ($match->{middle}) { # ternary
	my $middle = $match->{middle}->{_ast};
	$r->{'args'} = [$left,$middle,$right];
    }
    else {
	$r->{'args'} = [$left,$right];
    }
    my $a = $r->{'.'} = [$left,$match->{_ast},$right];
    $r->{BEG} = $a->[0]->{BEG} // $match->{_from};
    $r->{END} = $a->[-1]->{END} // $match->{_pos};
    $r->{'infix'} = $a->[1];

    my $base = ucfirst $match->{O}{dba} // $match->{sym} // 'termish';
    $base =~ s/ /_/g;
    $base =~ s/^/VAST::/;

    my $class;
    if ($match->{fake}) {
	$class = $base;
	$base = '';
    }
    else {
	$class =
	    $match->{_ast}{infix}{_op} //
	    $match->{_ast}{regex_infix}{_op} //
	    warn ::Dump($match);
    }

    gen_class($class, $base);
    $r = bless $r, $class;
    $match->{'_ast'} = $r;
}

sub nibbler {
    my $self = shift;
    my $match = shift;
    my $r = hoist($match);
    if ($match->{nibbles}) {
	my @dot;
	for my $n ( @{ $match->{nibbles} } ) {
	    if (ref $n eq 'Str') {
		push @dot, bless($n,"VAST::Str");
	    }
	    elsif (ref $n eq 'VAST::Str') {
		push @dot, $n;
	    }
	    elsif (ref $n eq 'ARRAY') {
		push @dot, $n->[0]{_ast};
	    }
	    elsif ($n->{_ast}) {
		push @dot, $n->{_ast};
	    }
	    elsif ($n->{EXPR}) {
		push @dot, $n->{EXPR}->{_ast};
	    }
	    else {
		warn "Oops", ::Dump($n);
		exit;
	    }
	}
	my $a = $r->{'.'} = \@dot;
	$r->{BEG} = $a->[0]->{BEG} // $match->{_from};
	$r->{END} = $a->[-1]->{END} // $match->{_pos};
    }
    elsif ($match->{EXPR}) {	# regex?
	$r->{'.'} = $match->{EXPR}->{_ast};
	$r->{BEG} = $r->{'.'}->{BEG} // $match->{_from};
	$r->{END} = $r->{'.'}->{END} // $match->{_pos};
    }

    my $class = 'VAST::nibbler';
#	print STDERR ::Dump($r);
    gen_class($class);
    $r = bless $r, $class;
    $match->{'_ast'} = $r;
}

sub EXPR {
    return;
}

sub termish {
    my $self = shift;
    my $match = shift;
    $match->{'_ast'} = $match->{term}{'_ast'};
}

sub gen_class {
    my $class = shift;
    my $base = shift() // 'VAST::Base';
    # say $class;
    no strict 'refs';
    if (@{$class . '::ISA'}) {
	print STDERR "Existing class $class\n" if $OPT_log;
	return;
    }
    print STDERR "Creating class $class\n" if $OPT_log;
    @{$class . '::ISA'} = $base;
}

1;
