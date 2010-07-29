package MyBuilder;
use base 'Module::Build';

use warnings;
use strict;


use v5.10;

sub new {
    my ($self,@args) = @_;
    $self->SUPER::new('pm_files'=>{map {$_ => $_} grep {! -d $_} <lib/*>,<lib/*/*>},@args);
}
1;
