package main;
use utf8;

sub mangle {
    my @list = @_;
    for (@list) {
	s/\`/Grave/g;
	s/\~/Tilde/g;
	s/\!/Bang/g;
	s/\@/At/g;
	s/\#/Sharp/g;
	s/\$/Dollar/g;
	s/\%/Percent/g;
	s/\^/Caret/g;
	s/\&/Amp/g;
	s/\*/Star/g;
	s/\(/Paren/g;
	s/\)/Thesis/g;
	s/\-/Minus/g;
	s/\+/Plus/g;
	s/\=/Equal/g;
	s/\{/Cur/g;
	s/\}/Ly/g;
	s/\[/Bra/g;
	s/\]/Ket/g;
	s/\|/Vert/g;
	s/\\/Back/g;
	s/\:/Colon/g;
	s/\;/Semi/g;
	s/\'/Single/g;
	s/\"/Double/g;
	s/\</Lt/g;
	s/\>/Gt/g;
	s/\«/Fre/g;
	s/\»/Nch/g;
	s/\,/Comma/g;
	s/\./Dot/g;
	s/\?/Question/g;
	s/\//Slash/g;
	s/([^a-zA-Z_0-9])/sprintf("_%02x_",ord($1))/eg;
    }
    join '_', @list;
}
1;
