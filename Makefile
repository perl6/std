STD.pmc: STD.pm gimme5 try5 Cursor.pmc LazyMap.pm mangle.pl
	./gimme5 STD.pm >STD.pmc
	perl -c STD.pmc
	rm -rf lex
	# pre-generate common sublexers
	./try5 comp_unit -e 'say "howdy" ~ "";'
