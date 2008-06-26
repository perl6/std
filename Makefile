STD5.pm: STD.pm gimme5 STD5_run
	./gimme5 STD.pm >STD5.pm
	perl -c STD5.pm
	rm -rf lex
	# pre-generate common sublexers
	./STD5_run comp_unit -e 'say "howdy" ~ "";'
