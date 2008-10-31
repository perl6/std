.PHONY: check try cat clean distclean purge test

FIXINS=Cursor.pmc LazyMap.pm mangle.pl

all: $(FIXINS) check try

STD.pmc: STD.pm gimme5
	./gimme5 $< >STD.pm5
	perl -p -e 'next if /^---/../\A\w+\Z/;' -e 's/\A[ \t]+//;' STD.pm5 >$@
	rm -rf lex

check: STD.pmc
	/usr/local/bin/perl -c $<

try: try5.out
	@echo "View $< to see output"
	@echo "or run: make try cat"

# pre-generate common sublexers
try5.out: try5 
	./try5 comp_unit -e 'say "howdy" ~ "";' > $@

cat:
	cat try5.out

clean:
	rm -rf lex try5.*

distclean purge: clean
	rm -rf STD.pmc*

test: all
	./tryfile STD.pm >STD.out
	./teststd
testt: all
	./tryfile STD.pm >STD.out
	./teststd ../../t
