.PHONY: check try cat clean distclean purge test

FIXINS=Cursor.pmc LazyMap.pm mangle.pl

all: $(FIXINS) check lex/STD/termish

fast: $(FIXINS) check

STD.pmc: STD.pm gimme5
	./gimme5 $< >STD.pm5
	perl -p -e 'next if /^---/../\A\w+\Z/;' -e 's/\A[ \t]+//;' STD.pm5 >$@
	rm -rf lex

check: STD.pmc
	/usr/local/bin/perl -c $<

# pre-generate common sublexers
lex/STD/termish: STD.pmc
	@echo 'Generating STD lexers...'
	./tryfile STD.pm

cat:
	cat try5.out

clean:
	rm -rf lex try5.*

distclean purge: clean
	rm -rf STD.pmc*

test: all
	./teststd
testt: all
	./teststd ../../t
