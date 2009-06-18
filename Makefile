.PHONY: check try cat clean distclean purge test

FIXINS=Cursor.pmc LazyMap.pm mangle.pl CORE.pad

all: $(FIXINS) check lex/STD/termish

fast: $(FIXINS) check

snap: $(FIXINS) check lex/STD/termish
	mkdir snap.new
	cp $(FIXINS) tryfile STD.pmc snap.new
	mv lex snap.new
	test -d snap && mv snap snap.old
	mv snap.new snap

STD.pmc: STD.pm gimme5
	./gimme5 $< >STD.pm5
	perl -p -e 'next if /^---/../\A\w+\Z/;' -e 's/\A[ \t]+//;' STD.pm5 >$@
	rm -rf lex *.pad.store

check: STD.pmc
	/usr/local/bin/perl -c $<

# pre-generate common sublexers
lex/STD/termish: STD.pmc
	@echo 'Generating STD lexers...'
	./tryfile STD.pm

cat:
	cat try5.out

clean:
	rm -rf lex try5.* *.pad.store

distclean purge: clean
	rm -rf STD.pmc STD.pm5

test: all
	./teststd
testt: all
	./teststd ../../t
