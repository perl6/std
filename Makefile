.PHONY: check try cat clean distclean purge test

FIXINS=Cursor.pmc LazyMap.pm mangle.pl

all: $(FIXINS) check try

STD.pmc: STD.pm gimme5
	./gimme5 $< > $@

check: STD.pmc
	perl -c $<

try: try5.out
	@echo "View $< to see output"
	@echo "or run: make try cat"

# pre-generate common sublexers
try5.out: try5 clean
	./try5 comp_unit -e 'say "howdy" ~ "";' > $@

cat:
	cat try5.out	

clean:
	rm -rf lex try5.*

distclean purge: clean
	rm -rf STD.pmc*

test:
	./teststd
