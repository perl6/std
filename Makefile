.PHONY: check try cat clean distclean purge test

FIXINS=Cursor.pmc LazyMap.pm DEBUG.pm DEBUG.pmc Test.pm CORE.setting NULL.pad std mangle.pl CORE.pad NAME.pm NAME.pmc STASH.pm STASH.pmc

all: $(FIXINS) check lex/STD/termish

fast: $(FIXINS) check

snap: $(FIXINS) check lex/STD/termish
	rm -rf snap.new
	mkdir snap.new
	svn info |grep ^Revision|cut -d' ' -f2  > snap.new/revision
	cp $(FIXINS)  tryfile STD.pmc *.syml CORE.*.store snap.new
	cd snap.new; ln -s ../vivjs .; cd ..
	-mv lex snap.new
	-rm -rf snap.old
	-mv snap snap.old
	mv snap.new snap

STD.pmc: STD.pm gimme5
	./gimme5 $< >STD.pm5
	perl -p -e 'next if /^---/../\A\w+\Z/;' -e 's/\A[ \t]+//;' STD.pm5 >$@
	rm -rf lex *.pad.store

CORE.syml: CORE.setting
	-rm CORE.syml.store
	-./std CORE.setting

check: STD.pmc
	/usr/local/bin/perl -c $<

# pre-generate common sublexers
lex/STD/termish: STD.pmc CORE.syml
	@echo 'Generating STD lexers...'
	./tryfile STD.pm

cat:
	cat try5.out

clean:
	rm -rf lex try5.* *.pad.store *.syml.store *.syml STD.pmc STD.pm5

distclean purge: clean
	rm -rf STD.pmc STD.pm5

snaptest: snap all
	cd snap; ../teststd ../../../t

test: all
	./teststd
testt: all
	./teststd ../../t
