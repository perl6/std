# Makefile for STD.pm6 viv etcetera in pugs/src/perl6
.PHONY: check try cat clean distclean purge test

FIXINS=Cursor.pmc CursorBase.pmc RE_ast.pmc LazyMap.pm Actions.pm lib/DEBUG.pm6 DEBUG.pmc lib/Test.pm6 \
	CORE.setting NULL.pad std mangle.pl CORE.pad lib/NAME.pm6 NAME.pmc \
	lib/Stash.pm6 Stash.pmc sprixel.pl ToJS.pm viv sprixelCORE.setting

all: $(FIXINS) check lex/STD/termish

fast: $(FIXINS) check

snap: $(FIXINS) check lex/STD/termish
	rm -rf snap.new
	mkdir snap.new
	@#svn info |grep ^Revision|cut -d' ' -f2  > snap.new/revision
	svn info |perl -ne 'print "$$1\n" if /Revision:\s+(\d+)/' > snap.new/revision
	cp -r $(FIXINS) tryfile STD.pmc STD_P5.pmc syml snap.new
	-cp -r lib snap.new
	-cp -r setting snap.new
	-cp -r sprixel snap.new
	-cp -r lex snap.new
	-rm -rf snap.old
	-mv snap snap.old
	mv snap.new snap

boot/syml/CORE.syml: CORE.setting
	STD5PREFIX=boot/ PERL5LIB=boot perl std

# .store files depend little on viv; ideally it should be factored into two
# programs; this rule is missing a few dependencies!
# boot/STD.pm because boot/STD.pmc doesn't take precedence over ./STD.pm
STD.store: STD.pm6 boot/STD.pm boot/Cursor.pmc Actions.pm boot/syml/CORE.syml
	STD5PREFIX=boot/ PERL5LIB=boot perl viv -o STD.store --freeze STD.pm6
STD.pmc: STD.store viv
	perl -Iboot viv --no-indent -5 -o STD.pmc --thaw STD.store
	rm -rf lex syml/*.pad.store
Cursor.store: Cursor.pm6 boot/STD.pm boot/Cursor.pmc Actions.pm boot/syml/CORE.syml
	STD5PREFIX=boot/ PERL5LIB=boot perl viv -o Cursor.store --freeze Cursor.pm6
Cursor.pmc: Cursor.store viv
	perl -Iboot viv --no-indent -5 -o Cursor.pmc --thaw Cursor.store
# for debugging
STD.pm5: STD.store viv
	perl -Iboot viv -5 -o STD.pm5 --thaw STD.store
STD_P5.store: STD_P5.pm6 boot/STD.pm boot/Cursor.pmc Actions.pm boot/syml/CORE.syml
	STD5PREFIX=boot/ PERL5LIB=boot perl viv -o STD_P5.store --freeze STD_P5.pm6
STD_P5.pmc: STD_P5.store viv
	perl -Iboot viv --no-indent -5 -o STD_P5.pmc --thaw STD_P5.store
	rm -rf lex syml/*.pad.store

syml/CORE.syml: CORE.setting
	-rm -f syml/CORE.syml.store
	-./std CORE.setting

check: STD.pmc STD_P5.pmc Cursor.pmc
	/usr/local/bin/perl -c STD.pmc
	/usr/local/bin/perl -c Cursor.pmc
	/usr/local/bin/perl -c STD_P5.pmc

reboot: STD.pmc Cursor.pmc
	cp STD.pmc boot/STD.pm
	cp Cursor.pmc boot/Cursor.pmc

# pre-generate common sublexers
lex/STD/termish: Cursor.pmc STD.pmc STD_P5.pmc syml/CORE.syml
	@echo 'Generating STD lexers...'
	./tryfile STD.pm6

cat:
	cat try5.out

clean:
	rm -rf lex try5.* *.pad.store syml boot/lex boot/syml STD.pmc STD_P5.pmc STD.store STD_P5.store Cursor.store Cursor.pmc STD.pm5

# purge is an alias for distclean
distclean purge: clean
	rm -rf STD.pmc STD_P5.pmc

snaptest: snap all
	cd snap; ../teststd ../../../t

test: all
	./teststd
testt: all
	./teststd ../../t

# List all targets with brief descriptions.
# Gradual shaving of targets with Occam's Razor would be a Good Thing.
help:
	@echo
	@echo 'In pugs/src/perl6 you can make these targets:'
	@echo
	@echo 'all (default)   builds viv'
	@echo 'fast            same as check'
	@echo 'snap            copies runnable files and svn revision to snap/'
	@echo 'STD_P5.pmc      (internal)'
	@echo 'STD.pmc         (internal)'
	@echo 'syml/CORE.syml  (internal)'
	@echo 'check           validates STD.pmc and STD_P5.pmc in Perl 5'
	@echo 'lex/STD/termish (internal)'
	@echo 'cat             shows the output written by the last tryfile'
	@echo 'clean           removes many generated files'
	@echo 'distclean       removes even more generated files'
	@echo 'purge           alias for distclean'
	@echo 'snaptest        run snapshot teststd on pugs/t/spec/*'
	@echo 'test            run teststd on pugs/t/*'
	@echo 'testt           run teststd on pugs/t/spec/*'
	@echo 'help            show this list'
	@echo
