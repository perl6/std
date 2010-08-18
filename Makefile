# Makefile for STD.pm6 viv etcetera in pugs/src/perl6
.PHONY: six all sixfast clean dist snap snaptest

# technically viv is part of the frontend too, but it's used very little.  viv
# should probably be refactored into independant programs
FRONTEND=Actions.pm CORE.setting CursorBase.pmc DEBUG.pmc LazyMap.pm NAME.pmc\
	 NULL.lex RE_ast.pmc Stash.pmc mangle.pl std uniprops
BACKEND=mangle.pl Actions.pm viv
DISTSRC=Actions.pm CursorBase.pmc DEBUG.pmc LazyMap.pm NAME.pmc RE_ast.pmc\
	Stash.pmc mangle.pl
DISTSRC6=uniprops
DISTGEN=$(GENERATE)
DISTSYML=syml/NULL.lex.store syml/CORE.syml
INVARIANT=$(FRONTEND) viv
GENERATE=STD.pmc Cursor.pmc
STD_SOURCE=STD.pm6 Cursor.pm6 CursorBase.pm6 lib/Stash.pm6 lib/NAME.pm6\
       lib/DEBUG.pm6
CURSOR_SOURCE=Cursor.pm6 CursorBase.pm6
# suppress environmental PERL6LIB while bootstrapping
PERL6LIB=./lib:.
export PERL6LIB

six: .stamp
all: .stamp STD_P5.pmc

clean:
	rm -rf syml STD_P5.pmc $(GENERATE) boot/syml boot/.stamp .stamp

stage0: stage0/.stamp
stage1: stage1/.stamp
stage2: stage2/.stamp

########################################
# */.stamp indicates that the corresponding compiler is "usable"
boot/.stamp: $(INVARIANT) $(addprefix boot/,$(GENERATE))
	rm -rf boot/syml
	BOOT=1 ./std CORE.setting
	touch boot/.stamp

STD.pmc: $(STD_SOURCE) boot/.stamp $(FRONTEND) $(BACKEND)
	BOOT=1 ./viv -5 -o STD.pm5 STD.pm6
	perl -pe '/---/../RETREE_END/ && s/^\s*//' < STD.pm5 > STD.pmc
STD_P5.pmc: STD_P5.pm6 boot/.stamp $(FRONTEND) $(BACKEND)
	BOOT=1 ./viv -5 -o STD_P5.pm5 STD_P5.pm6
	perl -pe '/---/../RETREE_END/ && s/^\s*//' < STD_P5.pm5 > STD_P5.pmc
Cursor.pmc: $(CURSOR_SOURCE) boot/.stamp $(FRONTEND) $(BACKEND)
	BOOT=1 ./viv -5 -o Cursor.pm5 Cursor.pm6
	perl -pe '/---/../RETREE_END/ && s/^\s*//' < Cursor.pm5 > Cursor.pmc
.stamp: STD.pmc Cursor.pmc $(FRONTEND)
	rm -rf syml
	./std CORE.setting
	touch .stamp

reboot: .stamp
	cp -pR $(GENERATE) boot
	rm -rf boot/syml

snap: all
	rm -rf snap.new
	mkdir snap.new
	svn info |perl -ne 'print "$$1\n" if /Revision:\s+(\d+)/' > snap.new/revision
	cp -r $(INVARIANT) $(addprefix stage2/,$(GENERATE) syml) STD_P5.pmc lib tryfile teststd snap.new
	-rm -rf snap.old
	-mv snap snap.old
	mv snap.new snap

snaptest: snap
	cd snap && ./teststd $(realpath ../../t/spec)

dist: stage2/.stamp .stamp5
	rm -rf dist
	mkdir dist dist/lib dist/lib6 dist/syml
	cp $(addprefix stage2/,$(DISTGEN)) $(DISTSRC) STD_P5.pmc dist/lib
	cp $(DISTSRC6) dist/lib6
	cp $(DISTSYML) dist/syml

#List all targets with brief descriptions.
# Gradual shaving of targets with Occam's Razor would be a Good Thing.
help:
	@echo
	@echo 'In pugs/src/perl6 you can make these targets:'
	@echo
	@echo 'six (default)   builds viv for Perl6'
	@echo 'all             builds viv for Perl5 too'
	@echo 'reboot          builds and updates boot; test first!'
	@echo 'clean           removes generated files'
	@echo 'dist            make a minimal distribution set'
	@echo 'help            show this list'
	@echo 'snaptest        run snapshot teststd on pugs/t/*'
	@echo
