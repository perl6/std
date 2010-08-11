# Makefile for STD.pm6 viv etcetera in pugs/src/perl6
.PHONY: six all sixfast clean stage0 stage1 stage2 stage3 snap snaptest

# technically viv is part of the frontend too, but it's used very little.  viv
# should probably be refactored into independant programs
FRONTEND=Actions.pm CORE.setting CursorBase.pmc DEBUG.pmc LazyMap.pm NAME.pmc\
	 NULL.lex RE_ast.pmc Stash.pmc mangle.pl std uniprops
BACKEND=mangle.pl Actions.pm viv
DISTSRC=Actions.pm CursorBase.pmc DEBUG.pmc LazyMap.pm NAME.pmc RE_ast.pmc\
	Stash.pmc mangle.pl
DISTSRC6=uniprops
DISTGEN=$(GENERATE) STD_P5.pmc
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
all: .stamp .stamp5
.stamp: stage1/.stamp
	rm -rf syml
	cp -pR stage0/syml stage1/STD.pmc stage1/Cursor.pmc .
	touch .stamp
.stamp5: .stamp
	./viv -5 --no-indent -o STD_P5.pmc STD_P5.pm6
	touch .stamp5

clean:
	rm -rf syml STD_P5.pmc $(GENERATE) stage0/syml stage1/*\
	    stage2/* stage3/* stage*/.stamp .stamp .stamp5

stage0: stage0/.stamp
stage1: stage1/.stamp
stage2: stage2/.stamp

########################################
# */.stamp indicates that the corresponding compiler is "usable"
stage0/.stamp: $(INVARIANT) $(addprefix stage0/,$(GENERATE))
	rm -rf stage0/syml
	STAGE=0 ./std CORE.setting
	touch stage0/.stamp

stage1/STD.store: $(STD_SOURCE) stage0/.stamp $(FRONTEND)
	STAGE=0 ./viv -o stage1/STD.store --freeze STD.pm6
stage1/Cursor.store: $(CURSOR_SOURCE) stage0/.stamp $(FRONTEND)
	STAGE=0 ./viv -o stage1/Cursor.store --freeze Cursor.pm6
stage1/STD.pmc: stage1/STD.store stage0/.stamp $(BACKEND)
	STAGE=0 ./viv -5 --no-indent -o stage1/STD.pmc --thaw stage1/STD.store
stage1/Cursor.pmc: stage1/Cursor.store stage0/.stamp $(BACKEND)
	STAGE=0 ./viv -5 --no-indent -o stage1/Cursor.pmc \
	      --thaw stage1/Cursor.store
stage1/.stamp: stage1/STD.pmc stage1/Cursor.pmc $(FRONTEND)
	rm -rf stage1/syml
	STAGE=1 ./std CORE.setting
	touch stage1/.stamp

########################################
# if the compiler is working correctly, stage2 will be the same as stage1
stage2/STD.store: $(STD_SOURCE) stage1/.stamp $(FRONTEND)
	STAGE=1 ./viv -o stage2/STD.store --freeze STD.pm6
stage2/Cursor.store: $(CURSOR_SOURCE) stage1/.stamp $(FRONTEND)
	STAGE=1 ./viv -o stage2/Cursor.store --freeze Cursor.pm6
stage2/STD.pmc: stage2/STD.store stage1/.stamp $(BACKEND)
	STAGE=1 ./viv -5 --no-indent -o stage2/STD.pmc --thaw stage2/STD.store
stage2/Cursor.pmc: stage2/Cursor.store stage1/.stamp $(BACKEND)
	STAGE=1 ./viv -5 --no-indent \
	    -o stage2/Cursor.pmc --thaw stage2/Cursor.store
stage2/.stamp: stage2/STD.pmc stage2/Cursor.pmc
	cmp stage1/STD.pmc stage2/STD.pmc
	cmp stage1/Cursor.pmc stage2/Cursor.pmc
	rm -rf stage2/syml
	cp -pR stage1/syml stage2
	touch stage2/.stamp

########################################

reboot: stage2/.stamp
	cp -pR $(addprefix stage2/,$(GENERATE)) stage0
	rm -rf stage0/syml

snap: stage2/.stamp .stamp5
	rm -rf snap.new
	mkdir snap.new
	svn info |perl -ne 'print "$$1\n" if /Revision:\s+(\d+)/' > snap.new/revision
	cp -r $(INVARIANT) $(addprefix stage2/,$(GENERATE) syml) stage2/STD_P5.pmc lib tryfile teststd snap.new
	-rm -rf snap.old
	-mv snap snap.old
	mv snap.new snap

snaptest: snap
	cd snap && ./teststd $(realpath ../../t/spec)

dist: stage2 stage2/STD_P5.pmc
	rm -rf dist
	mkdir dist dist/lib dist/lib6 dist/syml
	cp $(addprefix stage2/,$(DISTGEN)) $(DISTSRC) dist/lib
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
	@echo 'reboot          builds third stage and updates stage0'
	@echo 'clean           removes generated files'
	@echo 'dist            make a minimal distribution set'
	@echo 'stage0          prepares bootstrap for usage'
	@echo 'stage1          prepares stage 1 compiler'
	@echo 'stage2          prepares stage 2 compiler (testing)'
	@echo 'help            show this list'
	@echo 'snaptest        run snapshot teststd on pugs/t/*'
	@echo
