# Makefile for STD.pm6 viv etcetera in pugs/src/perl6
.PHONY: six all sixfast clean stage0 stage1 stage2 stage3 snap snaptest

# technically viv is part of the frontend too, but it's used very little.  viv
# should probably be refactored into independant programs
FRONTEND=Actions.pm CORE.setting CursorBase.pmc DEBUG.pmc LazyMap.pm NAME.pmc\
	 NULL.lex RE_ast.pmc Stash.pmc mangle.pl std uniprops
BACKEND=mangle.pl Actions.pm viv
INVARIANT=$(FRONTEND) viv
GENERATE=STD.pmc Cursor.pmc
STD_SOURCE=STD.pm6 Cursor.pm6 CursorBase.pm6 lib/Stash.pm6 lib/NAME.pm6\
       lib/DEBUG.pm6
CURSOR_SOURCE=Cursor.pm6 CursorBase.pm6

six: .stamp
all: .stamp .stamp5
.stamp: stage2/.stamp
	rm -rf syml
	cp -a stage2/syml stage2/STD.pmc stage2/Cursor.pmc .
	touch .stamp
.stamp5: stage2/STD_P5.pmc
	cp stage2/STD_P5.pmc .
	touch .stamp5

clean:
	rm -rf syml STD_P5.pmc $(GENERATE) stage0/syml stage1/*\
	    stage2/* stage3/* stage*/.stamp .stamp .stamp5

stage0: stage0/.stamp
stage1: stage1/.stamp
stage2: stage2/.stamp
stage3: stage3/.stamp

########################################
# stage0 is rather weird, in that it has its own copy of the invariant files
# */.stamp indicates that the corresponding compiler is "usable"
stage0/.stamp: $(addprefix stage0/,$(INVARIANT) $(GENERATE))
	rm -rf stage0/syml
	cd stage0 && ./std CORE.setting
	touch stage0/.stamp

########################################
# stage 1 is built from the working STD.pm6 using a bootstrap compiler
# it can be built very quickly, usually.  Quirk: stage1/.stamp isn't always
# updated when viv is, to avoid needless reparsing; check it yourself!
stage1/STD.store: $(STD_SOURCE) stage0/.stamp
	cd stage0 && PERL6LIB=../lib:.. ./viv -o ../stage1/STD.store \
	    --freeze ../STD.pm6
stage1/Cursor.store: $(CURSOR_SOURCE) stage0/.stamp
	cd stage0 && PERL6LIB=../lib:.. ./viv -o ../stage1/Cursor.store \
	    --freeze ../Cursor.pm6
stage1/STD.pmc: stage1/STD.store stage0/.stamp
	cd stage0 && PERL6LIB=../lib:.. ./viv -5 --no-indent \
	    -o ../stage1/STD.pmc --thaw ../stage1/STD.store
stage1/Cursor.pmc: stage1/Cursor.store stage0/.stamp
	cd stage0 && PERL6LIB=../lib:.. ./viv -5 --no-indent \
	    -o ../stage1/Cursor.pmc --thaw ../stage1/Cursor.store
stage1/.stamp: stage1/STD.pmc stage1/Cursor.pmc $(FRONTEND)
	rm -rf stage1/syml
	STD5PREFIX=stage1/ PERL5LIB=stage1/:. ./std CORE.setting
	touch stage1/.stamp

########################################
# stage 2 is built using the working STD.pm6, and serves to expose miscompiling
# bugs in STD
stage2/STD.store: $(STD_SOURCE) stage1/.stamp
	STD5PREFIX=stage1/ PERL5LIB=stage1/:. ./viv \
		   -o stage2/STD.store --freeze STD.pm6
stage2/Cursor.store: $(CURSOR_SOURCE) stage1/.stamp
	STD5PREFIX=stage1/ PERL5LIB=stage1/:. ./viv \
		   -o stage2/Cursor.store --freeze Cursor.pm6
stage2/STD.pmc: stage2/STD.store stage1/.stamp $(BACKEND)
	STD5PREFIX=stage1/ PERL5LIB=stage1/:. ./viv -5 --no-indent \
		   -o stage2/STD.pmc --thaw stage2/STD.store
stage2/Cursor.pmc: stage2/Cursor.store stage1/.stamp $(BACKEND)
	STD5PREFIX=stage1/ PERL5LIB=stage1/:. ./viv -5 --no-indent \
		   -o stage2/Cursor.pmc --thaw stage2/Cursor.store
stage2/.stamp: stage2/STD.pmc stage2/Cursor.pmc $(INVARIANT)
	rm -rf stage2/syml
	STD5PREFIX=stage2/ PERL5LIB=stage2/:. ./std CORE.setting
	touch stage2/.stamp
# not part of the normal stage2, but built as if it were
stage2/STD_P5.store: $(STD_P5_SOURCE) stage1/.stamp
	STD5PREFIX=stage1/ PERL5LIB=stage1/:. ./viv \
		   -o stage2/STD_P5.store --freeze STD_P5.pm6
stage2/STD_P5.pmc: stage2/STD_P5.store stage1/.stamp $(BACKEND)
	STD5PREFIX=stage1/ PERL5LIB=stage1/:. ./viv -5 --no-indent \
		   -o stage2/STD_P5.pmc --thaw stage2/STD_P5.store

########################################
# if the compiler is working correctly, stage3 will be the same as stage2
stage3/STD.store: $(STD_SOURCE) stage2/.stamp
	STD5PREFIX=stage2/ PERL5LIB=stage2/:. ./viv \
		   -o stage3/STD.store --freeze STD.pm6
stage3/Cursor.store: $(CURSOR_SOURCE) stage2/.stamp
	STD5PREFIX=stage2/ PERL5LIB=stage2/:. ./viv \
		   -o stage3/Cursor.store --freeze Cursor.pm6
stage3/STD.pmc: stage3/STD.store stage2/.stamp
	STD5PREFIX=stage2/ PERL5LIB=stage2/:. ./viv -5 --no-indent \
		   -o stage3/STD.pmc --thaw stage3/STD.store
stage3/Cursor.pmc: stage3/Cursor.store stage2/.stamp
	STD5PREFIX=stage2/ PERL5LIB=stage2/:. ./viv -5 --no-indent \
		   -o stage3/Cursor.pmc --thaw stage3/Cursor.store
stage3/.stamp: stage3/STD.pmc stage3/Cursor.pmc $(INVARIANT)
	cmp stage2/STD.pmc stage3/STD.pmc
	cmp stage2/Cursor.pmc stage3/Cursor.pmc
	rm -rf stage3/syml
	cp -a stage2/syml stage3
	touch stage3/.stamp

########################################

reboot: stage3/.stamp
	cp -a $(INVARIANT) stage0
	cp -a $(addprefix stage3/,$(GENERATE)) stage0
	rm -rf stage0/syml

snap: stage3/.stamp .stamp5
	rm -rf snap.new
	mkdir snap.new
	svn info |perl -ne 'print "$$1\n" if /Revision:\s+(\d+)/' > snap.new/revision
	cp -r $(INVARIANT) $(addprefix stage3/,$(GENERATE) syml) stage2/STD_P5.pmc lib tryfile teststd snap.new
	-rm -rf snap.old
	-mv snap snap.old
	mv snap.new snap

snaptest: snap
	cd snap && ./teststd $(realpath ../../t/spec)

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
	@echo 'stage0          prepares bootstrap for usage'
	@echo 'stage1          prepares stage 1 compiler (fast)'
	@echo 'stage2          prepares stage 2 compiler'
	@echo 'stage3          prepares stage 3 compiler (only for testing)'
	@echo 'help            show this list'
	@echo 'snaptest        run snapshot teststd on pugs/t/*'
	@echo
