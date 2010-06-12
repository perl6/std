# Makefile for STD.pm6 viv etcetera in pugs/src/perl6
.PHONY: six all sixfast

# technically viv is part of the frontend too, but it's used very little.  viv
# should probably be refactored into independant programs
FRONTEND=Actions.pm CORE.setting CursorBase.pmc DEBUG.pmc LazyMap.pm NAME.pmc\
	 NULL.pad RE_ast.pmc Stash.pmc mangle.pl std
BACKEND=mangle.pl Actions.pm viv
INVARIANT=$(FRONTEND) viv
GENERATE=STD.pmc Cursor.pmc
STD_SOURCE=STD.pm6 Cursor.pm6 CursorBase.pm6 lib/Stash.pm6 lib/NAME.pm6\
       lib/DEBUG.pm6
CURSOR_SOURCE=Cursor.pm6 CursorBase.pm6

six: .stamp
all: .stamp .stamp5
.stamp: stage2/.stamp
	rm -rf lex syml
	cp -a stage2/lex stage2/syml stage2/STD.pmc stage2/Cursor.pmc .
	touch .stamp
.stamp5: stage2/STD_P5.pmc
	rm -rf lex/STD/P5
	cp stage2/STD_P5.pmc .
	touch .stamp5

# stage0 is rather weird, in that it has its own copy of the invariant files
# */.stamp indicates that the corresponding compiler is "usable"
stage0/.stamp: $(addprefix stage0/,$(INVARIANT) $(GENERATE))
	rm -rf stage0/lex stage0/syml
	cd stage0 && ./std CORE.setting
	touch stage0/.stamp

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
stage1/.stamp: stage1/STD.pmc stage1/Cursor.pmc $(INVARIANT)
	STD5PREFIX=stage1/ PERL5LIB=stage1/:. ./std CORE.setting
	touch stage1/.stamp

stage2/STD.store: $(STD_SOURCE) stage1/.stamp
	STD5PREFIX=stage1/ PERL5LIB=stage1/:. ./viv \
		   -o stage2/STD.store --freeze STD.pm6
stage2/Cursor.store: $(CURSOR_SOURCE) stage1/.stamp
	STD5PREFIX=stage1/ PERL5LIB=stage1/:. ./viv \
		   -o stage2/Cursor.store --freeze Cursor.pm6
stage2/STD.pmc: stage2/STD.store stage1/.stamp
	STD5PREFIX=stage1/ PERL5LIB=stage1/:. ./viv -5 --no-indent \
		   -o stage2/STD.pmc --thaw stage2/STD.store
stage2/Cursor.pmc: stage2/Cursor.store stage1/.stamp
	STD5PREFIX=stage1/ PERL5LIB=stage1/:. ./viv -5 --no-indent \
		   -o stage2/Cursor.pmc --thaw stage2/Cursor.store
stage2/.stamp: stage2/STD.pmc stage2/Cursor.pmc $(INVARIANT)
	STD5PREFIX=stage2/ PERL5LIB=stage2/:. ./std CORE.setting
	touch stage2/.stamp
# not part of the normal stage2, but built as if it were
stage2/STD_P5.store: $(STD_P5_SOURCE) stage1/.stamp
	STD5PREFIX=stage1/ PERL5LIB=stage1/:. ./viv \
		   -o stage2/STD_P5.store --freeze STD_P5.pm6
stage2/STD_P5.pmc: stage2/STD_P5.store stage1/.stamp
	STD5PREFIX=stage1/ PERL5LIB=stage1/:. ./viv -5 --no-indent \
		   -o stage2/STD_P5.pmc --thaw stage2/STD_P5.store


slow: stage2
	cp stage2/STD.pmc stage2/Cursor.pmc .

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


#STD.pm5: STD.store viv
#STD_P5.store: STD_P5.pm6 boot/STD.pm boot/Cursor.pmc Actions.pm boot/syml/CORE.syml
#STD_P5.pmc: STD_P5.store viv

#reboot: STD.pmc Cursor.pmc

#clean distclean:

#snap:
#snaptest: snap all
#test: all
#testt: all

# List all targets with brief descriptions.
# Gradual shaving of targets with Occam's Razor would be a Good Thing.
help:
	@echo
	@echo 'In pugs/src/perl6 you can make these targets:'
	@echo
	@echo 'six (default)   builds viv for Perl6'
	@echo 'all             builds viv for Perl5 too'
	@echo 'snap            copies runnable files and svn revision to snap/'
	@echo 'clean           removes many generated files'
	@echo 'snaptest        run snapshot teststd on pugs/t/spec/*'
	@echo 'test            run teststd on pugs/t/*'
	@echo 'testt           run teststd on pugs/t/spec/*'
	@echo 'help            show this list'
	@echo
