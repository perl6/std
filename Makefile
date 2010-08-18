# Makefile for STD.pm6 viv etcetera in pugs/src/perl6
.PHONY: six all sixfast clean snap snaptest

INVARIANT=Actions.pm CORE.setting CursorBase.pmc DEBUG.pmc LazyMap.pm NAME.pmc\
	  RE_ast.pmc Stash.pmc mangle.pl std uniprops viv
GENERATE=STD.pmc Cursor.pmc
BOOTFILES=boot/STD.pmc boot/Cursor.pmc

STD_SOURCE=STD.pm6 Cursor.pm6 CursorBase.pm6 lib/Stash.pm6 lib/NAME.pm6\
           lib/DEBUG.pm6
CURSOR_SOURCE=Cursor.pm6 CursorBase.pm6

STDINC=--clear-inc --inc lib --inc .

PERL=perl

RM_RF=$(PERL) -MExtUtils::Command -e rm_rf
CP=$(PERL) -MExtUtils::Command -e cp
MV=$(PERL) -MExtUtils::Command -e mv
TOUCH=$(PERL) -MExtUtils::Command -e touch
MKDIR=mkdir
SVN=svn
# no snaptest on win32 just yet
CP_R=cp -r

six: .stamp
all: .stamp STD_P5.pmc

clean:
	$(RM_RF) syml STD_P5.pmc $(GENERATE) boot/syml boot/.stamp .stamp\
	    STD_P5.pm5 STD.pm5 Cursor.pm5 snap snap.old snap.new

########################################
# */.stamp indicates that the corresponding compiler is "usable"
boot/.stamp: $(INVARIANT) $(BOOTFILES)
	$(RM_RF) boot/syml
	$(PERL) std --boot $(STDINC) CORE.setting
	$(TOUCH) boot/.stamp

STD.pmc: $(STD_SOURCE) boot/.stamp $(INVARIANT)
	$(PERL) viv --boot $(STDINC) -5 -o STD.pm5 STD.pm6
	$(PERL) -pe '(/^---/../^RETREE_END/) || s/^\s*//' < STD.pm5 > STD.pmc
STD_P5.pmc: STD_P5.pm6 boot/.stamp $(INVARIANT)
	$(PERL) viv --boot $(STDINC) -5 -o STD_P5.pm5 STD_P5.pm6
	$(PERL) -pe '(/^---/../^RETREE_END/) || s/^\s*//' < STD_P5.pm5 > STD_P5.pmc
Cursor.pmc: $(CURSOR_SOURCE) boot/.stamp $(INVARIANT)
	$(PERL) viv --boot $(STDINC) -5 -o Cursor.pm5 Cursor.pm6
	$(PERL) -pe '(/^---/../^RETREE_END/) || s/^\s*//' < Cursor.pm5 > Cursor.pmc
.stamp: STD.pmc Cursor.pmc $(INVARIANT)
	$(RM_RF) syml
	$(PERL) std $(STDINC) CORE.setting
	$(TOUCH) .stamp

reboot: .stamp
	$(CP) $(GENERATE) boot
	$(RM_RF) boot/syml

snap: all
	$(RM_RF) snap.new
	$(MKDIR) snap.new
	$(SVN) info | $(PERL) -ne 'print "$$1\n" if /Revision:\s+(\d+)/' > snap.new/revision
	$(CP_R) $(INVARIANT) $(GENERATE) syml STD_P5.pmc lib tools/tryfile tools/teststd snap.new
	-$(RM_RF) snap.old
	-$(MV) snap snap.old
	$(MV) snap.new snap

snaptest: snap
	cd snap && $(PERL) teststd $(realpath ../../t/spec)

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
	@echo 'help            show this list'
	@echo 'snaptest        run snapshot teststd on pugs/t/*'
	@echo
