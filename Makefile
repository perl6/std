# Makefile for STD.pm6 viv etcetera in pugs/src/perl6
.PHONY: six all sixfast clean snap snaptest

INVARIANT=Actions.pm CORE.setting CursorBase.pmc DEBUG.pmc LazyMap.pm NAME.pmc\
	  RE_ast.pmc Stash.pmc mangle.pl uniprops viv
GENERATE=STD.pmc Cursor.pmc
BOOTFILES=boot/STD.pmc boot/Cursor.pmc

STD_SOURCE=STD.pm6 Cursor.pm6 CursorBase.pm6 lib/Stash.pm6 lib/NAME.pm6\
           lib/DEBUG.pm6
CURSOR_SOURCE=Cursor.pm6 CursorBase.pm6

PERL=perl

RM_RF=$(PERL) -MExtUtils::Command -e rm_rf
CP=$(PERL) -MExtUtils::Command -e cp
MV=$(PERL) -MExtUtils::Command -e mv
MKDIR=mkdir
GIT=git
# no snaptest on win32 just yet
CP_R=cp -r

all: syml/CORE.syml STD_P5.pmc
six: syml/CORE.syml

clean:
	$(RM_RF) syml STD_P5.pmc $(GENERATE) boot/syml boot/.stamp .stamp\
	    STD_P5.pm5 STD.pm5 Cursor.pm5 snap.old snap.new

########################################
# */syml/CORE.syml indicates that the corresponding compiler is "usable"
boot/syml/CORE.syml: $(INVARIANT) $(BOOTFILES)
	$(RM_RF) boot/syml
	$(PERL) ./viv --boot --noperl6lib --compile-setting CORE.setting

STD.pmc: $(STD_SOURCE) boot/syml/CORE.syml $(INVARIANT)
	$(PERL) ./viv --boot --noperl6lib -5 -o STD.pm5 STD.pm6
	$(PERL) tools/compact_pmc < STD.pm5 > STD.pmc
STD_P5.pmc: STD_P5.pm6 boot/syml/CORE.syml $(INVARIANT)
	$(PERL) ./viv --boot --noperl6lib -5 -o STD_P5.pm5 STD_P5.pm6
	$(PERL) tools/compact_pmc < STD_P5.pm5 > STD_P5.pmc
Cursor.pmc: $(CURSOR_SOURCE) boot/syml/CORE.syml $(INVARIANT)
	$(PERL) ./viv --boot --noperl6lib -5 -o Cursor.pm5 Cursor.pm6
	$(PERL) tools/compact_pmc < Cursor.pm5 > Cursor.pmc
syml/CORE.syml: STD.pmc Cursor.pmc $(INVARIANT)
	$(RM_RF) syml
	$(PERL) ./viv --noperl6lib --compile-setting CORE.setting
	$(CP) boot/syml/CursorBase.syml boot/syml/Cursor.syml boot/syml/DEBUG.syml boot/syml/NAME.syml boot/syml/Stash.syml boot/syml/STD.syml syml
# reboot after incompatibly changing syml format

reboot: six
	$(CP) $(GENERATE) boot
	$(RM_RF) boot/syml

snap: all
	$(RM_RF) snap.new
	$(MKDIR) snap.new
	$(GIT) log -1 --pretty="format:%h" > snap.new/revision
	$(CP_R) $(INVARIANT) $(GENERATE) syml STD_P5.pmc lib tools/tryfile tools/teststd snap.new
	-$(RM_RF) snap.old
	-$(MV) snap snap.old
	$(MV) snap.new snap

snaptest: snap
	cd snap && $(PERL) teststd $(realpath ../roast)

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
