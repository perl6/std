# Makefile for sprixel

# 
T_01_SANITY = ../../t/01-sanity/04-if.t

# usage: make -f sprixel.mk test
test:
	prove -e 'perl sprixel.pl' $(T_01_SANITY)
	@echo Q.E.D.

spectest:
	perl ~/rakudo/t/harness prove -e 'perl vijs' vivjs/t/
	@echo Q.E.D.

