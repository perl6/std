# Makefile for sprixel

# list of simple tests that sprixel passes
T_01_SANITY = ../../t/01-sanity/04-if.t
TESTS = $(T_01_SANITY)

# usage: make -f sprixel.mk test
test:
	prove -e 'perl sprixel.pl' $(TESTS)
	@echo Q.E.D.

# usage: make -f sprixel.mk spectest
spectest:
	perl sprixel/harness-fudging.pl 'perl sprixel.pl' sprixel/spectest.data ../../t/spec
	@echo Q.E.D.

