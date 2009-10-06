# Makefile for sprixel

# list of simple tests that sprixel passes
T_01_SANITY = ../../t/01-sanity/03-equal.t ../../t/01-sanity/04-if.t \
 ../../t/01-sanity/05-sub.t
TESTS = $(T_01_SANITY)

# usage: make -f sprixel.mk test
test:
	prove -e 'perl sprixel.pl' $(TESTS)
	@echo Q.E.D.

# usage: make -f sprixel.mk spectest
spectest:
	perl sprixel/harness-fudging.pl --execute='perl sprixel.pl'\
       --spectest_data=sprixel/spectest.data --spectest_base=../../t/spec
	@echo Q.E.D.

