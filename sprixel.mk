# Makefile for sprixel

# list of simple tests that sprixel passes
T_01_SANITY = ../../t/01-sanity/04-if.t
TESTS = $(T_01_SANITY)

#  list of spectests that sprixel passes
T_SPEC_S02_BUILTIN_DATA_TYPES = ../../t/spec/S02-builtin_data_types/num.t
SPECTESTS = $(T_SPEC_S02_BUILTIN_DATA_TYPES)

# usage: make -f sprixel.mk test
test:
	prove -e 'perl sprixel.pl' $(TESTS)
	@echo Q.E.D.

# usage: make -f sprixel.mk spectest
spectest:
	prove -e 'perl sprixel.pl' $(SPECTESTS)
	@echo Q.E.D.

