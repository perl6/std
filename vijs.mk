# Makefile for vivjs

test:
	prove -e 'perl vijs' vivjs/t/
	@echo Q.E.D.

spectest:
	perl ~/rakudo/t/harness prove -e 'perl vijs' vivjs/t/
	@echo Q.E.D.

