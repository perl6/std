# Makefile for vill - a viv+llvm mashup
# To keep things understandable, use of 'make' variables is avoided
# except for the 'objects' at the top.  It saves so much time not
# having to hunt through dozens of trivial abstractions ;-)

# Object file names sorted alphabetically, each one comes from a
# corresponding .cpp or .c source file and some .h files (listed below)
objects = src/arglist.o src/args.o src/ast_mapentry.o \
	src/ast_seqentry.o src/backslash__S_n.o src/codegen.o \
	src/comp_unit.o src/decint.o src/eat_terminator.o \
	src/escape__S_Back.o src/identifier.o src/infixish.o \
	src/infix__S_Plus.o src/integer.o src/link_codegen.o \
	src/load_ast.o src/main.o src/make_puts.o \
	src/nibbler.o src/noun__S_term.o src/noun__S_value.o \
	src/number__S_numish.o src/numish.o src/optimize.o src/options.o \
	src/quote__S_Double_Double.o src/run.o src/save.o \
	src/statement.o src/statementlist.o src/Str.o \
	src/term__S_identifier.o src/term__S_value.o src/unload_ast.o \
	src/value__S_number.o src/value__S_quote.o src/verify.o \
	src/villCompiler.o \
	src/yaml_compose.o src/yaml_compose_alias.o \
	src/yaml_compose_anchor.o src/yaml_compose_mapping.o \
	src/yaml_compose_root.o src/yaml_compose_scalar.o \
	src/yaml_compose_sequence.o src/yaml_compose_tag.o \
	src/yaml_decompose.o src/yaml_parse.o src/yaml_token.o

# The first target is the default when make is run without arguments.
# Build the main executable by linking all the object files together.
vill: $(objects) src/villCompiler.h src/llvm-version2.6 Makefile
	@echo 'Linking *.o -> ./vill'
	@c++ $(objects) `llvm-config --cxxflags --ldflags --libs bitwriter` \
	  -O3 -o vill
	@find src -name 'llvm-version*' -exec rm {} \;

# Object targets listed alphabetically by filename but not by directory
# separately compile each object file from its corresponding source file
src/arglist.o: src/arglist.cpp src/villCompiler.h Makefile
	c++ -c src/arglist.cpp `llvm-config --cxxflags` -O3 -o src/arglist.o

src/args.o: src/args.cpp src/villCompiler.h Makefile
	c++ -c src/args.cpp `llvm-config --cxxflags` -O3 -o src/args.o

src/ast_mapentry.o: src/ast_mapentry.c src/yaml_compose.h Makefile
	cc -c src/ast_mapentry.c -O3 -o src/ast_mapentry.o

src/ast_seqentry.o: src/ast_seqentry.c src/yaml_parse.h Makefile
	cc -c src/ast_seqentry.c -O3 -o src/ast_seqentry.o

src/backslash__S_n.o: src/backslash__S_n.cpp src/villCompiler.h Makefile
	c++ -c src/backslash__S_n.cpp `llvm-config --cxxflags` -O3 \
		-o src/backslash__S_n.o

src/codegen.o: src/codegen.cpp src/villCompiler.h Makefile
	c++ -c src/codegen.cpp `llvm-config --cxxflags` -O3 -o src/codegen.o

src/comp_unit.o: src/comp_unit.cpp src/villCompiler.h Makefile
	c++ -c src/comp_unit.cpp `llvm-config --cxxflags` -O3 \
		-o src/comp_unit.o

src/decint.o: src/decint.cpp src/villCompiler.h Makefile
	c++ -c src/decint.cpp `llvm-config --cxxflags` -O3 -o src/decint.o

src/eat_terminator.o: src/eat_terminator.cpp src/villCompiler.h Makefile
	c++ -c src/eat_terminator.cpp `llvm-config --cxxflags` -O3 \
		-o src/eat_terminator.o

src/escape__S_Back.o: src/escape__S_Back.cpp src/villCompiler.h Makefile
	c++ -c src/escape__S_Back.cpp `llvm-config --cxxflags` -O3 \
		-o src/escape__S_Back.o

src/integer.o: src/integer.cpp src/villCompiler.h Makefile
	c++ -c src/integer.cpp `llvm-config --cxxflags` -O3 -o src/integer.o

src/identifier.o: src/identifier.cpp src/villCompiler.h Makefile
	c++ -c src/identifier.cpp `llvm-config --cxxflags` -O3 \
		-o src/identifier.o

src/infixish.o: src/infixish.cpp src/villCompiler.h Makefile
	c++ -c src/infixish.cpp `llvm-config --cxxflags` -O3 \
		-o src/infixish.o

src/infix__S_Plus.o: src/infix__S_Plus.cpp src/villCompiler.h Makefile
	c++ -c src/infix__S_Plus.cpp `llvm-config --cxxflags` -O3 \
		-o src/infix__S_Plus.o

src/link_codegen.o: src/link_codegen.cpp src/villCompiler.h \
		src/yaml_compose.h Makefile
	c++ -c src/link_codegen.cpp `llvm-config --cxxflags` -O3 \
		-o src/link_codegen.o

# Cause the make to fail if the LLVM version is not 2.6
# This (or rather the rm in the vill target) has the unfortunate side
# effect of causing every plain 'make' to do a top level recompile.
# A more elegant solution that avoids unnecessary recompiles would be
# very welcome.
src/llvm-version2.6:
	@find src -name 'llvm-version*' -exec rm {} \;
	@export VERSION=`llvm-config --version`; echo $$VERSION > \
	  src/llvm-version$$VERSION
	@test -e src/llvm-version2.6

src/load_ast.o: src/load_ast.cpp src/villCompiler.h src/yaml_compose.h \
		Makefile
	c++ -c src/load_ast.cpp `llvm-config --cxxflags` -O3 \
		-o src/load_ast.o

src/main.o: src/main.cpp src/villCompiler.h Makefile
	c++ -c src/main.cpp `llvm-config --cxxflags` -O3 -o src/main.o

src/make_puts.o: src/make_puts.cpp src/villCompiler.h Makefile
	c++ -c src/make_puts.cpp `llvm-config --cxxflags` -O3 \
		-o src/make_puts.o

src/nibbler.o: src/nibbler.cpp src/villCompiler.h Makefile
	c++ -c src/nibbler.cpp `llvm-config --cxxflags` -O3 -o src/nibbler.o

src/noun__S_term.o: src/noun__S_term.cpp src/villCompiler.h Makefile
	c++ -c src/noun__S_term.cpp `llvm-config --cxxflags` -O3 \
		-o src/noun__S_term.o

src/noun__S_value.o: src/noun__S_value.cpp src/villCompiler.h Makefile
	c++ -c src/noun__S_value.cpp `llvm-config --cxxflags` -O3 \
		-o src/noun__S_value.o

src/number__S_numish.o: src/number__S_numish.cpp src/villCompiler.h \
		Makefile
	c++ -c src/number__S_numish.cpp `llvm-config --cxxflags` -O3 \
		-o src/number__S_numish.o

src/numish.o: src/numish.cpp src/villCompiler.h Makefile
	c++ -c src/numish.cpp `llvm-config --cxxflags` -O3 -o src/numish.o

src/optimize.o: src/optimize.cpp src/villCompiler.h Makefile
	c++ -c src/optimize.cpp `llvm-config --cxxflags` -O3 \
		-o src/optimize.o

src/options.o: src/options.cpp src/villCompiler.h Makefile
	c++ -c src/options.cpp `llvm-config --cxxflags` -O3 -o src/options.o

src/quote__S_Double_Double.o: src/quote__S_Double_Double.cpp \
		src/villCompiler.h Makefile
	c++ -c src/quote__S_Double_Double.cpp `llvm-config --cxxflags` -O3 \
		-o src/quote__S_Double_Double.o

src/run.o: src/run.cpp src/villCompiler.h Makefile
	c++ -c src/run.cpp `llvm-config --cxxflags` -O3 -o src/run.o

src/save.o: src/save.cpp src/villCompiler.h Makefile
	c++ -c src/save.cpp `llvm-config --cxxflags` -O3 -o src/save.o

src/statement.o: src/statement.cpp src/villCompiler.h Makefile
	c++ -c src/statement.cpp `llvm-config --cxxflags` -O3 \
		-o src/statement.o

src/statementlist.o: src/statementlist.cpp src/villCompiler.h Makefile
	c++ -c src/statementlist.cpp `llvm-config --cxxflags` -O3 \
		-o src/statementlist.o

src/Str.o: src/Str.cpp src/villCompiler.h Makefile
	c++ -c src/Str.cpp `llvm-config --cxxflags` -O3 -o src/Str.o

src/term__S_identifier.o: src/term__S_identifier.cpp \
		src/villCompiler.h Makefile
	c++ -c src/term__S_identifier.cpp `llvm-config --cxxflags` -O3 \
		-o src/term__S_identifier.o

src/term__S_value.o: src/term__S_value.cpp src/villCompiler.h Makefile
	c++ -c src/term__S_value.cpp `llvm-config --cxxflags` -O3 \
		-o src/term__S_value.o

src/unload_ast.o: src/unload_ast.cpp src/villCompiler.h src/yaml_compose.h \
		Makefile
	c++ -c src/unload_ast.cpp `llvm-config --cxxflags` -O3 \
		-o src/unload_ast.o

src/value__S_number.o: src/value__S_number.cpp src/villCompiler.h \
		Makefile
	c++ -c src/value__S_number.cpp `llvm-config --cxxflags` -O3 \
		-o src/value__S_number.o

src/value__S_quote.o: src/value__S_quote.cpp src/villCompiler.h Makefile
	c++ -c src/value__S_quote.cpp `llvm-config --cxxflags` -O3 \
		-o src/value__S_quote.o

src/verify.o: src/verify.cpp src/villCompiler.h Makefile
	c++ -c src/verify.cpp `llvm-config --cxxflags` -O3 -o src/verify.o

src/villCompiler.o: src/villCompiler.cpp src/villCompiler.h Makefile
	c++ -c src/villCompiler.cpp `llvm-config --cxxflags` -O3 \
		-o src/villCompiler.o

# The yaml parser is written in C, not C++
src/yaml_compose.o: src/yaml_compose.c src/yaml_compose_internal.h \
		src/yaml_parse.h Makefile
	cc -c src/yaml_compose.c -O3 -o src/yaml_compose.o

src/yaml_compose_alias.o: src/yaml_compose_alias.c \
		src/yaml_compose_internal.h src/yaml_compose.h Makefile
	cc -c src/yaml_compose_alias.c -O3 -o src/yaml_compose_alias.o

src/yaml_compose_anchor.o: src/yaml_compose_anchor.c \
		src/yaml_compose_internal.h src/yaml_compose.h Makefile
	cc -c src/yaml_compose_anchor.c -O3 -o src/yaml_compose_anchor.o

src/yaml_compose_mapping.o: src/yaml_compose_mapping.c \
		src/yaml_compose_internal.h src/yaml_compose.h Makefile
	cc -c src/yaml_compose_mapping.c -O3 -o src/yaml_compose_mapping.o

src/yaml_compose_root.o: src/yaml_compose_root.c \
		src/yaml_compose_internal.h src/yaml_compose.h Makefile
	cc -c src/yaml_compose_root.c -O3 -o src/yaml_compose_root.o

test/yaml_compose_roundtrip: test/yaml_compose_roundtrip.c \
		src/yaml_compose.h src/yaml_compose.o src/yaml_compose_alias.o \
		src/yaml_compose_anchor.o src/yaml_compose_mapping.o \
		src/yaml_compose_scalar.o src/yaml_compose_sequence.o \
		src/yaml_compose_tag.o src/yaml_decompose.o src/yaml_parse.o \
		src/yaml_token.o Makefile
	cc test/yaml_compose_roundtrip.c src/yaml_compose.o \
		src/yaml_compose_alias.o src/yaml_compose_anchor.o \
		src/yaml_compose_mapping.o src/yaml_compose_scalar.o \
		src/yaml_compose_sequence.o src/yaml_compose_tag.o \
		src/yaml_decompose.o src/yaml_parse.o src/yaml_token.o \
		-O3 -o test/yaml_compose_roundtrip

src/yaml_compose_scalar.o: src/yaml_compose_scalar.c \
		src/yaml_compose_internal.h src/yaml_compose.h Makefile
	cc -c src/yaml_compose_scalar.c -O3 -o src/yaml_compose_scalar.o

src/yaml_compose_sequence.o: src/yaml_compose_sequence.c \
		src/yaml_compose_internal.h src/yaml_compose.h Makefile
	cc -c src/yaml_compose_sequence.c -O3 -o src/yaml_compose_sequence.o

src/yaml_compose_tag.o: src/yaml_compose_tag.c \
		src/yaml_compose_internal.h src/yaml_compose.h Makefile
	cc -c src/yaml_compose_tag.c -O3 -o src/yaml_compose_tag.o

src/yaml_decompose.o: src/yaml_decompose.c src/yaml_compose_internal.h \
		src/yaml_compose.h Makefile
	cc -c src/yaml_decompose.c -O3 -o src/yaml_decompose.o

src/yaml_parse.o: src/yaml_parse.c src/yaml_parse.h src/yaml_token.h \
		Makefile
	cc -c src/yaml_parse.c -O3 -o src/yaml_parse.o

test/yaml_parse_roundtrip: test/yaml_parse_roundtrip.c \
		src/yaml_parse.h src/yaml_parse.o src/yaml_token.o Makefile
	cc test/yaml_parse_roundtrip.c src/yaml_parse.o \
		src/yaml_token.o -O3 -o test/yaml_parse_roundtrip

src/yaml_token.o: src/yaml_token.c src/yaml_token.h Makefile
	cc -c src/yaml_token.c -O3 -o src/yaml_token.o

test/yaml_token_roundtrip: test/yaml_token_roundtrip.c \
		src/yaml_token.o Makefile
	cc test/yaml_token_roundtrip.c src/yaml_token.o -O3 \
		-o test/yaml_token_roundtrip

# Miscellaneous non object file targets

# test does a quick sanity check with some arbitrary example files
test: vill test/yaml_compose_roundtrip test/yaml_parse_roundtrip \
		test/yaml_token_roundtrip
	@# paths are relative to pugs/src/perl6 for viv to use
	@test/yaml_token_roundtrip  -e 'print "token round trip"'
	@test/yaml_token_roundtrip  vill/test/01-print.t
	@test/yaml_token_roundtrip  ../../t/spec/S02-literals/autoref.t
	@test/yaml_token_roundtrip  ../../t/spec/S02-literals/char-by-name.t
	@test/yaml_token_roundtrip  ../../t/spec/S02-literals/subscript.t
	@test/yaml_parse_roundtrip  -e 'print "parse round trip"'
	@test/yaml_parse_roundtrip  vill/test/01-print.t
	@test/yaml_parse_roundtrip  ../../t/spec/S02-literals/hex_chars.t
	@test/yaml_parse_roundtrip  ../../t/spec/S02-literals/char-by-number.t
	@#test/yaml_compose_roundtrip -e 'print "compose round trip"'
	@#test/yaml_compose_roundtrip  vill/test/01-print.t
	@./vill -e 'print "hello world!"' -o test/hello

tokentest: test/yaml_token_roundtrip
	@# list the test files relative to viv's directory, pugs/src/perl6
	@cd ..; find ../../t/spec/S02-builtin_data_types \
		-name '*.t' | sort | \
		( cd vill; xargs test/yaml_token_roundtrip )

parsetest: test/yaml_parse_roundtrip
	@# list the test files relative to viv's directory, pugs/src/perl6
	@cd ..; find ../../t/spec/S02-literals \
		-name '*.t' | sort | \
		( cd vill; xargs test/yaml_parse_roundtrip )

clean:
	@find . -name '*~'          -exec rm {} ';'  # editor backup files
	@find . -name '*.o'         -exec rm {} ';'  # compiled object files
	@find . -executable -type f -exec rm {} ';'  # er, executables?

wordcount:
	@find . -name '*.[ch]*' | grep -v '\.svn' | grep -v '~$$' \
		| sort | xargs 'wc'

help:
	@echo
	@echo 'Makefile for vill: you can make these targets:'
	@echo
	@echo 'vill      (default) the viv+llvm mashup executable'
	@echo 'test      checks whether vill runs ok'
	@echo 'spectest  TODO'
	@echo 'parsetest run yaml_parse_roundtrip on part of pugs/t/spec/*'
	@echo 'tokentest run yaml_token_roundtrip on part of pugs/t/spec/*'
	@echo 'clean     delete editor backups and generated files'
	@echo 'wordcount line, word and byte counts of all source files'
	@echo 'help      display this help'
	@echo

# Notes:
# cc and c++ option -ggdb produces GNU Debugger compatible output
# 
