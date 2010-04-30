# Makefile for vill - a viv+llvm mashup
# To keep things understandable, use of 'make' variables is avoided
# except for the 'objects' at the beginning.  It reads so much easier
# not having to hunt through dozens of trivial substitutions ;-)

# Object file names sorted alphabetically, each one comes from a
# corresponding .cpp or .c source file and some .h files (listed below)
objects = src/arglist.o src/args.o src/ast_mapentry.o \
	src/ast_seqentry.o src/ast_type_tag_equals.o src/backslash__S_n.o \
	src/codegen.o src/comp_unit.o src/decint.o src/declarator.o \
	src/desigilname.o \
	src/dottyop.o src/dotty__S_Dot.o src/eat_terminator.o \
	src/escape__S_Back.o src/identifier.o src/infixish.o \
	src/infix__S_Comma.o src/infix__S_Equal.o src/infix__S_Plus.o \
	src/integer.o \
	src/link_codegen.o src/load_ast.o src/longname.o src/main.o \
	src/make_puts.o src/methodop.o src/modifier.o src/modifier_expr.o \
	src/name.o src/nibbler.o src/noun__S_term.o src/noun__S_value.o \
	src/number__S_numish.o src/numish.o src/optimize.o src/options.o \
	src/POST.o src/quote__S_Double_Double.o src/run.o src/save.o \
	src/scoped.o src/scope_declarator__S_my.o src/semiarglist.o \
	src/sigil__S_At.o src/statement.o \
	src/statementlist.o src/statement_mod_cond__S_if.o \
	src/statement_mod_loop__S_for.o src/Str.o src/SYM_dotty__S_Dot.o \
	src/SYM_infix__S_Comma.o src/SYM_infix__S_Equal.o \
	src/SYM_infix__S_Plus.o src/termish.o \
	src/term__S_dotty.o src/term__S_identifier.o \
	src/term__S_scope_declarator.o src/term__S_value.o \
	src/term__S_variable.o src/twigil__S_Star.o src/unload_ast.o \
	src/value__S_number.o src/value__S_quote.o src/variable.o \
	src/variable_declarator.o src/verify.o src/villCompiler.o \
	src/yaml_compose.o src/yaml_compose_alias.o \
	src/yaml_compose_anchor.o src/yaml_compose_mapping.o \
	src/yaml_compose_root.o src/yaml_compose_scalar.o \
	src/yaml_compose_sequence.o src/yaml_compose_tag.o \
	src/yaml_decompose.o src/yaml_parse.o src/yaml_token.o

# Executable target files sorted aphabetically

# The first target is the default when make is run without arguments.
# Build the main executable by linking all the object files together.
vill: $(objects) src/villCompiler.h src/llvm-version2.7 Makefile
	@echo 'Link *.o -> ./vill'
	@c++ $(objects) `llvm-config --cxxflags --ldflags --libs bitwriter` \
	  -O3 -o vill
	@find src -name 'llvm-version*' -exec rm {} \;

test/yaml_compose_roundtrip: test/yaml_compose_roundtrip.c \
		src/yaml_compose.h src/graph_traverse.o \
		src/graph_traverse_begin.o src/graph_traverse_end.o \
		src/yaml_compose.o src/yaml_compose_alias.o \
		src/yaml_compose_anchor.o src/yaml_compose_mapping.o \
		src/yaml_compose_scalar.o src/yaml_compose_sequence.o \
		src/yaml_compose_tag.o src/yaml_decompose.o src/yaml_parse.o \
		src/yaml_token.o Makefile
	@echo 'Compile+link test/yaml_compose_roundtrip'
	@cc test/yaml_compose_roundtrip.c src/graph_traverse.o \
		src/graph_traverse_begin.o src/graph_traverse_end.o \
		src/yaml_compose.o src/yaml_compose_alias.o \
		src/yaml_compose_anchor.o src/yaml_compose_mapping.o \
		src/yaml_compose_scalar.o src/yaml_compose_sequence.o \
		src/yaml_compose_tag.o src/yaml_decompose.o src/yaml_parse.o \
		src/yaml_token.o -O3 -o test/yaml_compose_roundtrip

test/yaml_parse_roundtrip: test/yaml_parse_roundtrip.c \
		src/yaml_parse.h src/yaml_parse.o src/yaml_token.o Makefile
	@echo 'Compile+link test/yaml_parse_roundtrip'
	@cc test/yaml_parse_roundtrip.c src/yaml_parse.o src/yaml_token.o \
		-O3 -o test/yaml_parse_roundtrip

test/yaml_token_roundtrip: test/yaml_token_roundtrip.c \
		src/yaml_token.o Makefile
	@echo 'Compile+link test/yaml_token_roundtrip'
	@cc test/yaml_token_roundtrip.c src/yaml_token.o -O3 \
		-o test/yaml_token_roundtrip

# Object targets listed alphabetically by filename but not by directory.
# Separately compile each object from its corresponding source file.
src/arglist.o: src/arglist.cpp src/villCompiler.h Makefile
	c++ -c src/arglist.cpp `llvm-config --cxxflags` -O3 -o src/arglist.o

src/args.o: src/args.cpp src/villCompiler.h Makefile
	c++ -c src/args.cpp `llvm-config --cxxflags` -O3 -o src/args.o

src/ast_mapentry.o: src/ast_mapentry.c src/yaml_compose.h Makefile
	cc -c src/ast_mapentry.c -O3 -o src/ast_mapentry.o

src/ast_seqentry.o: src/ast_seqentry.c src/yaml_parse.h Makefile
	cc -c src/ast_seqentry.c -O3 -o src/ast_seqentry.o

src/ast_type_tag_equals.o: src/ast_type_tag_equals.c \
		src/yaml_compose.h Makefile
	cc -c src/ast_type_tag_equals.c -O3 -o src/ast_type_tag_equals.o

src/backslash__S_n.o: src/backslash__S_n.cpp src/villCompiler.h Makefile
	c++ -c src/backslash__S_n.cpp `llvm-config --cxxflags` -O3 \
		-o src/backslash__S_n.o

src/codegen.o: src/codegen.cpp src/villCompiler.h Makefile
	c++ -c src/codegen.cpp `llvm-config --cxxflags` -O3 -o src/codegen.o

src/comp_unit.o: src/comp_unit.cpp src/villCompiler.h Makefile
	c++ -c src/comp_unit.cpp `llvm-config --cxxflags` -O3 -o src/comp_unit.o

src/decint.o: src/decint.cpp src/villCompiler.h Makefile
	c++ -c src/decint.cpp `llvm-config --cxxflags` -O3 -o src/decint.o

src/declarator.o: src/declarator.cpp src/villCompiler.h Makefile
	c++ -c src/declarator.cpp `llvm-config --cxxflags` -O3 -o src/declarator.o

src/desigilname.o: src/desigilname.cpp src/villCompiler.h Makefile
	c++ -c src/desigilname.cpp `llvm-config --cxxflags` -O3 -o src/desigilname.o

src/dottyop.o: src/dottyop.cpp src/villCompiler.h Makefile
	c++ -c src/dottyop.cpp `llvm-config --cxxflags` -O3 -o src/dottyop.o

src/dotty__S_Dot.o: src/dotty__S_Dot.cpp src/villCompiler.h Makefile
	c++ -c src/dotty__S_Dot.cpp `llvm-config --cxxflags` -O3 -o src/dotty__S_Dot.o

src/eat_terminator.o: src/eat_terminator.cpp src/villCompiler.h Makefile
	c++ -c src/eat_terminator.cpp `llvm-config --cxxflags` -O3 \
		-o src/eat_terminator.o

src/escape__S_Back.o: src/escape__S_Back.cpp src/villCompiler.h Makefile
	c++ -c src/escape__S_Back.cpp `llvm-config --cxxflags` -O3 \
		-o src/escape__S_Back.o

src/graph_traverse.o: src/graph_traverse.c src/graph_traverse.h \
		src/yaml_compose_internal.h Makefile
	cc -c src/graph_traverse.c -O3 -o src/graph_traverse.o

src/graph_traverse_begin.o: src/graph_traverse_begin.c \
		src/graph_traverse.h src/yaml_compose_internal.h Makefile
	cc -c src/graph_traverse_begin.c -O3 -o src/graph_traverse_begin.o

src/graph_traverse_end.o: src/graph_traverse_end.c \
		src/graph_traverse.h src/yaml_compose_internal.h Makefile
	cc -c src/graph_traverse_end.c -O3 -o src/graph_traverse_end.o

src/integer.o: src/integer.cpp src/villCompiler.h Makefile
	c++ -c src/integer.cpp `llvm-config --cxxflags` -O3 -o src/integer.o

src/identifier.o: src/identifier.cpp src/villCompiler.h Makefile
	c++ -c src/identifier.cpp `llvm-config --cxxflags` -O3 -o src/identifier.o

src/infixish.o: src/infixish.cpp src/villCompiler.h Makefile
	c++ -c src/infixish.cpp `llvm-config --cxxflags` -O3 -o src/infixish.o

src/infix__S_Comma.o: src/infix__S_Comma.cpp src/villCompiler.h Makefile
	c++ -c src/infix__S_Comma.cpp `llvm-config --cxxflags` -O3 \
		-o src/infix__S_Comma.o

src/infix__S_Equal.o: src/infix__S_Equal.cpp src/villCompiler.h Makefile
	c++ -c src/infix__S_Equal.cpp `llvm-config --cxxflags` -O3 \
		-o src/infix__S_Equal.o

src/infix__S_Plus.o: src/infix__S_Plus.cpp src/villCompiler.h Makefile
	c++ -c src/infix__S_Plus.cpp `llvm-config --cxxflags` -O3 \
		-o src/infix__S_Plus.o

src/link_codegen.o: src/link_codegen.cpp src/villCompiler.h \
		src/yaml_compose.h Makefile
	c++ -c src/link_codegen.cpp `llvm-config --cxxflags` -O3 -o src/link_codegen.o

src/load_ast.o: src/load_ast.cpp src/villCompiler.h src/yaml_compose.h \
		Makefile
	c++ -c src/load_ast.cpp `llvm-config --cxxflags` -O3 -o src/load_ast.o

src/longname.o: src/longname.cpp src/villCompiler.h Makefile
	c++ -c src/longname.cpp `llvm-config --cxxflags` -O3 -o src/longname.o

src/main.o: src/main.cpp src/villCompiler.h Makefile
	c++ -c src/main.cpp `llvm-config --cxxflags` -O3 -o src/main.o

src/make_puts.o: src/make_puts.cpp src/villCompiler.h Makefile
	c++ -c src/make_puts.cpp `llvm-config --cxxflags` -O3 -o src/make_puts.o

src/methodop.o: src/methodop.cpp src/villCompiler.h Makefile
	c++ -c src/methodop.cpp `llvm-config --cxxflags` -O3 -o src/methodop.o

src/modifier.o: src/modifier.cpp src/villCompiler.h Makefile
	c++ -c src/modifier.cpp `llvm-config --cxxflags` -O3 -o src/modifier.o

src/modifier_expr.o: src/modifier_expr.cpp src/villCompiler.h Makefile
	c++ -c src/modifier_expr.cpp `llvm-config --cxxflags` -O3 -o src/modifier_expr.o

src/name.o: src/name.cpp src/villCompiler.h Makefile
	c++ -c src/name.cpp `llvm-config --cxxflags` -O3 -o src/name.o

src/nibbler.o: src/nibbler.cpp src/villCompiler.h Makefile
	c++ -c src/nibbler.cpp `llvm-config --cxxflags` -O3 -o src/nibbler.o

src/noun__S_term.o: src/noun__S_term.cpp src/villCompiler.h Makefile
	c++ -c src/noun__S_term.cpp `llvm-config --cxxflags` -O3 -o src/noun__S_term.o

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
	c++ -c src/optimize.cpp `llvm-config --cxxflags` -O3 -o src/optimize.o

src/options.o: src/options.cpp src/villCompiler.h Makefile
	c++ -c src/options.cpp `llvm-config --cxxflags` -O3 -o src/options.o

src/POST.o: src/POST.cpp src/villCompiler.h Makefile
	c++ -c src/POST.cpp `llvm-config --cxxflags` -O3 -o src/POST.o

src/quote__S_Double_Double.o: src/quote__S_Double_Double.cpp \
		src/villCompiler.h Makefile
	c++ -c src/quote__S_Double_Double.cpp `llvm-config --cxxflags` -O3 \
		-o src/quote__S_Double_Double.o

src/run.o: src/run.cpp src/villCompiler.h Makefile
	c++ -c src/run.cpp `llvm-config --cxxflags` -O3 -o src/run.o

src/save.o: src/save.cpp src/villCompiler.h Makefile
	c++ -c src/save.cpp `llvm-config --cxxflags` -O3 -o src/save.o

src/scoped.o: src/scoped.cpp src/villCompiler.h Makefile
	c++ -c src/scoped.cpp `llvm-config --cxxflags` -O3 -o src/scoped.o

src/scope_declarator__S_my.o: src/scope_declarator__S_my.cpp \
		src/villCompiler.h Makefile
	c++ -c src/scope_declarator__S_my.cpp `llvm-config --cxxflags` -O3 \
		-o src/scope_declarator__S_my.o

src/semiarglist.o: src/semiarglist.cpp src/villCompiler.h Makefile
	c++ -c src/semiarglist.cpp `llvm-config --cxxflags` -O3 -o src/semiarglist.o

src/sigil__S_At.o: src/sigil__S_At.cpp src/villCompiler.h Makefile
	c++ -c src/sigil__S_At.cpp `llvm-config --cxxflags` -O3 -o src/sigil__S_At.o

src/statement.o: src/statement.cpp src/villCompiler.h Makefile
	c++ -c src/statement.cpp `llvm-config --cxxflags` -O3 -o src/statement.o

src/statementlist.o: src/statementlist.cpp src/villCompiler.h Makefile
	c++ -c src/statementlist.cpp `llvm-config --cxxflags` -O3 \
		-o src/statementlist.o

src/statement_mod_cond__S_if.o: src/statement_mod_cond__S_if.cpp \
		src/villCompiler.h Makefile
	c++ -c src/statement_mod_cond__S_if.cpp `llvm-config --cxxflags` \
		-O3 -o src/statement_mod_cond__S_if.o

src/statement_mod_loop__S_for.o: src/statement_mod_loop__S_for.cpp \
		src/villCompiler.h Makefile
	c++ -c src/statement_mod_loop__S_for.cpp `llvm-config --cxxflags` \
		-O3 -o src/statement_mod_loop__S_for.o

src/Str.o: src/Str.cpp src/villCompiler.h Makefile
	c++ -c src/Str.cpp `llvm-config --cxxflags` -O3 -o src/Str.o

src/SYM_dotty__S_Dot.o: src/SYM_dotty__S_Dot.cpp src/villCompiler.h \
		Makefile
	c++ -c src/SYM_dotty__S_Dot.cpp `llvm-config --cxxflags` -O3 \
		-o src/SYM_dotty__S_Dot.o

src/SYM_infix__S_Comma.o: src/SYM_infix__S_Comma.cpp \
		src/villCompiler.h Makefile
	c++ -c src/SYM_infix__S_Comma.cpp `llvm-config --cxxflags` -O3 \
		-o src/SYM_infix__S_Comma.o

src/SYM_infix__S_Equal.o: src/SYM_infix__S_Equal.cpp \
		src/villCompiler.h Makefile
	c++ -c src/SYM_infix__S_Equal.cpp `llvm-config --cxxflags` -O3 \
		-o src/SYM_infix__S_Equal.o

src/SYM_infix__S_Plus.o: src/SYM_infix__S_Plus.cpp \
		src/villCompiler.h Makefile
	c++ -c src/SYM_infix__S_Plus.cpp `llvm-config --cxxflags` -O3 \
		-o src/SYM_infix__S_Plus.o

src/termish.o: src/termish.cpp src/villCompiler.h Makefile
	c++ -c src/termish.cpp `llvm-config --cxxflags` -O3 -o src/termish.o

src/term__S_dotty.o: src/term__S_dotty.cpp src/villCompiler.h Makefile
	c++ -c src/term__S_dotty.cpp `llvm-config --cxxflags` -O3 \
		-o src/term__S_dotty.o

src/term__S_identifier.o: src/term__S_identifier.cpp \
		src/villCompiler.h Makefile
	c++ -c src/term__S_identifier.cpp `llvm-config --cxxflags` -O3 \
		-o src/term__S_identifier.o

src/term__S_scope_declarator.o: src/term__S_scope_declarator.cpp \
		src/villCompiler.h Makefile
	c++ -c src/term__S_scope_declarator.cpp `llvm-config --cxxflags` \
		-O3 -o src/term__S_scope_declarator.o

src/term__S_value.o: src/term__S_value.cpp src/villCompiler.h Makefile
	c++ -c src/term__S_value.cpp `llvm-config --cxxflags` -O3 \
		-o src/term__S_value.o

src/term__S_variable.o: src/term__S_variable.cpp src/villCompiler.h Makefile
	c++ -c src/term__S_variable.cpp `llvm-config --cxxflags` -O3 \
		-o src/term__S_variable.o

src/twigil__S_Star.o: src/twigil__S_Star.cpp src/villCompiler.h Makefile
	c++ -c src/twigil__S_Star.cpp `llvm-config --cxxflags` -O3 \
		-o src/twigil__S_Star.o

src/unload_ast.o: src/unload_ast.cpp src/villCompiler.h \
		src/yaml_compose.h Makefile
	c++ -c src/unload_ast.cpp `llvm-config --cxxflags` -O3 -o src/unload_ast.o

src/value__S_number.o: src/value__S_number.cpp src/villCompiler.h \
		Makefile
	c++ -c src/value__S_number.cpp `llvm-config --cxxflags` -O3 \
		-o src/value__S_number.o

src/value__S_quote.o: src/value__S_quote.cpp src/villCompiler.h Makefile
	c++ -c src/value__S_quote.cpp `llvm-config --cxxflags` -O3 \
		-o src/value__S_quote.o

src/variable.o: src/variable.cpp src/villCompiler.h Makefile
	c++ -c src/variable.cpp `llvm-config --cxxflags` -O3 -o src/variable.o

src/variable_declarator.o: src/variable_declarator.cpp \
		src/villCompiler.h Makefile
	c++ -c src/variable_declarator.cpp `llvm-config --cxxflags` -O3 -o \
		src/variable_declarator.o

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

src/yaml_token.o: src/yaml_token.c src/yaml_token.h Makefile
	cc -c src/yaml_token.c -O3 -o src/yaml_token.o

# Miscellaneous non object file targets

clean:
	@find . -name '*~'         -exec rm {} ';'  # editor backup files
	@find . -name '*.o'        -exec rm {} ';'  # compiled object files
	@find . -perm -u=x -type f -exec rm {} ';'  # executables

# Cause the make to fail if the LLVM version is not 2.6
# This (or rather the rm in the vill target) has the unfortunate side
# effect of causing every plain 'make' to do a top level recompile/link.
# A more elegant solution that avoids unnecessary rebuilds would be very
# welcome.
src/llvm-version2.7:
	@find src -name 'llvm-version*' -exec rm {} \;
	@export VERSION=`llvm-config --version`; echo $$VERSION > \
	  src/llvm-version$$VERSION
	@test -e src/llvm-version2.7

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

fruit:
	@grep -nr 'TODO.*LHF' src/* test/*

todo:
	@grep -nr 'TODO'      src/* test/*

counts:
	@echo ' Lines  Words  Bytes File'
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
	@echo 'todo      show all TODO lines'
	@echo 'fruit     search for LHF (low hanging fruit) in TODO lines'
	@echo 'counts    line, word and byte counts of all source files'
	@echo 'help      display this help'
	@echo

# Notes:
# cc and c++ option -ggdb produces GNU Debugger compatible output
#
