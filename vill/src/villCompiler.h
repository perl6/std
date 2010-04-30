// villCompiler.h
// the villCompiler class and the Codegen functions
#ifndef VILLCOMPILER_H
#define VILLCOMPILER_H
#include "llvm/LLVMContext.h"  // llvmContext Value
#include "llvm/Module.h"       // Module Value
#include "yaml_compose.h"      // graph_node Value
// #undef NDEBUG           // #undef: assert() on, #define: assert() off

using namespace llvm;

// tell 'vill' where to find the 'viv' Perl 6 parser
#define VIV_RELATIVE_PATH ".."

// villCompiler - container class for the 'vill' compiler
struct villCompiler {
  // data members
  struct graph_node * AST;          // Abstract Syntax Tree as C structs
  char *              commandline;     // -e 'command', multiple ok
  int                 compile_only;    // -c flag
  int                 debug_flags;     // -Dletters [p]  TODO:-Dnumber
  char *              output_filename; // -o name instead of a.out
  char *              programfile;     // Perl 6 source file
  LLVMContext         villContext;     // for LLVM internal use
  Module *            villModule;   // generated IR bitcode

  // public methods (alphabetical, not order of execution)
  void codegen();      // compile AST to Module
  int  load_ast();     // calls viv and yaml_compose, returns status
  void optimize();     // transform the IR code to be more efficient
  void options( int argc, char *argv[] );
  int  run();          // executes program, if -c option was not used
  void save();         // write IR to file and convert to native code
  void unload_ast();   // decompile the AST, freeing memory
  void verify();       // validate module IR integrity
       villCompiler(); // constructor
      ~villCompiler(); // destructor

  // helper methods
  void link_codegen();    // add pointers to code generators to (* data)
  void make_puts( BasicBlock *BB, StringRef s );   // sanity scaffolding
};
typedef Value * (* function_pointer)(struct villCompiler * vill,
  struct graph_node * node);

// Codegen functions (alphabetical)
Value * arglist(                  struct villCompiler *, struct graph_node *);
Value * args(                     struct villCompiler *, struct graph_node *);
Value * backslash__S_n(           struct villCompiler *, struct graph_node *);
Value * comp_unit(                struct villCompiler *, struct graph_node *);
Value * decint(                   struct villCompiler *, struct graph_node *);
Value * declarator(               struct villCompiler *, struct graph_node *);
Value * desigilname(              struct villCompiler *, struct graph_node *);
Value * dottyop(                  struct villCompiler *, struct graph_node *);
Value * dotty__S_Dot(             struct villCompiler *, struct graph_node *);
Value * eat_terminator(           struct villCompiler *, struct graph_node *);
Value * escape__S_Back(           struct villCompiler *, struct graph_node *);
Value * identifier(               struct villCompiler *, struct graph_node *);
Value * infixish(                 struct villCompiler *, struct graph_node *);
Value * infix__S_Comma(           struct villCompiler *, struct graph_node *);
Value * infix__S_Equal(           struct villCompiler *, struct graph_node *);
Value * infix__S_Plus(            struct villCompiler *, struct graph_node *);
Value * infix__S_Tilde(           struct villCompiler *, struct graph_node *);
Value * integer(                  struct villCompiler *, struct graph_node *);
Value * longname(                 struct villCompiler *, struct graph_node *);
Value * methodop(                 struct villCompiler *, struct graph_node *);
Value * modifier(                 struct villCompiler *, struct graph_node *);
Value * modifier_expr(            struct villCompiler *, struct graph_node *);
Value * name(                     struct villCompiler *, struct graph_node *);
Value * nibbler(                  struct villCompiler *, struct graph_node *);
Value * noun__S_scope_declarator( struct villCompiler *, struct graph_node *);
Value * noun__S_term(             struct villCompiler *, struct graph_node *);
Value * noun__S_value(            struct villCompiler *, struct graph_node *);
Value * number__S_numish(         struct villCompiler *, struct graph_node *);
Value * numish(                   struct villCompiler *, struct graph_node *);
Value * POST(                     struct villCompiler *, struct graph_node *);
Value * quote__S_Double_Double(   struct villCompiler *, struct graph_node *);
Value * scoped(                   struct villCompiler *, struct graph_node *);
Value * scope_declarator__S_my(   struct villCompiler *, struct graph_node *);
Value * semiarglist(              struct villCompiler *, struct graph_node *);
Value * sigil__S_At(              struct villCompiler *, struct graph_node *);
Value * sigil__S_Dollar(          struct villCompiler *, struct graph_node *);
Value * statement(                struct villCompiler *, struct graph_node *);
Value * statementlist(            struct villCompiler *, struct graph_node *);
Value * statement_mod_cond__S_if( struct villCompiler *, struct graph_node *);
Value * statement_mod_loop__S_for(struct villCompiler *, struct graph_node *);
Value * Str(                      struct villCompiler *, struct graph_node *);
Value * SYM_dotty__S_Dot(         struct villCompiler *, struct graph_node *);
Value * SYM_infix__S_Comma(       struct villCompiler *, struct graph_node *);
Value * SYM_infix__S_Equal(       struct villCompiler *, struct graph_node *);
Value * SYM_infix__S_Plus(        struct villCompiler *, struct graph_node *);
Value * termish(                  struct villCompiler *, struct graph_node *);
Value * term__S_identifier(       struct villCompiler *, struct graph_node *);
Value * term__S_dotty(            struct villCompiler *, struct graph_node *);
Value * term__S_scope_declarator( struct villCompiler *, struct graph_node *);
Value * term__S_value(            struct villCompiler *, struct graph_node *);
Value * term__S_variable(         struct villCompiler *, struct graph_node *);
Value * twigil__S_Star(           struct villCompiler *, struct graph_node *);
Value * value__S_number(          struct villCompiler *, struct graph_node *);
Value * value__S_quote(           struct villCompiler *, struct graph_node *);
Value * variable(                 struct villCompiler *, struct graph_node *);
Value * variable_declarator(      struct villCompiler *, struct graph_node *);

// Debug flags similar to -D in P5's perldoc perlrun. Set in options.cpp
#define DEBUG_PARSE            1
#define DEBUG_SYNTAXTREE    1024
#define DEBUG_USER          4096
#define DEBUG_VERBOSE    1048576

// Bit masks for the 'vill' specific graph_node -> flags.
// Take care to avoid bits defined near the end of src/yaml_compose.h 
#define AST_VISITED         0x20

#endif
