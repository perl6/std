// load_ast.cpp
// Run the viv parser and import the Abstract Syntax Tree
#include "villCompiler.h"  // villCompiler
#include "yaml_compose.h"  // graph_node yaml_compose
#include <fcntl.h>         // open O_RDONLY
#include <stdio.h>         // fgets fprintf pclose popen
// #define NDEBUG          // #undef: assert() on, #define: assert() off

using namespace llvm;

int
villCompiler::load_ast() {
  int status;
  FILE * infile;
  int cur_dir_descriptor = open(".", O_RDONLY);
  int viv_dir_descriptor = open(VIV_RELATIVE_PATH, O_RDONLY); /* eg ".." */
  assert( cur_dir_descriptor >= 0 );
  assert( viv_dir_descriptor >= 0 );
  // fprintf( stderr, "programfile: %s\n", programfile );
  // fprintf( stderr, "commandline: %s\n", commandline );
  char * viv_command;
  if ( strlen(commandline) > 0 ) {
    viv_command = (char *) malloc( 12 + strlen(commandline) );
    sprintf( viv_command, "./viv -e '%s'", commandline );
  }
  else {
    viv_command = (char *) malloc( 107 + strlen(programfile) );
    sprintf( viv_command, "./viv %s", programfile );
  }
  // fprintf( stderr, "viv_command: %s\n", viv_command );
  status = fchdir(viv_dir_descriptor); // viv directory (pugs/src/perl6)
  assert( status == 0 );
  infile = popen( viv_command, "r" );  // run viv
  assert( infile != NULL );
  status = fchdir(cur_dir_descriptor); // previously stashed directory
  assert( status == 0 );
  // transfer the debug flags from the C++ villCompiler object
  // to the C static variables in yaml_compose.c
  yaml_compose_debug_flag    = debug_flags & DEBUG_PARSE;
  yaml_compose_debug_verbose = debug_flags & DEBUG_VERBOSE;
  AST = yaml_compose( infile );
  link_codegen(); // traverse the AST, setting codegen function pointers
  status = pclose( infile );
#if 0
  fprintf( stderr, "viv status = 0x%x\n", status );
#endif
  return status;
}
