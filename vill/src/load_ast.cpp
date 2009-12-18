// load_ast.cpp
// Run the viv parser and import the Abstract Syntax Tree
#include "villCompiler.h"  // villCompiler
#include "yaml_compose.h"  // graph_node yaml_compose
#include <stdio.h>         // fgets fprintf pclose popen

using namespace llvm;

int
villCompiler::load_ast() {
  int status;
  FILE * infile;
  char * cwd, * stash_current_dir;
  // Cannot be sure that cwd will be unchanged by chdir, so stash it.
  cwd = getcwd( NULL, 0 );
  assert( cwd != NULL );
  stash_current_dir = (char *) malloc( strlen(cwd) + 1 );
  assert( stash_current_dir != NULL );
  strcpy( stash_current_dir, cwd );
  assert( chdir( VIV_RELATIVE_PATH ) == 0 );
  // Run viv in its directory, composing the YAML produced into an AST.
  // fprintf( stderr, "programfile: %s\n", programfile );
  // fprintf( stderr, "commandline: %s\n", commandline );
  // return 0;
  char * viv_command;
  if ( strlen(commandline) > 0 ) {
    viv_command = (char *) malloc( 12 + strlen(commandline) );
    sprintf( viv_command, "./viv -e '%s'", commandline );
  }
  else {
    viv_command = (char *) malloc( 7 + strlen(programfile) );
    sprintf( viv_command, "./viv %s", programfile );
  }
  infile = popen( viv_command, "r" );
  assert( infile != NULL );
  // transfer the debug flags from the C++ villCompiler object
  // to the C static variables in yaml_compose.c
  yaml_compose_debug_flag    = debug_flags & DEBUG_PARSE;
  yaml_compose_debug_verbose = debug_flags & DEBUG_VERBOSE;
  AST = yaml_compose( infile );
  status = pclose( infile );
  link_codegen();
#if 0
  fprintf( stderr, "viv status = %d\n", status );
#endif
  // Change back to the previously stashed working directory
  assert( chdir( stash_current_dir ) == 0 );
  free( stash_current_dir );
  return status;
}
