// villCompiler.cpp
// constructor and destructor of villCompiler object
#include <stdlib.h>                     // malloc
#include "villCompiler.h"
#include "llvm/Support/ManagedStatic.h" // llvm_shutdown

// constructor
villCompiler::villCompiler() {
  AST             = NULL;
  compile_only    = 0;
  debug_flags     = 0;
  commandline     = (char *) malloc(1);
  strcpy( commandline, "" );
  programfile     = (char *) malloc(1);
  strcpy( programfile, "" );
  output_filename = (char *) malloc(11);
  strcpy( output_filename, "/tmp/a.out" );
  // an LLVM Module is the outermost container for a program
  villModule = new Module("villModule", villContext );
}

// destructor
villCompiler::~villCompiler() {
  delete villModule;
  llvm_shutdown();
}
