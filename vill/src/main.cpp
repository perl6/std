// main.cpp: compile and run a Perl 6 program
#include "villCompiler.h"

int
main( int argc, char *argv[] ) {
  int status;
  villCompiler vill;         // create a vill compiler object
  vill.options(argc, argv);  // read the command line
  status = vill.load_ast();  // run viv and import the resulting AST
  if ( status == 0 ) {       // proceed only if viv was happy
    vill.codegen();          // convert Perl 6 source to LLVM bitcode
    vill.unload_ast();       // free the AST from memory
    vill.optimize();         // apply LLVM magic to improve the bitcode
    vill.verify();           // hope nothing is broken
    vill.save();             // bitcode to file, convert to native code
  }
  status = vill.run();       // Execute the native code file
  return status;
}
