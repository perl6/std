#include "villCompiler.h"
#include <stdio.h>  /* printf */

// ParseCommandLineOptions.cpp - this relatively long file name was
// retained to remain consistent the LLVM command processing library,
// see http://llvm.org/docs/CommandLine.html

// Using the original LLVM command processsing library as it currently
// works is unfortunately not practical, because various LLVM modules
// insert their own options and appear in the help output.  It would be
// really nice to have the same library working inpdependently, but that
// has not yet been built.

void
villCompiler::ParseCommandLineOptions( int argc, char *argv[] ) {
  if (argc < 2) {
    printf( "Usage: %s <filename>\n", argv[0] );
    abort();
  }
#if 0
  int i;
  for ( i=0; i<argc; ++i ) {
    printf( "argc[%d]: %s\n", i, argv[i] );
  }
#endif
  executable_filename = argv[1];
}
