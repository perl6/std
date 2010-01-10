// run.cpp
// execute the program that 'vill' has just compiled and saved
#include <stdlib.h>        // free malloc
#include <string.h>        // strcat strcpy strlen
#include "villCompiler.h"

// Run the native code executable file
int
villCompiler::run() {
  int status = 0;
  char * command;
  // Skip the run if the -c option had been given.
  if ( ! compile_only ) {
    // So we're going to run.  Ensure the executable name has a path.
    if ( strspn( output_filename, "/." ) > 0 ) { // Either / or . is ok.
      // The name is already prefixed by a directory, so keep that.
      command = (char *) malloc( 1 + strlen(output_filename) );
      strcpy( command, output_filename );
    }
    else {
      // This command would be at the mercy of a random search path.
      // Fix that by prefixing the current directory.
      command = (char *) malloc( 3 + strlen(output_filename) );
      sprintf( command, "./%s", output_filename );
    }
    // Run it!
    assert( ( status = system( command ) ) > -1 );
    // TODO: there should be no need for vill to run any longer, so try
    // replacing the above system() with execl execlp execv or execvp.
    free( command );
  }
  return status;
}
