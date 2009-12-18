// run.cpp
// execute the program that 'vill' has just compiled and saved
#include <malloc.h>        // free malloc
#include <string.h>        // strcat strcpy strlen
#include "villCompiler.h"

// Run the native code executable file
int
villCompiler::run() {
  int status = 0;
  char * command;
  // skip the run if the -c option had been given
  if ( ! compile_only ) {
    if ( strspn( output_filename, "/." ) > 0 ) {
      // the name is already prefixed by a directory, so keep that
      command = (char *) malloc( 1 + strlen(output_filename) );
      strcpy( command, output_filename );
    }
    else {
      command = (char *) malloc( 3 + strlen(output_filename) );
      sprintf( command, "./%s", output_filename );
    }
    assert( ( status = system( command ) ) > -1 );
    // TODO: there should be no need for vill to run any longer, so try
    // replacing the system() with execl execlp execle execv or execvp.
    free( command );
  }
  return status;
}
