// options.cpp
// Read options and arguments in the command line.
// In order to minimize the risk of future incompatibilities, vill uses
// just the C standard library that processes single letter options.
// This is consistent with Perl 5.
#include "villCompiler.h"  // fprintf printf stderr
#include <unistd.h>        // getopt

// TODO (LHF): Consider using getpid() to make the output filename
// unique at a given moment, so that concurrent executions of 'vill'
// do not clash.

#define USAGE "\nUsage: %s [options] [programfile]\n" \
  "-c       check and compile but do not run\n" \
  "-Dflags  set Debug flags, see -H for details\n" \
  "-e cmd   execute cmd instead of programfile" \
            " (multiple -e possible)\n" \
  "-h       show this help\n" \
  "-H       show flags extended Debug help\n" \
  "-o file  write compiled output to file instead of /tmp/a.out\n" \
  "\n"

#define DEBUG_USAGE "Usage: %s -Dletters\n" \
  " p  parse tree (YAML format from viv)\n" \
  " s  syntax tree (AST hierarchy)\n" \
  " U  User defined (Codegen trace)\n" \
  "\n"

int
local_debug_options( char * letters ) {
  char * letter;
  int flags = 0;
  for ( letter=letters; *letter; ++letter ) {
    // fprintf( stderr, "Debug flag %c\n", *letter );
    switch ( * letter ) {
      case 'p': flags |= DEBUG_PARSE;           break;
      case 's': flags |= DEBUG_SYNTAXTREE;      break;
      case 'U': flags |= DEBUG_USER;            break;
      case 'v': flags |= DEBUG_VERBOSE;         break;
      default: break; // TODO: complain about error
    }
  }
  return flags;
}

void
villCompiler::options( int argc, char *argv[] ) {
  int opt;
  while ((opt = getopt(argc, argv, "cD:e:hHo:")) != -1) {
    switch (opt) {
      case 'c':
        compile_only = 1;
        break;
      case 'D':
        debug_flags = local_debug_options( optarg );
        break;
      case 'e':
        commandline = (char *) realloc( commandline,
          strlen(commandline) + strlen(optarg) + 1 );
        strcat( commandline, optarg );
        break;
      case 'h':
        fprintf( stderr, USAGE, argv[0] );
        exit(EXIT_SUCCESS);
        break;
      case 'H':
        fprintf( stderr, DEBUG_USAGE, argv[0] );
        exit(EXIT_SUCCESS);
        break;
      case 'o':
        output_filename = (char *) realloc(output_filename,
          strlen(optarg) + 1);
        strcpy( output_filename, optarg );
        break;
      default: // react to invalid options with help
        fprintf( stderr, USAGE, argv[0] );
        exit(EXIT_FAILURE);
    }
  }
}
