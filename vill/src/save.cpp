// save.cpp: make a bitcode file, then convert it to native code
#include <llvm/Bitcode/ReaderWriter.h> // WriteBitcodeToFile
#include <llvm/Support/raw_ostream.h>  // raw_fd_ostream
#include <stdlib.h>                    // free malloc realloc
#include <string.h>                    // strcat strcpy strlen
#include "villCompiler.h"              // villCompiler villModule

void
villCompiler::save() {
  // create the LLVM bitcode (Intermediate Representation) file
  const char * bitcode_suffix = ".bc";
  char * command;
  char * bitcode_filename = (char *) malloc( strlen(output_filename)
    + strlen(bitcode_suffix) + 1);
  strcpy( bitcode_filename, output_filename );
  strcat( bitcode_filename, bitcode_suffix );
  raw_ostream *ostream;
  std::string ErrInfo;
  //change for llvm 2.7
  ostream = new raw_fd_ostream( bitcode_filename, ErrInfo, raw_fd_ostream::F_Binary );
  // llvm 2.6
  //ostream   = new raw_fd_ostream( bitcode_filename, true, true, ErrInfo );
  WriteBitcodeToFile(villModule, *ostream);
  delete ostream;
  // run the command to convert bitcode to native executable
  const char * loader_prefix = "llvm-ld -native -o ";
  command = (char *) malloc( strlen(loader_prefix)
    + strlen(bitcode_filename) + strlen(output_filename) + 2 );
  strcpy( command, loader_prefix );
  strcat( command, output_filename );
  strcat( command, " " );
  strcat( command, bitcode_filename );
  // fprintf(stderr,"LLVM loader command: %s", command);
  int status = system(command);
  assert( status == 0 );
  // run the command to delete the bitcode file
  const char * remove_prefix = "rm ";
  command = (char *) realloc( command, strlen(remove_prefix)
    + strlen(bitcode_filename) + 1 );       // + 1 for the \0 terminator
  strcpy( command, remove_prefix );
  strcat( command, bitcode_filename );
  status = system(command);
  assert( status == 0 );
  // clean up
  free( command );
  free( bitcode_filename );
}
