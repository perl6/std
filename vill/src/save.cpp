// save.cpp: make a bitcode file, then convert it to native code
#include <llvm/Bitcode/ReaderWriter.h> // WriteBitcodeToFile
#include <llvm/Support/raw_ostream.h>  // raw_fd_ostream
#include <malloc.h>                    // free malloc realloc
#include <string.h>                    // strcat strcpy strlen
#include "villCompiler.h"              // villCompiler villModule

void
villCompiler::save() {
  // create the LLVM bitcode (Intermediate Representation) file
  const char * bitcode_suffix = ".bc";
  char * bitcode_filename = (char *) malloc( strlen(output_filename)
    + strlen(bitcode_suffix) + 1);
  strcpy( bitcode_filename, output_filename );
  strcat( bitcode_filename, bitcode_suffix );
  raw_ostream *ostream;
  std::string ErrInfo;
  //change for llvm 2.7
  //ostream = new raw_fd_ostream( bitcode_filename, ErrInfo, raw_fd_ostream::F_Binary );
  ostream   = new raw_fd_ostream( bitcode_filename, true, true, ErrInfo );
  WriteBitcodeToFile(villModule, *ostream);
  delete ostream;
  // run the command to convert bitcode to native executable
  const char * loader_prefix = "llvm-ld -native ";
  const char * loader_option = " -o ";
  char * command;
  command = (char *) malloc( strlen(loader_prefix)
    + strlen(bitcode_filename) + strlen(loader_option)
    + strlen(output_filename) + 1 );
  strcpy( command, loader_prefix );
  strcat( command, bitcode_filename );
  strcat( command, loader_option );
  strcat( command, output_filename );
  assert( system(command) == 0 );
  // run the command to delete the bitcode file
  const char * remove_prefix = "rm ";
  command = (char *) realloc( command, strlen(remove_prefix)
    + strlen(bitcode_filename) + 1 );
  strcpy( command, remove_prefix );
  strcat( command, bitcode_filename );
  assert( system(command) == 0 );
  // clean up
  free( command );
  free( bitcode_filename );
}
