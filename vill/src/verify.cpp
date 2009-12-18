// verify.cpp: check the integrity of the all the bitcode in a Module
#include "villCompiler.h"              // villCompiler
#include "llvm/Analysis/Verifier.h"    // verifyModule
#include "llvm/Support/raw_ostream.h"  // raw_ostream raw_fd_ostream outs errs

void
villCompiler::verify() {
  if (verifyModule(*villModule)) {
    errs() << "Error: module failed verification.  This shouldn't happen.\n";
    abort();
  }
}
