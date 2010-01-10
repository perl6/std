// optimize.cpp
// transform bitcode to be more efficient
#include "villCompiler.h"              // villCompiler
#include "llvm/ModuleProvider.h"       // ExistingModuleProvider
#include "llvm/PassManager.h"          // FunctionPassManager
#include "llvm/Support/raw_ostream.h"  // raw_ostream raw_fd_ostream
  // outs errs

void
villCompiler::optimize() {
  // TODO: fix: Optimize the code created so far (segfaults)
  // the LLVM tutorial shows it like this, but something must be missing.
#if 0
  ExistingModuleProvider villModuleProvider(villModule);
  FunctionPassManager villFPM(&villModuleProvider);
  FunctionPassManager *TheFPM = &villFPM;
  TheFPM = 0;
#endif
}
