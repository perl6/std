// optimize.cpp
// transform bitcode to be more efficient
#include "villCompiler.h"              // villCompiler
#include "llvm/PassManager.h"          // FunctionPassManager
#include "llvm/Transforms/Scalar.h"    // createReassociatePass

void
villCompiler::optimize() {
#if 0
  // TODO: make it that the FPM.run() is called once per function.
  FunctionPassManager villFunctionPassManager(villModule);
  villFunctionPassManager.add(createInstructionCombiningPass());
  villFunctionPassManager.add(createReassociatePass());
  villFunctionPassManager.doInitialization();
#endif
}
