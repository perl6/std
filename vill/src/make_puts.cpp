// make_puts.cpp
// add a puts call to the Basic Block
#include "villCompiler.h"
#include "llvm/ADT/STLExtras.h"        // array_lengthof array_endof
#include "llvm/Constants.h"            // ConstantArray
#include "llvm/DerivedTypes.h"
#include "llvm/Instructions.h" // CallInst

void
villCompiler::make_puts( BasicBlock *BB, StringRef s ) {
  Constant * msg = ConstantArray::get( villContext, s, true );
  GlobalVariable * globalmsg = new GlobalVariable(
    * villModule,
    msg -> getType(),
    true,
    GlobalValue::InternalLinkage,
    msg,
    "make_puts_msg");
  Constant * zero_32 = Constant::getNullValue( IntegerType::getInt32Ty(
                         villContext));
  Constant * gep_params[] = {
    zero_32,
    zero_32
  };
  Constant * msgptr = ConstantExpr::
    getGetElementPtr( globalmsg, gep_params,
                      llvm::array_lengthof(gep_params) );
  Value * puts_params[] = { msgptr };
  Function * puts_func = cast<Function>( villModule ->
                           getOrInsertFunction("puts",
                           IntegerType::getInt32Ty(villContext),
                           PointerType::getUnqual(
                             IntegerType::getInt8Ty(villContext)),NULL)
                         );
  CallInst * puts_call = CallInst::Create( puts_func,
                           puts_params, llvm::array_endof(puts_params),
                           "", BB
                         );
  puts_call->setTailCall(false);
}
