// codegen.cpp
// convert from Abstract Syntax Tree to Intermediate Representation
#include <assert.h>             // assert
#include "villCompiler.h"       // villCompiler and core of LLVM
#include "llvm/Instructions.h"  // BinaryOperator ConstantInt FunctionType ReturnInst

void
villCompiler::codegen() {
  // Create the main function: first create the type 'int ()'
  FunctionType *FT =
    FunctionType::get(Type::getInt32Ty(villContext), /*not vararg*/false);

  // By passing a module as the last parameter to the Function constructor,
  // it automatically gets appended to the Module.
  Function *F = Function::Create(FT, Function::ExternalLinkage, "main", villModule);

  // Add a basic block to the function... again, it automatically inserts
  // because of the last argument.
  BasicBlock *BB = BasicBlock::Create(villContext, "EntryBlock", F);

  make_puts( BB, "vill compiled A" );

  // Get pointers to the constant integers...
  Value *Two   = ConstantInt::get(Type::getInt32Ty(villContext), 2);
  Value *Three = ConstantInt::get(Type::getInt32Ty(villContext), 3);

  // Create the add instruction... does not insert...
  Instruction *Add = BinaryOperator::Create(Instruction::Add, Two, Three,
                                            "addresult");

  // explicitly insert it into the basic block...
  BB->getInstList().push_back(Add);

  // start at the top of the Abstract Syntax Tree.
  //  Call the Perl 6 "action" handlers for the language
  // elements (comp_unit, statementlist etc) as they are encountered.
  assert( AST != NULL );
  assert( AST -> data != NULL );
  function_pointer Codegen = (function_pointer) AST -> data;
  Value * result = Codegen( this, AST );
  make_puts( BB, "vill compiled Z" );

  // Create the return instruction and add it to the basic block
  BB->getInstList().push_back(ReturnInst::Create(villContext, Add));
}
