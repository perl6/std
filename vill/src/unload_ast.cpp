// unload_ast.cpp
// This is WAY better than garbage collection!
#include "villCompiler.h"  // AST villCompiler
#include "yaml_compose.h"  // yaml_decompose

void
villCompiler::unload_ast() {
  yaml_decompose( AST );
}
