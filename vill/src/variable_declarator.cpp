// variable_declarator.cpp
#include "villCompiler.h"

Value *
variable_declarator( struct villCompiler * vill, struct graph_node * node ) {
  Value * result = NULL;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<variable_declarator>" );
  graph_node * variable;
  variable = ast_mapentry( node, "variable" );
  assert( variable != NULL );
  function_pointer Codegen;
  Codegen = (function_pointer) variable -> data;
  assert( Codegen != NULL );
  result = Codegen( vill, variable );
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</variable_declarator>" );
  return result;
}
