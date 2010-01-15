// declarator.cpp
#include "villCompiler.h"

Value *
declarator( struct villCompiler * vill, struct graph_node * node ) {
  Value * result = NULL;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<declarator>" );
  graph_node * variable_declarator;
  variable_declarator = ast_mapentry( node, "variable_declarator" );
  assert( variable_declarator != NULL );
  function_pointer Codegen;
  Codegen = (function_pointer) variable_declarator -> data;
  assert( Codegen != NULL );
  result = Codegen( vill, variable_declarator );
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</declarator>" );
  return result;
}
