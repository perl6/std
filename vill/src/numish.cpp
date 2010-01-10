// numish.cpp
#include "villCompiler.h"

Value *
numish( struct villCompiler * vill, struct graph_node * node ) {
  Value * result = NULL;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<numish>" );
  graph_node * integer = ast_mapentry( node, "integer" );
  assert( integer != NULL );
  function_pointer Codegen;
  Codegen = (function_pointer) integer -> data;
  assert( Codegen != NULL );
  result = Codegen( vill, integer );
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</numish>" );
  return result;
}
