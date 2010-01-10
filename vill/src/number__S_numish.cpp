// number__S_numish.cpp
#include "villCompiler.h"

Value *
number__S_numish( struct villCompiler * vill, struct graph_node * node ) {
  Value * result = NULL;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<number__S_numish>" );
  graph_node * numish = ast_mapentry( node, "numish" );
  assert(numish != NULL );
  function_pointer Codegen;
  Codegen = (function_pointer) numish -> data;
  assert( Codegen != NULL );
  result = Codegen( vill, numish );
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</number__S_numish>" );
  return result;
}
