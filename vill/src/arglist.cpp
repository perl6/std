// arglist.cpp
#include "villCompiler.h"  // graph_node Value villCompiler

// An arglist occurs in an args node, and is a mapping containing the
// following keys/values:
// .: a term__S_value
// EXPR: an alias for the :. term__S_value
Value *
arglist( struct villCompiler * vill, struct graph_node * node ) {
  Value * result = NULL;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<arglist>" );
  graph_node * expr = ast_mapentry( node, "EXPR" );
  function_pointer Codegen;
  Codegen = (function_pointer) expr -> data;
  assert( Codegen != NULL );
  result = Codegen( vill, expr );
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</arglist>" );
  return result;
}
