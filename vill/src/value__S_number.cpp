// value__S_number.cpp
#include "villCompiler.h"

Value *
value__S_number( struct villCompiler * vill, struct graph_node * node ) {
  Value * result = NULL;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<value__S_number>" );
  graph_node * number = ast_mapentry( node, "number" );
  assert( number != NULL );
  function_pointer Codegen;
  Codegen = (function_pointer) number -> data;
  assert( Codegen != NULL );
  result = Codegen( vill, number );
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</value__S_number>" );
  return result;
}
