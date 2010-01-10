// term__S_value.cpp
#include "villCompiler.h"

Value *
term__S_value( struct villCompiler * vill, struct graph_node * node ) {
  Value * result = NULL;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<term__S_value>" );
  graph_node * value = ast_mapentry( node, "value" );
  assert( value != NULL );
  function_pointer Codegen;
  Codegen = (function_pointer) value -> data;
  assert( Codegen != NULL );
  result = Codegen( vill, value );
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</term__S_value>" );
  return result;
}
