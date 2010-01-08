// value__S_quote.cpp
#include "villCompiler.h"

Value *
value__S_quote( struct villCompiler * vill, struct graph_node * node ) {
  Value * result = NULL;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<value__S_quote>" );
  graph_node * quote = ast_mapentry( node, "quote" );
  function_pointer Codegen;
  Codegen = (function_pointer) quote -> data;
  assert( Codegen != NULL );
  result = Codegen( vill, quote );
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</value__S_quote>" );
  return result;
}
