// quote__S_Double_Double.cpp
#include "villCompiler.h"

Value *
quote__S_Double_Double( struct villCompiler * vill, struct graph_node * node ) {
  Value * result = NULL;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<quote__S_Double_Double>" );
  graph_node * nibble = ast_mapentry( node, "nibble" );
  function_pointer Codegen;
  Codegen = (function_pointer) nibble -> data;
  assert( Codegen != NULL );
  result = Codegen( vill, nibble );
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</quote__S_Double_Double>" );
  return result;
}
