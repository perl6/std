// SYM_infix__S_Plus.cpp
#include "villCompiler.h"

Value *
SYM_infix__S_Plus( struct villCompiler * vill, struct graph_node * node ) {
  Value * result = NULL;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<integer>" );
  graph_node * decint = ast_mapentry( node, "decint" );
  assert( decint != NULL );
  function_pointer Codegen;
  Codegen = (function_pointer) decint -> data;
  assert( Codegen != NULL );
  result = Codegen( vill, decint );
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</integer>" );
  return result;
}
