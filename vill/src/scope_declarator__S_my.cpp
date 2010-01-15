// scope_declarator__S_my.cpp
#include "villCompiler.h"

Value *
scope_declarator__S_my( struct villCompiler * vill, struct graph_node * node ) {
  Value * result = NULL;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<scope_declarator__S_my>" );
  graph_node * scoped;
  scoped = ast_mapentry( node, "scoped" );
  assert( scoped != NULL );
  function_pointer Codegen;
  Codegen = (function_pointer) scoped -> data;
  assert( Codegen != NULL );
  result = Codegen( vill, scoped );
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</scope_declarator__S_my>" );
  return result;
}
