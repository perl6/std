// scoped.cpp
#include "villCompiler.h"

Value *
scoped( struct villCompiler * vill, struct graph_node * node ) {
  Value * result = NULL;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<scoped>" );
  graph_node * declarator;
  declarator = ast_mapentry( node, "declarator" );
  assert( declarator != NULL );
  function_pointer Codegen;
  Codegen = (function_pointer) declarator -> data;
  assert( Codegen != NULL );
  result = Codegen( vill, declarator );
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</scoped>" );
  return result;
}
