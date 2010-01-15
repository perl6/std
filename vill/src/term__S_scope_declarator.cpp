// term__S_scope_declarator.cpp
#include "villCompiler.h"

Value *
term__S_scope_declarator( struct villCompiler * vill, struct graph_node * node ) {
  Value * result = NULL;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<term__S_scope_declarator>" );
  graph_node * scope_declarator;
  scope_declarator = ast_mapentry( node, "scope_declarator" );
  assert( scope_declarator != NULL );
  function_pointer Codegen;
  Codegen = (function_pointer) scope_declarator -> data;
  assert( Codegen != NULL );
  result = Codegen( vill, scope_declarator );
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</term__S_scope_declarator>" );
  return result;
}
