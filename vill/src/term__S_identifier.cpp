// term__S_identifier.cpp
#include "villCompiler.h"

// A term__S_identifier is a mapping with the following keys/values:
// .: a sequence of two nodes of type:
//   - identifier
//   - args
// identifier:
// args:
Value *
term__S_identifier( struct villCompiler * vill, struct graph_node * node ) {
  Value * result = NULL;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<term__S_identifier>" );
  graph_node * identifier = ast_mapentry( node, "identifier" );
  function_pointer Codegen;
  Codegen = (function_pointer) identifier -> data;
  assert( Codegen != NULL );
  result = Codegen( vill, identifier );
  graph_node * args = ast_mapentry( node, "args" );
  assert( args != NULL );
  Codegen = (function_pointer) args -> data;
  assert( Codegen != NULL );
  result = Codegen( vill, args );
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</term__S_identifier>" );
  return result;
}
