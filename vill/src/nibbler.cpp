// nibbler.cpp
#include "villCompiler.h"

Value *
nibbler( struct villCompiler * vill, struct graph_node * node ) {
  Value * result = NULL;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<nibbler>" );
  graph_node * str;
  graph_node * nibble_sequence = ast_mapentry( node, "." );
  assert( nibble_sequence != NULL );
  int index = 0;
  while ( (str = ast_seqentry( nibble_sequence, index++ )) != NULL ) {
    assert( ast_type_tag_equals( str, "!perl/hash:VAST::Str" ) );
    function_pointer Codegen = (function_pointer) str -> data;
    assert( Codegen != NULL );
    result = Codegen( vill, str );
  }
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</nibbler>" );
  return result;
}
