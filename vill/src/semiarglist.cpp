// semiarglist.cpp
#include "villCompiler.h"

Value *
semiarglist( struct villCompiler * vill, struct graph_node * node ) {
  Value * result = NULL;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<semiarglist>" );
  graph_node * arglist_sequence, * arglist;
  arglist_sequence = ast_mapentry( node, "arglist" );
  assert( arglist_sequence != NULL );
  int index = 0;
  while ( (arglist = ast_seqentry( arglist_sequence, index++ ) ) != NULL ) {
    function_pointer Codegen;
    Codegen = (function_pointer) arglist -> data;
    assert( Codegen != NULL );
    result = Codegen( vill, arglist );
  }
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</semiarglist>" );
  return result;
}
