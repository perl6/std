// decint.cpp
#include "villCompiler.h"

Value *
decint( struct villCompiler * vill, struct graph_node * node ) {
  Value * result = NULL;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<decint>" );
  graph_node * text = ast_mapentry( node, "TEXT" );
  assert( text != NULL );
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "%s",
    text -> content.scalar.text );
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</decint>" );
  return result;
}
