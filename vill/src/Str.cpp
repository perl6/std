// Str.cpp
#include "villCompiler.h"

Value *
Str( struct villCompiler * vill, struct graph_node * node ) {
  Value * result = NULL;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<Str>" );
  graph_node * text = ast_mapentry( node, "TEXT" );
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "%s", text -> content.scalar.text );
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</Str>" );
  return result;
}
