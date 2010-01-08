// identifier.cpp
#include "villCompiler.h"

// An identifier is a mapping with the following keys/values:
// BEG:
// END:
// TEXT: a scalar containing the identifier name
Value *
identifier( struct villCompiler * vill, struct graph_node * node ) {
  Value * result = NULL;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<identifier>" );
  graph_node * text = ast_mapentry( node, "TEXT" );
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "%s", text -> content.scalar.text );
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</identifier>" );
  return result;
}
