// SYM_infix__S_Equal.cpp
#include "villCompiler.h"

// SYM_infix__S_Equal
Value *
SYM_infix__S_Equal( struct villCompiler * vill, struct graph_node * node ) {
  Value * result = NULL;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<SYM_infix__S_Equal>" );

  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</SYM_infix__S_Equal>" );
  return result;
}
