// statement.cpp
#include "villCompiler.h"

Value *
statement(struct villCompiler * vill, struct graph_node * node) {
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<statement>" );
  // The statement is a mapping containing these keys and values:
  // .               a noun__S_term mapping node
  // BEG             the beginning source code offset
  // END             the end source code offset
  // EXPR            alias of the end source code offset
  // eat_terminator  an optional sequence of space and ';' terminators
  // statementlist   an alias for the .: of statementlist
  Value * result = NULL;

  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</statement>" );
  return result;
}
