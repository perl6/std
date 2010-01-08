// statement.cpp
#include "villCompiler.h"

// The statement is a mapping containing these keys and values:
// .               a term__S_identifier mapping node
// BEG             the beginning source code offset
// END             the end source code offset
// EXPR            alias of . which is a map with a type tag of
//                 !perl/hash:VAST::term__S_identifier

Value *
statement(struct villCompiler * vill, struct graph_node * node) {
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<statement>" );
  struct graph_node * node_expr;
  Value * result = NULL;
  // the expression node is the unit to which the statement modifiers
  // statement_mod_cond: and statement_mod_loop: can be applied.
  // TODO: generate code for conditional and looping modifiers that
  // implement eg say "ok" if $a>0;
  node_expr = ast_mapentry( node, "EXPR" );
  if ( node_expr != NULL ) {
    function_pointer Codegen = (function_pointer) node_expr -> data;
    assert( Codegen != NULL );
    result = Codegen( vill, node_expr );
  }
  else {
#if 0
    fprintf( stderr, "TODO statement !EXPR\n" );
#endif
  }
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</statement>" );
  return result;
}
