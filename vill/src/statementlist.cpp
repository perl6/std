// statementlist.cpp
#include "villCompiler.h"

Value *
statementlist(struct villCompiler * vill, struct graph_node * node) {
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<statementlist>" );
  // The statementlist is a mapping containing these keys and values:
  // .    a sequence of statement nodes
  // BEG             the beginning source code offset
  // END             the end source code offset
  // WS
  // eat_terminator  an optional sequence of spaces and ';' terminators
  // statement       a sequence of aliases to the statement nodes
  struct graph_node * statement;
  Value * result = NULL;
  if ( (node -> flags & YAML_KIND) == YAML_SEQUENCE ) {
//if ( node -> kind == SEQUENCE ) {
    int index = 0;
    while ( (statement = ast_seqentry( node, index++ )) != NULL ) {
      function_pointer Codegen = (function_pointer) statement -> data;
      assert( Codegen != NULL );
      result = Codegen( vill, statement );
    }
  }
  else {
    assert( (node -> flags & YAML_KIND) == YAML_MAPPING );
//  assert( node -> kind == MAPPING );
    statement = ast_mapentry( node, "statement" );
    assert( statement != NULL );
    function_pointer Codegen = (function_pointer) statement -> data;
    assert( Codegen != NULL );
    result = Codegen( vill, statement );
  }
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</statementlist>" );
  return result;
}
