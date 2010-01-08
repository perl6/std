// statementlist.cpp
#include "villCompiler.h"

Value *
statementlist(struct villCompiler * vill, struct graph_node * node) {
  // The statementlist is a mapping containing these keys and values:
  // .    a sequence of statement nodes
  // BEG             the beginning source code offset
  // END             the end source code offset
  // WS
  // eat_terminator  an optional sequence of spaces and ';' terminators
  // statement       a sequence of aliases to the statement nodes, or
  //                 a mapping containing the key 'statement' and a
  //                 single statement node
  struct graph_node * statement, * statement_sequence;
  Value * result = NULL;
  if ( (node -> flags & YAML_KIND) == YAML_SEQUENCE ) {
    vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<statementlist seq>" );
    int index = 0;
    while ( (statement = ast_seqentry( node, index++ )) != NULL ) {
      function_pointer Codegen = (function_pointer) statement -> data;
      assert( Codegen != NULL );
      result = Codegen( vill, statement );
    }
    vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</statementlist seq>" );
  }
  else {
    assert( (node -> flags & YAML_KIND) == YAML_MAPPING );
    vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<statementlist map>" );
    // the statement: node omits the eat_terminator entries :)
    statement_sequence = ast_mapentry( node, "statement" );
    assert( statement_sequence != NULL );
    int index = 0;
    while ( (statement = ast_seqentry( statement_sequence, index++ )) != NULL ) {
      assert( ast_type_tag_equals( statement, "!perl/hash:VAST::statement" ) );
      function_pointer Codegen = (function_pointer) statement -> data;
      assert( Codegen != NULL );
      result = Codegen( vill, statement );
    }
    vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</statementlist map>" );
  }
  return result;
}
