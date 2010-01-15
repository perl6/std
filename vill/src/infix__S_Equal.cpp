// infix__S_Equal.cpp
#include "villCompiler.h"

// infix__S_Equal
Value *
infix__S_Equal( struct villCompiler * vill, struct graph_node * node ) {
  Value * result = NULL;
  function_pointer Codegen;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<infix__S_Equal>" );
  graph_node * args;
  args = ast_mapentry( node, "args" );
  assert( args != NULL );
  // The first argument is the left hand side of an assignment statement
  graph_node * scope_declarator;
  scope_declarator = ast_seqentry( args, 0 );
  assert( scope_declarator != NULL );
  // Do this extra validation to stop processing from handling different
  // source code that requires other generated code.
  assert( ast_type_tag_equals( scope_declarator,
    "!perl/hash:VAST::term__S_scope_declarator" ) );
  Codegen = (function_pointer) scope_declarator -> data;
  assert( Codegen != NULL );
  Value * scope_declarator_result;
  scope_declarator_result = Codegen( vill, scope_declarator );
  // The second argument is the right hand side of an assignment
  graph_node * value;
  value = ast_seqentry( args, 1 );
  assert( value != NULL );
  // Verify that the value node type tag is as expected, because it came
  // out of a sequence
  assert( ast_type_tag_equals( value,
    "!perl/hash:VAST::term__S_value" ) );
  Codegen = (function_pointer) value -> data;
  assert( Codegen != NULL );
  Value * value_result;
  value_result = Codegen( vill, value );
  // TODO: the actual assignment from right hand side to left hand side!
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</infix__S_Equal>" );
  return result;
}
