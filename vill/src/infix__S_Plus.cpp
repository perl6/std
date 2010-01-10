// infix__S_Plus.cpp
#include "villCompiler.h"

Value *
infix__S_Plus( struct villCompiler * vill, struct graph_node * node ) {
  Value * result = NULL;
  function_pointer Codegen;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<infix__S_Plus>" );
  struct graph_node * plus_args;
  plus_args = ast_mapentry( node, "args" );
  assert( plus_args != NULL );
  // Generate code for the term to the left of the plus
  struct graph_node * left;
  Value * left_result;
  left  = ast_seqentry( plus_args, 0 );
  assert( left != NULL );
  assert( ast_type_tag_equals( left, "!perl/hash:VAST::term__S_value" ) );
  Codegen = (function_pointer) left -> data;
  assert( Codegen != NULL );
  left_result = Codegen( vill, left );
  // Generate code for the term to the right of the plus
  struct graph_node * right;
  Value * right_result;
  right = ast_seqentry( plus_args, 1 );
  assert( right != NULL );
  assert( ast_type_tag_equals( right, "!perl/hash:VAST::term__S_value" ) );
  Codegen = (function_pointer) right -> data;
  assert( Codegen != NULL );
  right_result = Codegen( vill, right );
  // Verify that the operator is correct for this type tag selected Codegen function
  struct graph_node * infix;
  infix = ast_mapentry( node, "infix" );
  assert( infix != NULL );
  assert( ast_type_tag_equals( infix, "!perl/hash:VAST::infixish" ) );
  struct graph_node * sym_node;
  sym_node = ast_mapentry( infix, "infix" ); // yes, same key as before!
  assert( sym_node != NULL );
  assert( ast_type_tag_equals( sym_node, "!perl/hash:VAST::SYM_infix__S_Plus" ) );
  struct graph_node * sym_sym_node;
  sym_sym_node = ast_mapentry( sym_node, "SYM" );
  assert( sym_sym_node != NULL );
  assert( strcmp( sym_sym_node -> content.scalar.text, "+" ) == 0 );
  // Generate code to do the work of the operator, in this case Plus
  // TODO: yes, the actual work with left_result and right_result! ;)
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</infix__S_Plus>" );
  return result;
}
