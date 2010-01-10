// nibbler.cpp
#include "villCompiler.h"

Value *
nibbler( struct villCompiler * vill, struct graph_node * node ) {
  Value * result = NULL;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<nibbler>" );
  graph_node * str;
  graph_node * nibble_sequence = ast_mapentry( node, "." );
  assert( nibble_sequence != NULL );
  int index = 0;
  while ( (str = ast_seqentry( nibble_sequence, index++ )) != NULL ) {
    if ( ast_type_tag_equals( str, "!perl/hash:VAST::Str" ) ) {
      function_pointer Codegen = (function_pointer) str -> data;
      assert( Codegen != NULL );
      result = Codegen( vill, str );
    }
    else {
      assert( ast_type_tag_equals( str, "!perl/hash:VAST::escape__S_Back" ) );
      // In most Codegen functions (eg term__S_value), this point in the
      // code would use the type tag of the child node
      // (eg backslash__S_n) to call that codegen function.  It does not
      // seem worth compiling almost identical functions for "\n", "\t"
      // etc that differ by only one character.
      struct graph_node * backslash_node;
      // Do not jump via a function pointer to another Codegen function,
      // rather drill directly down the AST and pick out the letter
      // after the backslash.
      backslash_node = ast_mapentry( str, "item" );
      assert( backslash_node != NULL );
      struct graph_node * text_node;
      text_node = ast_mapentry( backslash_node, "TEXT" );
      assert( text_node != NULL );
      vill -> debug_flags & DEBUG_USER && fprintf( stderr,
        "<backslash>%s</backslash>", text_node -> content.scalar.text );
    }
  }
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</nibbler>" );
  return result;
}
