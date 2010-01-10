// args.cpp
#include "villCompiler.h"

// An args AST node occurs in a sequence, after an identifier, within a
// term__S_identifier.  It contains all the arguments for a function.
// It consists of a mapping with the following keys/values:
// .: an arglist node containing a sequence (of the arguments)
// arglist: an alias for the .: arglist node
// semiarglist: (optional) an alias for the .: semiarglist node
// invocant: a scalar

Value *
args( struct villCompiler * vill, struct graph_node * node ) {
  Value * result = NULL;
  graph_node * semiarglist, * arglist_sequence, * arglist;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<args>" );
  semiarglist = ast_mapentry( node, "semiarglist" );
  // Some args contain a semiarglist mapping node, eg print("ABC") while
  // others do not, eg print "ABC".  All have an arglist sequence node,
  // but if the semiarglist exists, the arglist is 
  if ( semiarglist != NULL ) {
    function_pointer Codegen;
    Codegen = (function_pointer) semiarglist -> data;
    assert( Codegen != NULL );
    result = Codegen( vill, semiarglist );
  }
  else {
    arglist_sequence = ast_mapentry( node, "arglist" );
    assert( arglist_sequence != NULL );
    int index = 0;
    while ( (arglist = ast_seqentry( arglist_sequence, index++ ) ) != NULL ) {
      function_pointer Codegen;
      Codegen = (function_pointer) arglist -> data;
      assert( Codegen != NULL );
      result = Codegen( vill, arglist );
    }
  }
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</args>" );
  return result;
}
