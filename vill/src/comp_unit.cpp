// comp_unit.cpp
#include <assert.h>        // assert
#include "villCompiler.h"  // graph_node

// comp_unit is the outermost container defined in STD.pm
Value *
comp_unit( struct villCompiler * vill, struct graph_node * node ) {
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<comp_unit>" );
  // The comp_unit is a mapping containing these keys and values:
  // .              either a statementlist node or an anonymous list
  //                  containing a statementlist and a comment
  // BEG            the beginning source code offset
  // END            the end source code offset
  // statementlist  an alias for the . node of comp_unit
  // comment        (optional) alias for a comment node
  //
  // Find the statementlist node and call its *Codegen method, or abort.
//function_pointer Codegen = (function_pointer) node -> data;
//graph_node * statementlist = ast_mapentry( node, "." );
  graph_node * statementlist = ast_mapentry( node, "statementlist" );
  function_pointer Codegen = (function_pointer) statementlist -> data;
  assert( Codegen != NULL );
  Value * result = Codegen( vill, statementlist );
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</comp_unit>" );
  return result;
}
