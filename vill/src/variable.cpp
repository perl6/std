// variable.cpp
#include "villCompiler.h"

Value *
variable( struct villCompiler * vill, struct graph_node * node ) {
  // Instead if recursing farther down the AST subtree via more type tag
  // specified code pointers, attempt to directly reach access the other
  // nodes describe the variable.  This also means the type tags nested
  // within the !perl/hash:VAST::variable node can have their *Codegen()
  // pointers set to NULL in link_codegen(), a small saving.
  // Effectively their handlers are manually inlined here instead :)
  Value * result = NULL;
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "<variable>" );
  // Find the sigil of the variable
  graph_node * sigil;
  sigil = ast_mapentry( node, "sigil" );
  assert( sigil != NULL );
  graph_node * sigil_text;
  // The sigil text is "$" for scalar, "@" for array, "%" for hash,
  // "&" for code
  sigil_text = ast_mapentry( sigil, "TEXT" );
  assert( sigil_text != NULL );
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "%s",
    sigil_text -> content.scalar.text );
  // Look for an optional twigil
  graph_node * twigil;
  twigil = ast_mapentry( node, "twigil" );
  assert( twigil != NULL );
  graph_node * twigil_entry;
  twigil_entry = ast_seqentry( twigil, 0 );
  assert( twigil_entry != NULL );
  // Find the name of the variable
  graph_node * desigilname;
  desigilname = ast_mapentry( node, "desigilname" );
  assert( desigilname != NULL );
  graph_node * longname;
  longname = ast_mapentry( desigilname, "longname" );
  assert( longname != NULL );
  graph_node * name;
  name = ast_mapentry( longname, "name" );
  assert( name != NULL );
  graph_node * identifier;
  identifier = ast_mapentry( name, "identifier" );
  assert( identifier != NULL );
  graph_node * identifier_text;
  identifier_text = ast_mapentry( identifier, "TEXT" );
  assert( identifier_text != NULL );
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "%s",
    identifier_text -> content.scalar.text );
  vill -> debug_flags & DEBUG_USER && fprintf( stderr, "</variable>" );
  return result;
}
