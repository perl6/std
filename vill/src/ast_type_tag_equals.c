/* ast_type_tag_equals.c */
#include <assert.h>        /* assert */
#include "yaml_compose.h"  /* graph_node type_tag strcmp */

/* Given an AST node (pointer) and a key string, find and return the */
/* corresponding child node (pointer). */
int
ast_type_tag_equals( struct graph_node * node, const char * str ) {
  assert( node -> type_tag != NULL );
  int result;
  result = (strcmp( node -> type_tag, str ) == 0 );
  return result;
}
