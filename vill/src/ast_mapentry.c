/* ast_mapentry.c */
#include <assert.h>        /* assert */
#include "yaml_compose.h"  /* graph_node MAPPING mapping_entry */
                           /* mapping_list_entry strcmp */

/* Given an AST node (pointer) and a key string, find and return the */
/* corresponding child node (pointer). */
struct graph_node *
ast_mapentry( struct graph_node * parent, const char * key ) {
  assert( (parent -> flags & YAML_KIND) == YAML_MAPPING );
  struct mapping_list_entry * search_entry;
  search_entry = parent -> content.mapping.head;
  struct graph_node * child = NULL;
  while ( search_entry != NULL ) {
    if ( strcmp( key, search_entry -> key ) == 0 ) {
      /* found the key, prepare to return */
      child = search_entry -> node;
      assert( child != NULL );  /* behind the key should be a value */
      search_entry = NULL;      /* quit the while loop */
    }
    else {
      search_entry = search_entry -> next;  /* proceed along the list */
    }
  }
  return child;
}
