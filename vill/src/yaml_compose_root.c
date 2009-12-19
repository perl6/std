/* yaml_compose_root.c */
#include <assert.h>                 /* assert */
#include <stdlib.h>                 /* malloc */
#include "yaml_compose_internal.h"  /* graph_node */

struct graph_node *
yaml_compose_root() {
  struct graph_node * new_root_node;
  new_root_node = (struct graph_node *)
    malloc( sizeof(struct graph_node) );
  assert( new_root_node != NULL );
  new_root_node -> flags    = 0;
  new_root_node -> type_tag = NULL;
  new_root_node -> data     = NULL;
  return new_root_node;
}
