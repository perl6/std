/* yaml_compose_tag.c */
#include <assert.h>
#include <stdlib.h>                 /* malloc */
#include <string.h>                 /* strncpy */
#include "yaml_compose_internal.h"  /* graph_node */

void
yaml_compose_tag( struct graph_node * node, const char * tag_str,
    size_t tag_len ) {
  /* first ensure there is no existing tag that would be clobbered */
  assert( node -> type_tag == NULL );
  node -> type_tag = (char *) malloc( tag_len + 1 );
  assert( node -> type_tag != NULL );
  strncpy( node -> type_tag, tag_str, tag_len );
  /* strncpy does not always terminate the string, so do it here */
  node -> type_tag[tag_len] = '\0';
//node -> flags |= YAML_TAGGED;
}
