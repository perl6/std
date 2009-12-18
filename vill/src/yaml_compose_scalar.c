/* yaml_compose_scalar.c */
#include <assert.h>                /* assert */
#include <string.h>                /* strncpy */
#include "yaml_compose_internal.h" /* graph_node YAML_SCALAR */

/* yaml_compose_sequence must: */
/* 1. verify that the parent node not yet assigned */
/* 2. make the parent node a scalar */
/* 3. copy the string to be the scalar content */
void
yaml_compose_scalar( struct graph_node * parent_node,
    const char * str, size_t len ) {
  assert( (parent_node -> flags & YAML_KIND) == 0 );
//assert( parent_node -> kind == NOT_YET_ASSIGNED );
  parent_node -> flags |= YAML_SCALAR;
//parent_node -> kind = SCALAR;
  parent_node -> content.scalar.text = (char *) malloc( len + 1 );
  assert( parent_node -> content.scalar.text != NULL );
  strncpy( parent_node -> content.scalar.text, str, len );
  /* strncpy does not always terminate the string, so do it here */
  parent_node -> content.scalar.text[len] = '\0';
}
