/* yaml_compose_mapping.c */
#include <assert.h>                 /* assert */
#include <stdlib.h>                 /* malloc */
#include <string.h>                 /* strncpy */
#include "yaml_parse.h"             /* MAPPING NOT_YET_ASSIGNED */
#include "yaml_compose_internal.h"  /* graph_node mapping_list_entry */
                                    /* yaml_compose.h */

/* yaml_compose_mapping must: */
/* 1. ensure the parent node is of mapping kind */
/* 2. add a new entry to the parent node's list of mapping entries */
/* 3. copy the key string into the new mapping list entry */
/* 4. create an empty graph node as mapping list value */
/* 5. return the new mapping entry */
struct mapping_list_entry *
yaml_compose_mapping( struct graph_node * parent_node,
    const char * key_str, size_t key_len ) {
  /* 1. ensure that the parent node is of mapping kind */
  if ( (parent_node -> flags & YAML_KIND) == 0 ) {
//if ( parent_node -> kind == NOT_YET_ASSIGNED ) {
    parent_node -> flags               |= YAML_MAPPING;
//  parent_node -> kind                 = MAPPING;
    parent_node -> content.mapping.head = NULL;
    parent_node -> content.mapping.tail = NULL;
  }
  else {
    /* verify that subsequent keys are also in a mapping */
    assert( (parent_node -> flags & YAML_KIND) == YAML_MAPPING );
//  assert( parent_node -> kind == MAPPING );
  }
  /* 2. add a new entry to the parent node's list of mapping entries */
  struct mapping_list_entry * new_mapping_entry;
  new_mapping_entry = (struct mapping_list_entry *)
    malloc( sizeof(struct mapping_list_entry) );
  /* mapping_list_entry contains key node and next fields */
  assert( new_mapping_entry != NULL );
  if ( parent_node -> content.mapping.head == NULL ) {
    /* sanity check - no head implies no tail */
    assert( parent_node -> content.mapping.tail == NULL );
    /* in an empty list the entry also becomes the head */
    parent_node -> content.mapping.head = new_mapping_entry;
    /* the tail gets assigned immediately after the else block */
  }
  else {
    /* in a non empty list the existing tail must point to */
    /* the new entry */
    parent_node -> content.mapping.tail -> next = new_mapping_entry;
  }
  /* in all lists the new entry becomes the tail */
  parent_node -> content.mapping.tail = new_mapping_entry;
  /* and the tail is the end of the linked list */
  new_mapping_entry -> next = NULL;
  /* 3. copy mapping key from the event data to the new entry */
  new_mapping_entry -> key = (char *) malloc( key_len + 1 );
  assert( new_mapping_entry -> key != NULL );
  strncpy( new_mapping_entry -> key, key_str, key_len );
  /* strncpy does not always terminate the string, so do it here */
  new_mapping_entry -> key[key_len] = '\0';

  /* 4. create the new empty graph node as mapping entry value */
  struct graph_node * new_node = (struct graph_node *)
    malloc( sizeof(struct graph_node) );
  assert( new_node != NULL );
  new_node -> flags    = 0;
//new_node -> kind     = NOT_YET_ASSIGNED;
  new_node -> type_tag = NULL;
  new_node -> data     = NULL;
  new_mapping_entry -> node = new_node;
  /* 5. return the new empty graph node */
  return new_mapping_entry;
}
