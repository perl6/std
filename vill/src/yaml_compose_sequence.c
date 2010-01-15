/* yaml_compose_sequence.c */
#include <assert.h>                 /* assert */
#include <stdlib.h>                 /* malloc */
#include "yaml_parse.h"             /* SEQUENCE NOT_YET_ASSIGNED */
#include "yaml_compose_internal.h"  /* graph_node */

/* Called from yaml_compose, yaml_compose_sequence must: */
/* 1. ensure the parent node is of sequence kind */
/* 2. add a new entry to the parent node's list of sequence entries */
/* 3. create an empty graph node as sequence list value */
/* 4. return the new empty graph node */
struct sequence_list_entry *
yaml_compose_sequence( struct graph_node * parent_node ) {
  /* 1. ensure that the parent node is of sequence kind */
  if ( (parent_node -> flags & YAML_KIND) == 0 ) {
    parent_node -> flags |= YAML_SEQUENCE;
    parent_node -> content.sequence.head = NULL;
    parent_node -> content.sequence.tail = NULL;
  }
  else {
    /* verify that subsequent keys are also in a sequence */
    assert( (parent_node -> flags & YAML_KIND) == YAML_SEQUENCE );
  }
  /* 2. add a new entry to the parent node's list of sequence entries */
  struct sequence_list_entry * new_sequence_entry;
  new_sequence_entry = (struct sequence_list_entry *)
    malloc( sizeof(struct sequence_list_entry) );
  assert( new_sequence_entry != NULL );
  if ( parent_node -> content.sequence.head == NULL ) {
    /* sanity check - no head implies no tail */
    assert( parent_node -> content.sequence.tail == NULL );
    /* in an empty list the entry also becomes the head */
    parent_node -> content.sequence.head = new_sequence_entry;
  }
  else {
    /* in a non empty list the existing tail must point to */
    /* the new entry */
    parent_node -> content.sequence.tail -> next = new_sequence_entry;
  }
  /* in all lists the new entry becomes the tail */
  parent_node -> content.sequence.tail = new_sequence_entry;
  /* and the tail is the end of the linked list */
  new_sequence_entry -> next = NULL;

  /* 3. create the new empty graph node as sequence entry value */
  new_sequence_entry -> node = (struct graph_node *)
    malloc( sizeof(struct graph_node) );
  new_sequence_entry -> node -> flags    = 0;
  new_sequence_entry -> node -> type_tag = NULL;
  new_sequence_entry -> node -> data     = NULL;
  /* 4. return the new sequence entry */
  return new_sequence_entry;
}
