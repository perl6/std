/* ast_seqentry.c */
#include <assert.h>        /* assert */
#include "yaml_compose.h"  /* graph_node SEQUENCE sequence_entry */
  /* sequence_list_entry */

/* sequence_entry receives a sequence parent node pointer and a */
/* number as parameters, and returns a pointer to the child node */
/* indexed by the number.  If the number is invalid, return NULL. */
struct graph_node *
ast_seqentry( struct graph_node * parent, int index ) {
  int i;
  static int previous_index;
  static struct sequence_list_entry * previous_entry  = NULL;
  static struct graph_node          * previous_parent = NULL;
  assert( (parent -> flags & YAML_KIND) == YAML_SEQUENCE );
  struct sequence_list_entry * entry= parent -> content.sequence.head;
  assert( entry != NULL );
  struct graph_node * child = NULL;  /* default result for bad index */
  if ( parent == previous_parent && index == previous_index + 1
      && previous_entry != NULL ) {
    /* Optimization for the most common case of getting successive */
    /* entries from the same sequence. */
    /* Warning: this is not thread safe or reentrant - static vars! */
    entry = previous_entry -> next;
  }
  else {
    /* Non optimized general case, get any entry from any sequence */
    /* Walk the linked list of sequence entries; may loop 0 times. */
    /* Admittedly the algorithm is simple and suboptimal, refactoring */
    /* should begin with a more efficient sequence data structure. */
    for ( i=0; i<index; ++i ) {
      if ( entry -> next != NULL ) {
        entry = entry -> next;  /* keep walking ... */
      }
      else {
        /* oops, the list ran out before reaching the desired index */
        entry = NULL;
      }
    }
  }
  if ( entry != NULL ) {
    child = entry -> node;
  }
  previous_parent = parent;
  previous_entry  = entry;
  previous_index  = index;
  return child;
}
