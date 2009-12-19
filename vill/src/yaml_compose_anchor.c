/* yaml_compose_anchor.c */
#include <assert.h>
#include <stdlib.h>                /* malloc */
#include "yaml_compose_internal.h" /* graph_node */

#ifdef YAML_ANCHORS_ARE_NUMERIC
#define local_make_new_entry(str,len)  new_entry -> anchor = atoi( str )
#else
#define local_make_new_entry(str,len)  /* TODO */
#endif

void
yaml_compose_anchor( struct node_stack_entry * top, const char * str,
    size_t len  ) {
  /* verify the the anchored flag is not yet set, then set it */
  assert( (top -> node -> flags & YAML_ANCHORED) == 0 );
  top -> node -> flags |= YAML_ANCHORED;
  struct anchor_list_entry * new_entry = (struct anchor_list_entry *)
    malloc( sizeof(struct anchor_list_entry) );
  assert( new_entry != NULL );
  new_entry -> node         = top -> node;
  new_entry -> anchor_shown = 0;
  /* point the graph node to the anchor entry */
//top -> node -> data = new_entry;
  /* put the anchor's value (integer or string) into the entry */
  local_make_new_entry(str,len);
  /* add the new entry to the tail of the anchors list */
  if ( anchor_list.head == NULL ) {
    /* the first entry in a new list */
    assert( anchor_list.tail == NULL );
    anchor_list.head = new_entry;
    anchor_list.tail = new_entry;
  }
  else {
    /* append to the end of an existing list */
    anchor_list.tail -> next = new_entry;
  }
  anchor_list.tail = new_entry;
  new_entry -> next = NULL;
}
