/* yaml_compose_alias.c */
#include <assert.h>
#include <stdlib.h>                /* malloc */
#include "yaml_compose_internal.h" /* graph_node */

/* Note: a more sophisticated form of anchor storage and searching */
/* can definitely improve the speed of this solution, but the */
/* difference in overall performance would probably be minimal. */

#ifdef YAML_ANCHORS_ARE_NUMERIC
#define local_make_sought(str,len) int sought = atoi(str)
#define local_anchor_cmp(entry,sought) (entry->anchor - sought)
#else
#define local_make_sought(str,len)       /* TODO */
#define local_anchor_cmp(entry,sought) ( /* TODO */)
#endif

struct node_stack_entry *
yaml_compose_alias( struct node_stack_entry * top, const char * str,
    size_t len ) {
  /* prepare to walk the single linked list */
  struct anchor_list_entry * anchor_search = anchor_list.head;
  local_make_sought( str, len );
  /* search the anchor list for a match */
  while ( anchor_search != NULL ) {
    /* walk the linked list of anchors, searching for a match */
    if ( local_anchor_cmp(anchor_search,sought) == 0 ) {
      /* Hooray! found the required anchor */
      /* What an alias means: the most recent NOT_YET_ASSIGNED node */
      /* is not what this node's parent is going to point to. */
      /* Instead, the parent will point to an existing node that is */
      /* labelled with the specified anchor. */
      assert( (top -> node -> flags & YAML_KIND) == 0 );
      /* (this assert could have been done before the while, but */
      /* this code is also executed only once per function call) */
      struct graph_node * redundant_node = top -> node;
      if ( (top -> prev -> node -> flags & YAML_KIND) == YAML_MAPPING ) {
        /* This alias appears after a 'key:' as a mapping entry */
        assert( top -> seq_entry == NULL ); /* stack integrity check */
        assert( top -> map_entry -> node == redundant_node );
        /*  */
        top -> map_entry -> node = anchor_search -> node;
      }
      else {
        /* This alias appears after a '-' as a sequence entry */
        assert( (top -> prev -> node -> flags & YAML_KIND) == YAML_SEQUENCE );
        assert( top -> map_entry == NULL ); /* stack integrity check */
        assert( top -> seq_entry -> node == redundant_node );
        top -> seq_entry -> node = anchor_search -> node;
      }
      free( redundant_node );  /* (the alias references another node) */
      anchor_search = NULL;    /* to exit the loop */
    }
    else {
      /* anchor not yet matched, keep looking */
      assert( anchor_search -> next != NULL );
      anchor_search = anchor_search -> next;
    }
  }
  return top -> prev;
}
