/* graph_traverse.h */

#include "yaml_compose_internal.h"  /* should be renamed */
  /* graph_node node_stack_entry */

/* Define functions and data structures to visit all nodes in a */
/* directed graph in depth first order. */

/* Message informing the caller about the next node (aka spidey) */
struct graph_traverse_event {
  struct graph_node * node;
  struct graph_node * parentnode;
  char * key;         /* null if in a sequence, not null in a mapping */
  int map_nest_count;
  int seq_nest_count;
};

/* the Traversal Controller (aka spider) */
struct graph_traverse_struct {
  struct graph_node * graph;
  struct node_stack_entry * head;
  struct node_stack_entry * tail;
  struct graph_traverse_event * spidey;
};

/* Move to the next node in the graph, depth first */
struct graph_traverse_event *
  graph_traverse( struct graph_traverse_struct * );

/* Construct a graph_traverse_struct */
struct graph_traverse_struct *
  graph_traverse_begin( struct graph_node * );

/* Destroy a graph_traverse_struct */
void
  graph_traverse_end( struct graph_traverse_struct * );
