/* graph_traverse_begin.c */

/* Create the controller (aka spider) that manages a graph traversal */

#include <stdlib.h>          /* malloc */
#include "graph_traverse.h"  /* graph_traverse_struct */

struct graph_traverse_struct *
graph_traverse_begin( struct graph_node * graph ) {
  struct graph_traverse_struct * spider=(struct graph_traverse_struct *)
    malloc( sizeof(struct graph_traverse_struct) );
  spider -> graph  = graph;
  /* advice: do not init the spider here, do it in graph_traverse */
  spider -> head   = NULL;
  spider -> tail   = NULL;
  spider -> spidey = NULL;
  return spider;
}
