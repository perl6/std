/* graph_traverse_end.c */

/* Remove the structs used to traverse a graph */

#include <stdlib.h>          /* free */
#include "graph_traverse.h"  /* graph_traverse_struct */

void
graph_traverse_end( struct graph_traverse_struct * spider ) {
  if( spider -> spidey != NULL ) {
    free( spider -> spidey );
  }
  free( spider );
}
