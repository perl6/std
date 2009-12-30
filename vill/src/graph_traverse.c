/* graph_traverse.c */

/* Retrieve successive nodes, depth first from a connected graph */

#include <assert.h>          /* assert */
#include <stdlib.h>          /* malloc */
#include "graph_traverse.h"  /* graph_traverse_struct */

/* Add a new nesting level to the graph node stack */
struct node_stack_entry *
local_traverse_push( struct graph_traverse_struct * spider,
    struct graph_node * node ) {
  struct node_stack_entry * new_entry = (struct node_stack_entry *)
    malloc( sizeof(struct node_stack_entry) );
  new_entry -> prev      = spider -> tail; /* join the linked list */
  spider    -> tail      = new_entry;      /* that is used as a stack */
  new_entry -> node      = node;
  /* the caller determines whether it is a mapping or a sequence */
  new_entry -> map_entry = NULL;
  new_entry -> seq_entry = NULL;
  return new_entry;
}

/* Move back up the stack */
struct node_stack_entry *
local_traverse_pop( struct node_stack_entry * top ) {
  /* Pop and free node stack entry records until either the stack is */
  /* empty, or a mapping or sequence node is reached that has more */
  /* entries. */
  struct node_stack_entry * parent;
  /* Accumulate the various looping tests in a flag for more */
  /* convenient development and testing. */
  /* This could later be refactored to improve efficiency. */
  int pop_flag = 1;
  while ( pop_flag ) {
#if 0
    if ( pop_flag && ) { pop_flag = 0; }
    (top -> node -> flags & YAML_KIND) == YAML_SCALAR ||
    ((top -> node -> flags & YAML_KIND) == YAML_MAPPING && top -> prev -> node -> ) ||
    (top -> node -> flags & YAML_KIND) == YAML_SEQUENCE
#endif
    if ( pop_flag ) { top = top -> prev; } /* do the pop! */
//  if ( top == NULL ) {
      pop_flag = 0;
//  }
  }
  return top;
}

/* The graph_traverse() function returns the next graph node in depth */
/* first order.  The first time it is called, it returns the root */
/* node.  When it returns NULL, it has returned the last node. */
struct graph_traverse_event *
graph_traverse( struct graph_traverse_struct * spider ) {
  struct node_stack_entry * top  = spider -> tail;
  if ( top == NULL ) {
    /* start a new traversal at the graph root */
    top = (struct node_stack_entry *)
      malloc( sizeof(struct node_stack_entry) );
    top -> node      = spider -> graph;
    top -> map_entry = NULL; /* these fields are all empty, because */
    top -> seq_entry = NULL; /* the root node has no parent */
    top -> prev      = NULL;
    /* tell the spider about the stack */
    spider -> tail = spider -> head = top;
    /* 'spidey' is the little critter than crawls all over */
    /* your web, er, graph.  Thanks for the name, masak :) */
    /* You can have several spiders and spideys independently */
    /* traversing the same graph, and clone them for continuations. */
    spider -> spidey = (struct graph_traverse_event *)
      malloc( sizeof(struct graph_traverse_event) );
    spider -> spidey -> node           = spider -> graph;
    spider -> spidey -> parentnode     = NULL;
    spider -> spidey -> key            = "";
    spider -> spidey -> map_nest_count = 0;
    spider -> spidey -> seq_nest_count = 0;
  }
  else { /* top != NULL */
    /* continue an already active traversal */
    switch ( top -> node -> flags & YAML_KIND ) {
      case YAML_MAPPING:
        /* If the mapping is empty, ascend the stack to find */
        /* another node.  Otherwise descend to the first child */
        /* node in the mapping's (key => value) pair list. */
        if ( top -> node -> content.mapping.head == NULL ) {
          /* the mapping is empty, ascend via the stack */
          top = local_traverse_pop( top );
          spider -> spidey = NULL;
        }
        else {
          /* the mapping has entries, descend to the first child */
          struct graph_node * first_child;
          first_child = top -> node -> content.mapping.head -> node;
          spider -> spidey -> node = first_child;
          spider -> spidey -> parentnode = top -> node;
          top = local_traverse_push( spider, first_child );
          top -> map_entry = top -> prev -> node -> content.mapping.head;
          top -> seq_entry = NULL;
          spider -> spidey -> key  = top -> prev -> node -> content.mapping.head -> key;
          ++(spider -> spidey -> map_nest_count);
        }
        break;
      case YAML_SEQUENCE:
        /* when the current node is a sequence node, the next node is */
        /* the first sequence entry, unless the sequence is empty */
        if ( top -> node -> content.sequence.head == NULL ) {
          /* the sequence is empty, ascend via the stack */
          top = local_traverse_pop( top );
          spider -> spidey = NULL;
        }
        else { /* top -> node -> content.sequence.head != NULL */
          /* the sequence has entries, descend to the first child */
          struct graph_node * first_child;
          first_child = top -> node -> content.sequence.head -> node;
          spider -> spidey -> node = first_child;
          spider -> spidey -> parentnode = top -> node;
          top = local_traverse_push( spider, first_child );
          top -> map_entry = NULL;
          top -> seq_entry = top -> prev -> node -> content.sequence.head;
          spider -> spidey -> key  = NULL;
          ++(spider -> spidey -> seq_nest_count);
        }
        break;
      case YAML_SCALAR:
        /* when the previous node was a scalar, the next node is the */
        /* next child of the nearest parent that has more children */
        top = local_traverse_pop( top );
        spider -> spidey -> node = top -> node;
        spider -> spidey -> parentnode = top -> prev -> node;
        switch ( spider -> spidey -> parentnode -> flags & YAML_KIND ) {
          case YAML_MAPPING:
            assert( top -> map_entry -> next != NULL );
            top -> map_entry = top -> map_entry -> next;
            spider -> spidey -> node = top -> map_entry -> node;
            spider -> spidey -> key = top -> map_entry -> key;
            break;
          case YAML_SEQUENCE:
            assert( top -> seq_entry -> next != NULL );
            top -> seq_entry = top -> seq_entry -> next;
            spider -> spidey -> node = top -> seq_entry -> node;
            spider -> spidey -> key = NULL;
            break;
        }
        break;
      default:
        /* something wrong, die noisily */
        abort();
        break; /* unnecessary, but sensible not to leave out */
    }
  }
  return spider -> spidey;
}
