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
    /* 'spidey' is the little critter that conveys messages from the */
    /* spider to the calling program.  It points to each node of the */
    /* web, er, graph, in turn, in depth first order. */
    /* Thanks for the identifier names, masak :) */
    /* You can have several spiders and their spideys concurrently */
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
    /* Continue an already active traversal. */
    /* The current node determines where to go next. */
    switch ( top -> node -> flags & YAML_KIND ) {
      case YAML_MAPPING:
        /* If the mapping is empty, ascend the stack to find */
        /* another node.  Otherwise descend to the first child */
        /* node in the mapping's (key => value) pair list. */
        if ( top -> node -> content.mapping.head == NULL ) {
          /* the mapping is empty, ascend via the stack */
          fprintf( stderr, "TODO 1\n" );
          abort();
          spider -> spidey = NULL;
        }
        else {
          /* the mapping has entries, descend to the first child */
          struct graph_node * first_child;
          first_child = top -> node -> content.mapping.head -> node;
          spider -> spidey -> node       = first_child;
          spider -> spidey -> parentnode = top -> node;
          top = local_traverse_push( spider, first_child );
          top -> map_entry = top -> prev -> node -> content.mapping.head;
          top -> seq_entry = NULL;
          spider -> spidey -> key  =
            top -> prev -> node -> content.mapping.head -> key;
          ++(spider -> spidey -> map_nest_count);
        }
        break;
      case YAML_SEQUENCE:
        /* when the current node is a sequence node, the next node is */
        /* the first sequence entry, unless the sequence is empty */
        if ( top -> node -> content.sequence.head == NULL ) {
          /* the sequence is empty, ascend via the stack */
          fprintf( stderr, "TODO 2\n" );
          abort();
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
        if ( (spider -> spidey -> parentnode -> flags & YAML_KIND) ==
            YAML_MAPPING && top -> map_entry -> next != NULL ) {
          /* Traverse horizontally to the next sibling in a mapping */
          top -> map_entry = top -> map_entry -> next;
          spider -> spidey -> node = top -> map_entry -> node;
          spider -> spidey -> key  = top -> map_entry -> key;
        }
        else {
          if ( (spider -> spidey -> parentnode -> flags & YAML_KIND) ==
              YAML_SEQUENCE && top -> seq_entry -> next != NULL ) {
            /* Traverse horizontally to the next sibling in a sequence */
            top -> seq_entry = top -> seq_entry -> next;
            spider -> spidey -> node = top -> seq_entry -> node;
            spider -> spidey -> key  = NULL;
          }
          else {
            /* The sequence or mapping has come to an end.  Ascend to */
            /* the nearest container node that has a sequence or a */
            /* mapping containing more entries.  If ascending reaches */
            /* the root of the graph, the traversal is complete */
            struct node_stack_entry * removed_entry;
            while (
              /* This is the hairiest leg of the spider, */
              /* so be careful when touching the parentheses! */
              spider -> tail -> prev != NULL &&
              ((spider -> tail -> node -> flags & YAML_KIND) ==
                YAML_MAPPING &&
                spider -> tail -> map_entry -> next == NULL
              ) &&
              ((spider -> tail -> node -> flags & YAML_KIND) ==
                YAML_SEQUENCE &&
                spider -> tail -> seq_entry -> next == NULL
              )
            ) {
              removed_entry = top;
              assert( spider -> tail -> prev );
              spider -> tail = spider -> tail -> prev;
              free( removed_entry );
            }
            if ( spider -> tail != NULL ) {
              /* the spider's tail points to a node that contains */
              /* another mapping entry or another sequence entry */
              if ( (spider -> tail -> node -> flags & YAML_KIND)
                  == YAML_MAPPING ) {
                assert( spider -> tail -> map_entry -> next );
                spider -> tail -> map_entry = spider -> tail -> map_entry -> next;
                spider -> tail -> node = spider -> tail -> map_entry -> node;
                spider -> spidey -> key = spider -> tail -> map_entry -> key;
              }
              else {
                if( (spider -> tail -> node -> flags & YAML_KIND)
                    == YAML_SEQUENCE ) {
                  assert( spider -> tail -> seq_entry -> next );
                  spider -> tail -> seq_entry = spider -> tail -> seq_entry -> next;
                  spider -> tail -> node = spider -> tail -> seq_entry -> node;
                }
                else {
                  assert( (spider -> tail -> node -> flags & YAML_KIND)
                    == YAML_SCALAR );
                  fprintf( stderr, "TODO 3\n" );
                  abort();
                }
              }
              top = spider -> tail;
              spider -> spidey -> node       = top -> node;
              spider -> spidey -> parentnode = top -> prev -> node;
//            spider -> spidey = NULL; /* TODO: backtracking */
            }
            else { /* spider -> tail == NULL */
              /* the traversal has finished */
              spider -> spidey = NULL;
            }
          }
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
