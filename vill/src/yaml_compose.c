/* yaml_compose.c */
#include <assert.h>                /* assert */
#include <stdlib.h>                /* abort atoi free malloc */
#include "yaml_compose_internal.h" /* node_stack_entry yaml_compose */
  /* yaml_compose_mapping yaml_compose_scalar yaml_compose_sequence */
#include "yaml_parse.h"            /* event_type */
  /* yaml_event_type YAML_EVENT_* */

int yaml_compose_debug_flag; /* -Dp enables printing during composing */
int yaml_compose_debug_verbose; /* -Dpv prints more */

/* Add a new node to the graph insertion point stack, returning a */
/* pointer to the new entry. */
struct node_stack_entry *
local_push( struct node_stack_entry * top_entry, struct graph_node *
    new_top_node, struct mapping_list_entry * mapping, struct
    sequence_list_entry * sequence ) {
  /* A node stack entry has node and prev members. */
  struct node_stack_entry * new_stack_entry;
  new_stack_entry = (struct node_stack_entry *)
    malloc( sizeof(struct node_stack_entry) );
  assert( new_stack_entry != NULL );
  new_stack_entry -> node      = new_top_node;
  /* The mapping and sequence fields were added for compose_alias. */
  new_stack_entry -> map_entry = mapping;
  new_stack_entry -> seq_entry = sequence;
  new_stack_entry -> prev      = top_entry;
  return new_stack_entry;
}

/* Remove count entries from the graph insertion point stack, */
/* returning a pointer the top of the remaining stack. */
struct node_stack_entry *
local_pop( struct node_stack_entry * top_entry, int count ) {
  struct node_stack_entry * popped_entry;
  while ( count-- ) {
    assert( top_entry != NULL );
    popped_entry = top_entry;
    assert( top_entry -> prev != NULL );
    top_entry = top_entry -> prev;
    free( popped_entry ); /* TODO: check */
    /* But do not free the node that popped_entry was pointing to. */
  }
  return top_entry;
}

struct anchor_list_struct anchor_list = { NULL, NULL };

/* Build an entire graph of nodes from a series of parsed YAML events */
struct graph_node *
yaml_compose( FILE * stream ) {
  int stack_levels = 0;
  int need_space   = 0;
  /* create a root node for the graph */
  struct graph_node * graph = (struct graph_node *)
    malloc( sizeof(struct graph_node) );
  assert( graph != NULL );
  graph -> flags    = YAML_MAPPING;
  graph -> type_tag = NULL;
  graph -> data     = NULL;
  /* Create the nesting stack to track the current graph insertion */
  /* point. */
  struct node_stack_entry * top = (struct node_stack_entry *)
    malloc( sizeof(struct node_stack_entry) );
  assert( top != NULL );
  top -> node = graph;
  top -> prev = NULL; /* Sentinel value: the root node has no parent. */
  enum yaml_event_type event_type;
  /* Pull events one at a time from the YAML stream parser. */
  while ( (event_type=yaml_parse(stream)) != YAML_EVENT_FILE_END ) {
    /* debug trace of each parser event activated by -Dpv */
    yaml_compose_debug_flag && yaml_compose_debug_verbose &&
      fprintf( stderr, "[%d,%d,%d,%d,%d]", yaml_event.seq_levels,
      yaml_event.seq_change, yaml_event.map_levels,
      yaml_event.map_change, stack_levels );

    struct sequence_list_entry * flow_seq_entry;
    struct mapping_list_entry  * flow_map_entry;

    switch ( event_type ) {
      case YAML_EVENT_ALIAS:
        yaml_compose_debug_flag && fprintf( stderr, " *%.*s",
          (int)yaml_event.len, yaml_event.str );
        top = yaml_compose_alias( top, yaml_event.str, yaml_event.len );
        --stack_levels;
        break;
      case YAML_EVENT_ANCHOR:
        yaml_compose_debug_flag && fprintf( stderr, " &%.*s",
          (int)yaml_event.len, yaml_event.str );
        yaml_compose_anchor( top, yaml_event.str, yaml_event.len );
        break;
      case YAML_EVENT_END_FLOW_MAPPING:
        top = local_pop( top, 1 );
        --stack_levels;
        yaml_compose_debug_flag && fprintf( stderr, "}" );
        break;
      case YAML_EVENT_END_FLOW_SEQUENCE:
        top = local_pop( top, 1 );
        --stack_levels;
        yaml_compose_debug_flag && fprintf( stderr, "]" );
        break;
      case YAML_EVENT_MAPPING_KEY:
        yaml_compose_debug_flag && fprintf( stderr, "\n%*s%.*s:",
          (yaml_event.seq_levels + yaml_event.map_levels - 1) * 2, "",
          (int)yaml_event.len, yaml_event.str );
        /* Compare the levels in the event with the stack depth */
        if ( yaml_event.seq_levels + yaml_event.map_levels <=
            stack_levels ) {
          int pop_count = stack_levels - yaml_event.seq_levels -
            yaml_event.map_levels + 1;
          /* TODO: eliminate the cases of pop_count==0 that run */
          /*       through here. */
          top = local_pop( top, pop_count );
          stack_levels -= pop_count;
        }
        /* push the new node onto the nesting stack */
        struct mapping_list_entry * map_entry;
        map_entry = yaml_compose_mapping(
          top -> node, yaml_event.str, yaml_event.len );
        top = local_push( top, map_entry -> node, map_entry, NULL );
        ++stack_levels;
        break;
      case YAML_EVENT_SCALAR_BARE:
      case YAML_EVENT_SCALAR_QUOTED:
        assert( need_space );
        yaml_compose_debug_flag && need_space && fprintf(stderr," ");
        yaml_compose_debug_flag && fprintf( stderr,
          event_type == YAML_EVENT_SCALAR_BARE ? "%.*s" : "'%.*s'",
          (int) yaml_event.len, yaml_event.str );
        yaml_compose_scalar( top -> node, yaml_event.str,
          yaml_event.len );
        top = local_pop( top, 1 );
        --stack_levels;
        break;
      case YAML_EVENT_SCALAR_CONTINUED:
        /* TODO */
        break;
      case YAML_EVENT_SEQUENCE_ENTRY:
        yaml_compose_debug_flag && need_space && fprintf(stderr," ");
        yaml_compose_debug_flag && fprintf( stderr, "\n%*s-",
          (yaml_event.seq_levels + yaml_event.map_levels - 1) * 2, "" );
        need_space = 1;
        /* Compare the levels in the event with the stack depth */
        if ( yaml_event.seq_levels + yaml_event.map_levels <=
            stack_levels ) {
          int pop_count = stack_levels - yaml_event.seq_levels -
            yaml_event.map_levels + 1;
          /* TODO: eliminate the cases of pop_count==0 that run */
          /*       through here */
          top = local_pop( top, pop_count);
          stack_levels -= pop_count;
        }
        /* push the new node onto the nesting stack */
        struct sequence_list_entry * seq_entry;
        seq_entry = yaml_compose_sequence( top -> node );
        top = local_push( top, seq_entry -> node, NULL, seq_entry );
        ++stack_levels;
        break;
      case YAML_EVENT_START_FLOW_MAPPING:
        flow_map_entry = yaml_compose_mapping( top -> node,
          yaml_event.str, yaml_event.len );
        top = local_push( top, flow_map_entry -> node, flow_map_entry,
          NULL );
        yaml_compose_debug_flag && fprintf( stderr, " {" );
        ++stack_levels;
        break;
      case YAML_EVENT_START_FLOW_SEQUENCE:
        flow_seq_entry = yaml_compose_sequence( top -> node );
        top = local_push( top, flow_seq_entry -> node, NULL,
          flow_seq_entry );
        yaml_compose_debug_flag && fprintf( stderr, " [" );
        ++stack_levels;
        break;
      case YAML_EVENT_TAG:
        /* Look for a possible corruption of the AST that might be */
        /* caused by bugs in yaml_parse() or this yaml_compose(). */
        if ( top -> node -> type_tag != NULL ) {
          fprintf( stderr,
            "\n[Fatal: type tag conflict on node]\n"
            "old = !%s\nnew = !%.*s\n",
            top -> node -> type_tag,
            (int)yaml_event.len, yaml_event.str
          );
          abort();
        }
        yaml_compose_tag( top -> node, yaml_event.str, yaml_event.len );
        yaml_compose_debug_flag && need_space && fprintf(stderr," ");
        yaml_compose_debug_flag && fprintf( stderr, "!%.*s",
          (int)yaml_event.len, yaml_event.str );
        break;
      case YAML_EVENT_DIRECTIVES_END:
        yaml_compose_debug_flag && fprintf( stderr, "---" );
        need_space = 1;
        break;
      default:
        fprintf( stderr, "UNKNOWN EVENT %d\n", event_type );
        abort(); /* should never occur */
    }
  }
  yaml_compose_debug_flag && fprintf( stderr, "\n" );
  return graph;
}
