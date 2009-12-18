/* yaml_compose_internal.h */
#ifndef YAML_COMPOSE_INTERNAL_H
#define YAML_COMPOSE_INTERNAL_H

#include "yaml_compose.h" /* graph_node */

#ifdef __cplusplus
extern "C" {
#endif

/* Anchors in viv output are numeric. This allows for simple, quick */
/* storage and lookup operations. */
/* Comment out the following #define to get the more generic but */
/* slower string of anchors, for more general YAML compatibility. */
#define YAML_ANCHORS_ARE_NUMERIC
/* TODO: */
/* For those of a tweaking inclination: the next two definitions */
/* govern how much memory is initially and then incrementally */
/* allocated on the heap to store numeric anchors. */
/* If the above YAML_ANCHORS_ARE_NUMERIC definition is commented out, */
/* the following two definitions are ignored. */
/* These settings are a typical speed versus memory tradeoff. */
/* Development and testing values are deliberately low (5,2) to */
/* exercise all the code. */
/* Production values should approximate the average number of anchors */
/* encountered in practice, and a coarse enough increment to avoid */
/* frequent reallocations.  The reallocation overheads of CPU time */
/* and heap fragmentation are system dependent. */
/* Suggested values for medium size (1000 statements) Perl 6 */
/* applications would be (1000,500). */
#define YAML_ANCHORS_NUMBER_INITIAL   5
#define YAML_ANCHORS_NUMBER_INCREMENT 2
#if 0
#endif

/* Anchors and aliases are handled internally by yaml_parse and */
/* yaml_compose.  Initially only the case of integer anchor */
/* strings is being handled, because that is what viv emits and they */
/* are faster/easier to deal with. */
struct anchor_list_entry {
  struct graph_node * node;
#ifdef YAML_ANCHORS_ARE_NUMERIC
  int anchor;
#else
  char * anchor;
#endif
  struct anchor_list_entry * next;
  /* The above is enough for composing, but in order to pass a */
  /* yaml_compose_roundtrip test requires a way to distinguish */
  /* between the anchor occurrence and the alias occurrences of */
  /* a node.  Generally a YAML Serialize operation does this, */
  /* but that would be overkill for this application. */
  /* Store an anchor_shown flag (initially zero, set once the &anchor */
  /* token has been output) so that subsequent references are output */
  /* merely as an alias. */
  int anchor_shown;
};

extern struct anchor_list_struct {
  struct anchor_list_entry * head;
  struct anchor_list_entry * tail;
} anchor_list;

/* The node stack is a list of pointers to nodes with back links used */
/* only to pop off the top stack item. */
struct node_stack_entry {
  struct graph_node          * node;
  /* the mapping and sequence fields were added for compose_alias */
  struct mapping_list_entry  * map_entry;
  struct sequence_list_entry * seq_entry;
  struct node_stack_entry    * prev;
};

struct node_stack_entry * yaml_compose_alias( struct node_stack_entry *,
  const char *, size_t );
void yaml_compose_anchor( struct node_stack_entry *, const char *,
  size_t );

struct mapping_list_entry * yaml_compose_mapping( struct graph_node *
  parent_node, const char * key_str, size_t key_len );

struct graph_node * yaml_compose_root();

void yaml_compose_scalar( struct graph_node * parent_node,
  const char * str, size_t len );

struct sequence_list_entry * yaml_compose_sequence( struct graph_node *
  parent_node );

void yaml_compose_tag( struct graph_node * parent_node,
  const char * str, size_t len );

#ifdef __cplusplus
}
#endif
#endif
/* end of yaml_compose_internal. */
