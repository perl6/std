/* yaml_compose.h */
#ifndef YAML_COMPOSE_H
#define YAML_COMPOSE_H
#include <stdio.h>  /* FILE */

/* Several other YAML parsing libraries (Syck, yaml-cpp, C JSON */
/* parser, Jaula) were tried.  They either did not give node by node */
/* access to the data or did not handle anchors and aliases :-( */

#ifdef __cplusplus
extern "C" {
#endif

/* The Abstract Syntax Tree contains three kinds of nodes: scalar, */
/* sequence and mapping.  One struct layout is used for all three, */
/* with a union containing one set of kind-specific fields at a time. */
struct graph_node; /* forward ref for use in struct definitions */

/* Scalar contains just a pointer to a C style text string */
struct scalar_struct { char * text; };

/* Sequence contains a single-linked list of AST node entries. */
/* A null value in the 'next' field terminates the list. */
struct sequence_list_entry {
  struct graph_node          * node;
  struct sequence_list_entry * next;
};
/* The head pointer is used when traversing the list sequentially. */
/* The tail pointer is used to append a new item to the list. */
struct sequence_struct {
  struct sequence_list_entry * head;
  struct sequence_list_entry * tail;
};

/* Mapping contains a single-linked list of entries each containing */
/* a pointer to a C style key string and a pointer to an AST node. */
/* A null value in the 'next' field terminates the list. */
struct mapping_list_entry {
  char                      * key;
  struct graph_node         * node;
  struct mapping_list_entry * next;
};
/* The head pointer is used when traversing the list sequentially. */
/* The tail pointer is used to append a new item to the list */
struct mapping_struct {
  struct mapping_list_entry * head;
  struct mapping_list_entry * tail;
};

//enum node_kind { NOT_YET_ASSIGNED, MAPPING, SCALAR, SEQUENCE };

/* The main graph node structure */
struct graph_node {
  union {
    struct   scalar_struct   scalar;
    struct sequence_struct sequence;
    struct  mapping_struct  mapping;
  } content;
  char * type_tag;      /* yaml type from tag string that follows a ! */
  void * data; /* hook for application use eg *Codegen() function ptr */
             /* and for anchor/alias linking and roundtrip generation */
  int flags; /* see bit values below.  TODO: specify actual # of bits */
  /* Obsoleted by flags: enum node_kind kind; */
  /* not_yet_assigned scalar sequence or mapping */
  /* Never change a 'kind' member that has already been assigned, */
  /* because that usually corrupts memory. */
  /* Always check that the 'kind' member is correctly assigned before */
  /* writing or reading the 'content' member, to be sure of using the */
  /* correct union member. */
};

/* data members */
extern int yaml_compose_debug_flag;
extern int yaml_compose_debug_verbose;

/* function prototypes */
struct graph_node * yaml_compose(FILE *);
void                yaml_decompose(struct graph_node *);
struct graph_node * ast_mapentry(struct graph_node *, const char *);
struct graph_node * ast_seqentry(struct graph_node *, int);
int                 ast_type_tag_equals(struct graph_node *, const char *);

/* Bit masks for the graph_node -> flags field */
#define YAML_MAPPING     0x1
#define YAML_SEQUENCE    0x2
#define YAML_SCALAR      0x4
#define YAML_KIND        0x7
#define YAML_ANCHORED    0x8
//#define YAML_TAGGED     0x10
/* Bits not defined here are available to be used by applications */

#ifdef __cplusplus
}
#endif
#endif
/* end of yaml_compose.h */
