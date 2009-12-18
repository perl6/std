/* yaml_decompose.c */
/* An excuse to use a name like this should never be wasted. */
#include <assert.h>        /* assert */
#include <stdio.h>         /* temporary FILE */
#include <stdlib.h>        /* abort */
#include <string.h>        /* temporary strlen */
#include "yaml_compose.h"  /* graph_node */

void local_free_scalar(   FILE *, struct graph_node *);
void local_free_sequence( FILE *, struct graph_node *);
void local_free_mapping(  FILE *, struct graph_node *);

void
local_free_scalar( FILE * stream, struct graph_node * node ) {
  assert( (node -> flags & YAML_KIND) == YAML_SCALAR ); /* TODO: drop when stable */
//assert( node -> kind == SCALAR ); /* TODO: drop when stable */
//fprintf( stream, "free %s", node -> content.scalar.text);
  if ( node -> type_tag != NULL ) {
//  fprintf( stream, "\nfree scalar TYPE_TAG:%s", node -> type_tag );
    abort();
  }
  free( node -> content.scalar.text );
  free( node );
}

void
local_free_sequence( FILE * stream, struct graph_node * node ) {
  if ( node -> type_tag != NULL ) {
    fprintf( stream, "\nfree sequence TYPE_TAG:%s", node -> type_tag );
  }
  struct sequence_list_entry * sequence_entry, * previous_entry;
  sequence_entry = node -> content.sequence.head;
  while ( sequence_entry != NULL ) {
    fprintf( stream, "\nfree -" );
    switch ( sequence_entry -> node -> flags & YAML_KIND ) {
//  switch ( sequence_entry -> node -> kind ) {
      case YAML_SCALAR:
        local_free_scalar(   stream, sequence_entry -> node );
        break;
      case YAML_SEQUENCE:
        local_free_sequence( stream, sequence_entry -> node );
        break;
      case YAML_MAPPING:
        local_free_mapping(  stream, sequence_entry -> node );
        break;
      case 0:
//    case NOT_YET_ASSIGNED:
        fprintf( stream, "\nNOT YET ASSIGNED" );
        break;
      default:
        fprintf( stream, "\nSEQUENCE_ENTRY_BAD_KIND" );
        abort();
        break;
    }
//  free( sequence_entry -> node );
    sequence_entry = sequence_entry -> next;
  }
}

void
local_free_mapping( FILE * stream, struct graph_node * node ) {
  if ( node -> type_tag != NULL ) {
    fprintf( stream, " free !%s", node -> type_tag );
  }
  struct mapping_list_entry * map_entry;
  map_entry = node -> content.mapping.head;
  while ( map_entry != NULL ) {
    fprintf( stream, "\nfree %s:", map_entry -> key );
    switch ( map_entry -> node -> flags & YAML_KIND ) {
//  switch ( map_entry -> node -> kind ) {
      case YAML_SCALAR:
        assert( map_entry -> node -> content.scalar.text != NULL );
        local_free_scalar(   stream, map_entry -> node );
        break;
      case YAML_SEQUENCE:
        local_free_sequence( stream, map_entry -> node );
        break;
      case YAML_MAPPING:
        local_free_mapping(  stream, map_entry -> node );
        break;
      case 0:
//    case NOT_YET_ASSIGNED:
        fprintf( stream, "\nNOT YET ASSIGNED" );
        break;
      default:
        fprintf( stream, "\nMAPPING_ENTRY_BAD_KIND" );
        abort();
        break;
    }
//  free( map_entry -> node );
    map_entry = map_entry -> next;
  }
}

void
yaml_decompose( struct graph_node * AST ) {
  /* TODO: check for bugs, it crashes at least sometimes */
//  local_free_mapping( stderr, AST );
}
