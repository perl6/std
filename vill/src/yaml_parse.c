/* yaml_parse.c */
#include <assert.h>   /* assert (#define NDEBUG to bypass assertions) */
#include <stdlib.h>     /* abort free malloc */
#include "yaml_parse.h" /* yaml_parse */
#include "yaml_token.h" /* yaml_token_type yaml_token_struct */
#undef NDEBUG           /* #undef: assert() on, #define: assert() off */

struct yaml_event_struct yaml_event;

/* yaml_nesting_stack tracks the nesting of block style yaml nodes. */
/* Sequence and mapping nest levels are tracked separately because */
/* sequence levels do not increase the amount of block indentation, */
/* but mapping levels do. */
/* It is a single linked list to enable O(1) push and pop operations */
/* at the tail end.  Each stack entry is individually malloc'ed and */
/* free'd.  A possible optimization is to malloc and free an array of */
/* N entries at a time, but don't do that prematurely ;-) */

enum entry_kind { ENTRY_MAPPING, ENTRY_SEQUENCE, FLOW_MAPPING,
                  FLOW_SEQUENCE };

struct yaml_nesting_stack_entry {
  int spaces;
  int seq_levels;
  int map_levels;
  enum entry_kind kind;
  struct yaml_nesting_stack_entry * prev;
};

struct yaml_nesting_stack {
  int depth;
  struct yaml_nesting_stack_entry * top;
} nesting_stack = { 0, NULL };

void
local_push_nesting( enum entry_kind kind, int spaces, int seq_levels,
    int map_levels ) {
  struct yaml_nesting_stack_entry * new_entry;
  new_entry = (struct yaml_nesting_stack_entry *)
    malloc( sizeof(struct yaml_nesting_stack_entry) );
  new_entry -> kind       = kind;
  new_entry -> spaces     = spaces;
  new_entry -> seq_levels = seq_levels;
  new_entry -> map_levels = map_levels;
  if ( nesting_stack.top == NULL ) {
    nesting_stack.top = new_entry;
  }
  else {
    new_entry->prev = nesting_stack.top;
    nesting_stack.top = new_entry;
  }
  ++nesting_stack.depth;
}

void
local_pop_nesting() {
  assert( nesting_stack.top != NULL );
  int spaces = nesting_stack.top -> spaces;
  struct yaml_nesting_stack_entry * old_stack_top;
  old_stack_top = nesting_stack.top;
  nesting_stack.top = nesting_stack.top -> prev;
  free( old_stack_top );
  --nesting_stack.depth;
}

/* Yaml pull event parser that reads tokens from a stream and returns */
/* one parser event per call.  The event data is passed in a globally */
/* scoped struct called yaml_event. */
enum yaml_event_type
yaml_parse( FILE * stream ) {
  /* Flag that indicates whether the token (including leading spaces) */
  /* is the first one on a new line, to track block nesting by */
  /* indentation. */
  /* Initially set to 1 because no YAML_TOKEN_LINE_END has come yet. */
  static int token_is_first_on_line = 1;
  enum yaml_token_type token_type;
  enum yaml_event_type event_type;
  /* record the YAML_TOKEN_LINE_END event so that the next event will */
  /* be treated as being the first one on a line */
  while ( (token_type=yaml_token(stream)) == YAML_TOKEN_LINE_END ) {
    token_is_first_on_line = 1;
  }
  yaml_event.seq_levels = 0;
  yaml_event.seq_change = 0;
  yaml_event.map_change = 0;
  yaml_event.map_levels = 0;
  yaml_event.margin     = yaml_token_data.spaces;
  if ( nesting_stack.top != NULL ) {
    yaml_event.seq_levels = nesting_stack.top -> seq_levels;
    yaml_event.map_levels = nesting_stack.top -> map_levels;
  }

  switch (token_type) {
    case YAML_TOKEN_BARE:
      switch ( * yaml_token_data.str ) {
        /* cases sorted by event_type, not by * yaml_token_data.str */
        case '&':
          event_type = YAML_EVENT_ANCHOR;
          /* the anchor is the text after the '&' until a space or newline */
          yaml_event.str = yaml_token_data.str + 1;
          yaml_event.len = yaml_token_data.len - 1;
          break;
        case '*':
          event_type = YAML_EVENT_ALIAS;
          /* the alias is the text after the '*' until a space or newline */
          yaml_event.str = yaml_token_data.str + 1;
          yaml_event.len = yaml_token_data.len - 1;
          break;
        case ']':
          event_type = YAML_EVENT_END_FLOW_SEQUENCE;
          yaml_event.str = yaml_token_data.str + 1;
          yaml_event.len = yaml_token_data.len - 1;
          break;
        case '}':
          event_type = YAML_EVENT_END_FLOW_MAPPING;
          yaml_event.str = yaml_token_data.str + 1;
          yaml_event.len = yaml_token_data.len - 1;
          break;
        case '[':
          event_type = YAML_EVENT_START_FLOW_SEQUENCE;
          yaml_event.str = yaml_token_data.str + 1;
          yaml_event.len = yaml_token_data.len - 1;
          break;
        case '{':
          event_type = YAML_EVENT_START_FLOW_MAPPING;
          yaml_event.str = yaml_token_data.str + 1;
          yaml_event.len = yaml_token_data.len - 1;
          break;
        case '!':
          event_type = YAML_EVENT_TAG;
          /* the type is the text after the '!' until a space or newline */
          yaml_event.str = yaml_token_data.str + 1;
          yaml_event.len = yaml_token_data.len - 1;
          break;
        case '-':
          /* The '-' character signifies one of three things: */
          /* - the beginning of "---", the end of directives marker */
          /* - if followed by a space, it defines a sequence entry */
          /* - if followed by non space, it's merely part of a scalar */
          if ( yaml_token_data.len==3 &&
              strncmp( yaml_token_data.str, "---", 3 ) == 0 ) {
            /* found the "---" end of directives marker */
            event_type = YAML_EVENT_DIRECTIVES_END;
          }
          else {
            /* check the character immediately after the '-' */
            if ( token_is_first_on_line && 
                 * ( yaml_token_data.str + 1 ) == ' ' ) {
              /* The "- " at the beginning of a line defines a */
              /* sequence entry.  The "- " characters are seen as */
              /* spaces which must be added to the indentation. */
              if ( yaml_token_data.spaces > nesting_stack.top -> spaces ) {
                /* the "- " is nested deeper than the previous line */
                yaml_event.seq_change = 1;
                local_push_nesting( ENTRY_SEQUENCE, yaml_token_data.spaces,
                  yaml_event.seq_levels + 1, yaml_event.map_levels );
              }
              else {
                if ( yaml_token_data.spaces == nesting_stack.top -> spaces ) {
                  /* the "- " is nested the same depth as the previous line */
                  if ( nesting_stack.top -> kind == ENTRY_MAPPING ) {
                    yaml_event.seq_change = 1;
                    local_push_nesting( ENTRY_SEQUENCE, yaml_token_data.spaces,
                      yaml_event.seq_levels + 1, yaml_event.map_levels );
                  }
                }
                else {
                  /* The "- " is nested less deeply than its parent. */
                  /* Pop all the more deeply nested nodes off the stack, */
                  /* counting how many sequences and mappings they were. */
                  while ( nesting_stack.top -> spaces > yaml_token_data.spaces ) {
                    assert( nesting_stack.depth > 0 );
                    if ( nesting_stack.top -> kind == ENTRY_SEQUENCE ) {
                      --yaml_event.seq_change;
                    }
                    else {
                      assert( nesting_stack.top -> kind == ENTRY_MAPPING );
                      --yaml_event.map_change;
                    }
                    local_pop_nesting();
                  }
                }
              }
              yaml_event.seq_levels += yaml_event.seq_change;
              yaml_event.map_levels += yaml_event.map_change;
              event_type = YAML_EVENT_SEQUENCE_ENTRY;
            }
            else {
              /* This '-' is not a metacharacter.  It just happens to */
              /* be the first character of a scalar. */
              /* (Same as in the default, "not colon suffixed" below */
              event_type = YAML_EVENT_SCALAR_BARE;
              yaml_event.str = yaml_token_data.str;
              yaml_event.len = yaml_token_data.len;
              /* Use the lookhead to extend the scalar to the end of */
              /* the input line.  Include the metacharacters that the */
              /* scalar may contain, such as * & ' : and -. */
              while ( yaml_token_data.next_char != '\n' &&
                      yaml_token_data.next_char != '\n' ) {
                token_type=yaml_token(stream);
              }
              /* calculate the length of the scalar from difference */
              /* between pointers plus the final token */
              yaml_event.len = yaml_token_data.str - yaml_event.str
                + yaml_token_data.len;
            }
          }
          break;
        default:
          if ( yaml_token_data.colon_suffixed ) {
            /* this is a mapping key */
            assert( token_is_first_on_line );
            if ( nesting_stack.top == NULL ||
                 yaml_token_data.spaces > nesting_stack.top -> spaces ) {
              /* the "key:" is nested more deeply than its parent */
              yaml_event.map_levels += yaml_event.map_change = 1;
              local_push_nesting( ENTRY_MAPPING, yaml_token_data.spaces,
                yaml_event.seq_levels, yaml_event.map_levels );
            }
            else {
              /* The "key:" is nested less deeply than its parent. */
              /* Pop all the more deeply nested nodes off the stack, */
              /* counting how many sequences and mappings they were. */
              while ( nesting_stack.top -> spaces > yaml_token_data.spaces
                || (nesting_stack.top -> kind == ENTRY_SEQUENCE && nesting_stack.top -> spaces == yaml_token_data.spaces)
              ) {
                assert( nesting_stack.depth > 0 );
                if ( nesting_stack.top -> kind == ENTRY_SEQUENCE ) {
                  --yaml_event.seq_change;
                }
                else {
                  assert( nesting_stack.top -> kind == ENTRY_MAPPING );
                  --yaml_event.map_change;
                }
                local_pop_nesting();
              }
              yaml_event.seq_levels += yaml_event.seq_change;
              yaml_event.map_levels += yaml_event.map_change;
              assert( yaml_token_data.spaces == nesting_stack.top -> spaces );
            }
            event_type = YAML_EVENT_MAPPING_KEY;
            yaml_event.str = yaml_token_data.str;
            yaml_event.len = yaml_token_data.len;
          }
          else { /* not colon_suffixed */
            /* it is a scalar value in either a sequence or a mapping */
            if ( token_is_first_on_line ) {
              /* Some YAML emitters may break a long scalar value */
              /* into multiple lines.  This second or later line is */
              /* indented one level deeper than the first line. */
              assert( nesting_stack.top -> kind == ENTRY_MAPPING ||
                      nesting_stack.top -> kind == ENTRY_SEQUENCE );
              assert( yaml_token_data.spaces > nesting_stack.top -> spaces );
              /* signal an event */
              event_type = YAML_EVENT_SCALAR_CONTINUED;
              yaml_event.str = yaml_token_data.str;
              yaml_event.len = yaml_token_data.len;
              yaml_event.margin = yaml_token_data.spaces;
            }
            else {  /* ! token_is_first_on_line */
              event_type = YAML_EVENT_SCALAR_BARE;
              yaml_event.str = yaml_token_data.str;
              yaml_event.len = yaml_token_data.len;
              /* Use the lookhead to extend the scalar to the end of */
              /* the input line.  Include the metacharacters that the */
              /* scalar may contain, such as * & ' : and -. */
              while ( yaml_token_data.next_char != '\n' &&
                      yaml_token_data.next_char != '\n' ) {
                token_type=yaml_token(stream);
              }
              /* calculate the length of the scalar from difference */
              /* between pointers plus the final token */
              yaml_event.len = yaml_token_data.str - yaml_event.str
                + yaml_token_data.len;
            }
          }
          break;
      }
      break;
    case YAML_TOKEN_QUOTED:
      event_type = YAML_EVENT_SCALAR_QUOTED;
      yaml_event.str = yaml_token_data.str;
      yaml_event.len = yaml_token_data.len;
      /* TODO: consider long scalars spanning multiple lines */
      break;
    case YAML_TOKEN_FILE_END:
      event_type = YAML_EVENT_FILE_END;
      break;
    default:
      printf( "UNKNOWN TOKEN\n" );
      abort();
      break;
  }
  token_is_first_on_line = 0;
  return event_type;
}
