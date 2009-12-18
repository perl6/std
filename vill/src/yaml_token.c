/* yaml_token.c */
/* Reads one yaml token at a time from a stream.  Returns a */
/* yaml_token_type enum and fills a static yaml_token_data struct. */
/* To save memory and time, strings are not copied.  Instead, a */
/* pointer to the first character and a length are returned in two */
/* fields in yaml_token_data. */

#include "yaml_token.h"  /* yaml_token_struct yaml_token_type */

#define YAML_TOKEN_BUFFER_SIZE 256

struct yaml_token_struct yaml_token_data;

enum yaml_token_type
yaml_token( FILE * stream ) {
  static char buffer[YAML_TOKEN_BUFFER_SIZE];
  static char * read_pointer = NULL;
  yaml_token_data.colon_suffixed = 0;
  enum yaml_token_type token_type;
  if ( read_pointer == NULL ) {
    read_pointer = fgets( buffer, YAML_TOKEN_BUFFER_SIZE, stream );
    /* TODO: handle data longer than YAML_TOKEN_BUFFER_SIZE */
  }
  if ( read_pointer == NULL ) {
    /* clean all fields rather than passing random data to the caller */
    token_type = YAML_TOKEN_FILE_END;
    yaml_token_data.spaces = 0;
    yaml_token_data.str = NULL;
    yaml_token_data.len = 0;
  }
  else {
    read_pointer += yaml_token_data.spaces = strspn(read_pointer, " ");
    /* Count (for indentation and YAML block nesting if at beginning */
    /* of line) and skip leading spaces. */
    yaml_token_data.str = read_pointer;
    switch ( * read_pointer ) { /* test the first non space character */
      case '!':
        token_type = YAML_TOKEN_BARE;
        /* tags are delimited by space or newline */
        read_pointer += yaml_token_data.len =
          strcspn( read_pointer, " \n" );
        break;
      case '\'':
        /* single quoted strings are delimited by quotes or newline */
        token_type = YAML_TOKEN_QUOTED;
        /* search for the closing quote or the end of line */
        read_pointer += ( yaml_token_data.len =
          strcspn( ++yaml_token_data.str, "'\n" ) ) + 2;
        while ( * (read_pointer - 1) != '\'' ) {
          /* The string was not closed before the line end :-( */
          /* Append line(s) from the stream to the end of the current */
          /* buffer content until the closing '\'' or EOF is reached. */
          read_pointer = fgets( read_pointer, YAML_TOKEN_BUFFER_SIZE
            - (read_pointer - buffer), stream );
          /* TODO: handle data longer than YAML_TOKEN_BUFFER_SIZE */
          yaml_token_data.len += strcspn( read_pointer, "'\n" ) + 1;
          read_pointer = yaml_token_data.str + yaml_token_data.len + 1;
        }
        break;
      case '[':
      case '{':
        token_type = YAML_TOKEN_BARE;
        /* The default token type lower down is also YAML_TOKEN_BARE, */
        /* but these two are special because they self delimit. */
        /* Their closing equivalents do get handled by the default. */
        ++read_pointer;
        yaml_token_data.len = 1;
        break;
      case '\0':
      case '\n':
        token_type = YAML_TOKEN_LINE_END;
        read_pointer = NULL;
        break;
      default:
        /* The default token is bare (unquoted) and is delimited by */
        /* the next space or newline. */
        /* TODO: perform extra check for the last line in the file, */
        /* that may end without a '\n' character. */
        token_type = YAML_TOKEN_BARE;
        read_pointer += yaml_token_data.len =
          strcspn( read_pointer, " \n" );
        yaml_token_data.colon_suffixed = (*(read_pointer-1) == ':');
        if (yaml_token_data.colon_suffixed) {
          --yaml_token_data.len;
        }
        break;
    }
    /* sneak the caller a preview of what comes after this token */
    yaml_token_data.next_char = read_pointer ? * read_pointer : '\0';
  }
  return token_type;
}
