/* yaml_token.h */
#ifndef YAML_TOKEN_H
#define YAML_TOKEN_H
#include <stdio.h>   /* FILE */
#include <string.h>  /* size_t */

#ifdef __cplusplus
extern "C" {
#endif

/* Parse the viv subset of http://yaml.org/spec/YAML.1.2.spec.html. */
enum yaml_token_type { YAML_TOKEN_BARE, YAML_TOKEN_QUOTED,
                       YAML_TOKEN_LINE_END, YAML_TOKEN_FILE_END };

struct yaml_token_struct {
  int spaces;
  int colon_suffixed;
  char next_char;      /* look ahead: the first char not in the token */
  char * str;
  size_t len;
};

extern struct yaml_token_struct yaml_token_data; /* yaml_token.c */
enum yaml_token_type yaml_token(FILE *);

#ifdef __cplusplus
}
#endif
#endif
/* end of yaml_token.h */
