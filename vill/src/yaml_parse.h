/* yaml_parse.h */
#ifndef YAML_PARSE_H
#define YAML_PARSE_H

#include <stdio.h>        /* FILE */

#ifdef __cplusplus
extern "C" {
#endif

enum yaml_event_type {
  YAML_EVENT_ALIAS, YAML_EVENT_ANCHOR, YAML_EVENT_DIRECTIVES_END,
  YAML_EVENT_END_FLOW_MAPPING, YAML_EVENT_END_FLOW_SEQUENCE,
  YAML_EVENT_MAPPING_KEY, YAML_EVENT_SCALAR_BARE,
  YAML_EVENT_SCALAR_CONTINUED, YAML_EVENT_SCALAR_QUOTED,
  YAML_EVENT_SEQUENCE_ENTRY, YAML_EVENT_START_FLOW_MAPPING,
  YAML_EVENT_START_FLOW_SEQUENCE, YAML_EVENT_TAG, YAML_EVENT_FILE_END
};

struct yaml_event_struct {
  char * str;
  size_t len;
  int    seq_levels;
  int    seq_change;
  int    map_levels;
  int    map_change;
  int    margin;
};

extern struct yaml_event_struct yaml_event;
/* function prototypes */
enum yaml_event_type yaml_parse(FILE *);

#ifdef __cplusplus
}
#endif

#endif
/* End of yaml_parse.h */
