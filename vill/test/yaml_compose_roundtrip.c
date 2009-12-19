/* yaml_compose_roundtrip.c */
/* The yaml_compose_roundtrip(), yaml_parse_roundtrip() and */
/* yaml_token_roundtrip() tests are diagnostics that do not need to */
/* be executed in every test run.  They verify the low level handling */
/* of the YAML formatted Abstract Syntax Tree produced by viv. */
/* You only need the roundtrip tests when you suspect that some low */
/* level part is broken, and these parts seldom change. */
/* Convert the graph produced by yaml_compose() back into a YAML */
/* stream and compare with the original text produced by viv. */
/* They should be identical. */
/* That also means that yaml_compose_roundtrip() is a simple YAML */
/* emitter, though not a flexible one. */

#include <assert.h>  /* assert */
#include <stdio.h>   /* fgets popen pclose fprintf */
#include <stdlib.h>  /* abort malloc */
#include <string.h>  /* strlen */
#include <unistd.h>  /* getcwd */
#include "../src/yaml_compose_internal.h"  /* graph_node */

// show where to find the 'viv' Perl 6 parser
#define VIV_RELATIVE_PATH ".."

void local_mapping(  FILE *, struct graph_node *, struct graph_node *);
void local_scalar(   FILE *, struct graph_node *, struct graph_node *);
void local_sequence( FILE *, struct graph_node *, struct graph_node *);

#define USAGE "\nUsage: %s [options] [programfile]\n" \
  " -e cmd  execute cmd instead of programfile (multiple -e possible)\n" \
  " -h      show this help\n" \
  "\n"

const char * temp_filename1 = "/tmp/yaml_compose_roundtrip1.yaml";
const char * temp_filename2 = "/tmp/yaml_compose_roundtrip2.yaml";
#define BUFFER_SIZE 256
char   line_buffer[BUFFER_SIZE];
int    need_space = 0;
char * commandline;
int    indent = -1;
/* a funny place to start, but http://www.yaml.org/spec/1.2/spec.html */
/* section 9.1.3 (Bare Documents) second paragraph says it's ok. ;-) */

int
local_options( int argc, char * argv[] ) {
  int opt;
  commandline = malloc(1);
  strcpy( commandline, "" );
  while ((opt = getopt(argc, argv, "e:h")) != -1) {
    switch (opt) {
      case 'e':
        commandline = (char *) realloc( commandline,
          strlen(commandline) + strlen(optarg) + 1 );
        strcat( commandline, optarg );
        break;
      case 'h':
        fprintf( stderr, USAGE, argv[0] );
        exit(EXIT_SUCCESS);
        break;
      default: /* react to invalid options with help */
        fprintf( stderr, USAGE, argv[0] );
        exit(EXIT_FAILURE);
    }
  }
  return optind;
}

void
local_mapping( FILE * stream, struct graph_node * parent,
    struct graph_node * node ) {
  ++indent;
  if ( node -> data != NULL ) {
    /* This node has an anchor.  Decide whether to print it as an */
    /* anchor, or as an alias. */
    struct anchor_list_entry * anchor_entry =
      (struct anchor_list_entry *) node -> data;
    if ( ! anchor_entry -> anchor_shown ) {
      struct anchor_list_entry * anchor_entry =
        (struct anchor_list_entry *) node -> data;
        fprintf( stream, " &%d", anchor_entry -> anchor );
      anchor_entry -> anchor_shown = 1;
    }
  }
  if ( node -> type_tag != NULL ) {
    fprintf( stream, " !%s", node -> type_tag );
  }
  struct mapping_list_entry * map_entry;
  map_entry = node -> content.mapping.head;
  while ( map_entry != NULL ) {
    fprintf( stream, "\n%*s%s:", indent * 2, "", map_entry -> key );
    switch ( map_entry -> node -> flags & YAML_KIND ) {
      case YAML_SCALAR:
        need_space = 1;
        assert( map_entry -> node -> content.scalar.text != NULL );
        local_scalar(   stream, node, map_entry -> node );
        break;
      case YAML_SEQUENCE:
        {
          if ( node -> data != NULL ) {
            struct anchor_list_entry * anchor_entry =
              (struct anchor_list_entry *) node -> data;
            if ( anchor_entry -> anchor_shown ) {
              fprintf( stream, " *%d", anchor_entry -> anchor );
            }
            else {
              local_sequence( stream, node, map_entry -> node );
              anchor_entry -> anchor_shown = 1;
            }
          }
          else {
            local_sequence( stream, node, map_entry -> node );
          }
        }
        break;
      case YAML_MAPPING:
        {
          if ( node -> data != NULL ) {
            struct anchor_list_entry * anchor_entry =
              (struct anchor_list_entry *) node -> data;
            if ( anchor_entry -> anchor_shown ) {
              fprintf( stream, " *%d", anchor_entry -> anchor );
            }
            else {
              local_mapping(  stream, node, map_entry -> node );
              anchor_entry -> anchor_shown = 1;
            }
          }
          else {
            local_mapping(  stream, node, map_entry -> node );
          }
        }
        break;
      case 0:
        fprintf( stream, "\nNOT YET ASSIGNED" );
        break;
      default:
        fprintf( stream, "\nMAPPING_ENTRY_BAD_KIND" );
        abort();
        break;
    }
    map_entry = map_entry -> next;
  }
  --indent;
}

void
local_scalar( FILE * stream, struct graph_node * parent,
    struct graph_node * node ) {
  if ( need_space ) {
    fprintf( stream, " " );
    need_space = 0;
  }
  assert( (node -> flags & YAML_KIND) == YAML_SCALAR );
  /* The text needs quoting in single quotes if it begins or */
  /* ends with a space or a double quote character, or begins */
  /* with an exclamation mark, or has zero length. */
  size_t scalar_len = strlen( node -> content.scalar.text );
  char first = * ( node -> content.scalar.text );
  char last  = * ( node -> content.scalar.text + scalar_len - 1 );
  int need_single_quotes = (
    first == ' ' || last == ' ' || first == '"' || last == '"' ||
    first == '!' || first == '#' || scalar_len == 0
  );
  fprintf( stream, "%s%s%s",     need_single_quotes?"'":"",
    node -> content.scalar.text, need_single_quotes?"'":"" );
  if ( node -> type_tag != NULL ) {
    fprintf( stream, "\nTYPE_TAG:%s", node -> type_tag );
    abort();
  }
}

void
local_sequence( FILE * stream, struct graph_node * parent,
    struct graph_node * node ) {
  ++indent;
  if ( node -> data != NULL ) {
    /* This node has an anchor.  Decide whether to print it as an */
    /* anchor, or as an alias. */
    struct anchor_list_entry * anchor_entry =
      (struct anchor_list_entry *) node -> data;
    if ( ! anchor_entry -> anchor_shown ) {
      /* found the anchor occurrence */
      fprintf( stream, " &%d", anchor_entry -> anchor );
      anchor_entry -> anchor_shown = 1;
    }
  }
  if ( node -> type_tag != NULL ) {
    fprintf( stream, "\nTYPE_TAG:%s", node -> type_tag );
  }
  struct sequence_list_entry * sequence_entry;
  sequence_entry = node -> content.sequence.head;
  if ( (sequence_entry -> node -> flags & YAML_KIND) == 0 ) {
    fprintf( stream, " []" );
  }
  else { /* sequence_entry != NULL */
    while ( sequence_entry != NULL ) {
      fprintf( stream, "\n%*s-", (indent - 1) * 2, "" );
      switch ( sequence_entry -> node -> flags & YAML_KIND ) {
        case YAML_SCALAR:
          need_space = 1;
          local_scalar(   stream, node, sequence_entry -> node );
          break;
        case YAML_SEQUENCE:
          need_space = 1;
          {
            if ( node -> data != NULL ) {
              struct anchor_list_entry * anchor_entry =
                (struct anchor_list_entry *) node -> data;
              if ( anchor_entry -> anchor_shown ) {
                fprintf( stream, " *%d", anchor_entry -> anchor );
              }
              else {
                local_sequence( stream, node, sequence_entry -> node );
                anchor_entry -> anchor_shown = 1;
              }
            }
            else {
              local_sequence( stream, node, sequence_entry -> node );
            }
          }
          break;
        case YAML_MAPPING:
          need_space = 1;
          --indent; /* cheat the nesting because the "- " is 2 columns. */
          {
            if ( node -> data != NULL ) {
              struct anchor_list_entry * anchor_entry =
                (struct anchor_list_entry *) node -> data;
              if ( anchor_entry -> anchor_shown ) {
                fprintf( stream, " *%d", anchor_entry -> anchor );
              }
              else {
                local_mapping( stream, node, sequence_entry -> node );
                anchor_entry -> anchor_shown = 1;
              }
            }
            else {
              local_mapping(  stream, node, sequence_entry -> node );
            }
          }
          ++indent; /* the yaml spec says this is ok ;-) so do I. */
          break;
        case 0:
          fprintf( stream, "\nNOT YET ASSIGNED" );
          break;
        default:
          fprintf( stream, "\nSEQUENCE_ENTRY_BAD_KIND" );
          abort();
          break;
      }
      sequence_entry = sequence_entry -> next;
    } /* while ( sequence_entry != NULL ) */
  } /* sequence_entry != NULL */
  --indent;
}


/* The main body of the test is here, a recursive depth first */
/* traversal of the graph. */
void
yaml_compose_roundtrip( FILE * infile, FILE * outfile ) {
  struct graph_node * graph;
  graph = yaml_compose( infile );
  fprintf( outfile, "---" );
  if ( (graph -> flags & YAML_KIND) == YAML_MAPPING ) {
    local_mapping( outfile, NULL, graph );
  }
  /* Instead of blindly delegating the deletion of all the graph */
  /* nodes to an inefficient garbage collection algorithm, perform */
  /* another traversal that specifically frees each allocation */
  yaml_decompose( graph ); /* TODO: complete and verify with valgrind */
  fprintf( outfile, "\n" );
}

/* run viv (a Perl 5 program) as a child process and save the output */
int
local_run_viv( char * viv_command, const char * output_filename ) {
  int status;
  char * cwd;
  FILE * infile, * outfile;
  /* stash the current working directory, then chdir to viv's dir */
  cwd = getcwd( NULL, 0);
  assert( cwd != NULL );
  char * stash_current_dir;
  stash_current_dir=(char *)malloc(strlen(cwd)+1);
  assert( stash_current_dir != NULL );
  strcpy( stash_current_dir, cwd );
  assert( chdir( VIV_RELATIVE_PATH ) == 0 );
  /* run viv, connecting its output in the pipe called infile */
  infile = popen( viv_command, "r" );
  assert( infile != NULL );
  /* stream the viv output to a first temporary file */
  outfile = fopen( output_filename, "w" );
  assert( outfile != NULL );
  while ( fgets(line_buffer, BUFFER_SIZE, infile) != NULL ) {
    fputs( (const char *)line_buffer, outfile );
  }
  fclose( outfile );
  status = pclose( infile );
  assert( chdir( stash_current_dir ) == 0 );
  free( stash_current_dir );
  return status;
}

/* run the diff utility to compare two files */
int
local_run_diff( const char * filename1, const char * filename2 ) {
  int status;
  const char * diff_template = "diff %s %s";
  char * diff_command = (char *) malloc( strlen(diff_template) +
    strlen(filename1) + strlen(filename2) );
  assert( diff_command != NULL );
  sprintf( diff_command, diff_template, filename1, filename2 );
  status = system( diff_command );
  free( diff_command );
  return status;
}

int
local_test_commandline( char * commandline ) {
  int status = 0;
  FILE * infile, * outfile;
  printf( "yaml_compose_roundtrip -e %s%.*s", commandline,
    47 - strlen(commandline),
    "................................................." );
  char * viv_command;
  viv_command = (char *) malloc( 12 + strlen(commandline) );
  sprintf( viv_command, "./viv -e '%s'", commandline );
  status = local_run_viv( viv_command, temp_filename1 );
  free( viv_command );
  if ( status == 0 ) { /* test more only if viv returned success */
    infile = fopen( temp_filename1, "r" );
    assert( infile != NULL );
    outfile = fopen( temp_filename2, "w" );
    assert( outfile != NULL );
    /* convert parse events back to original yaml doc */
    yaml_compose_roundtrip( infile, outfile );
    /* TODO: return status */
    fclose( infile );
    fclose( outfile );
    status = local_run_diff( temp_filename1, temp_filename2 );
  }
  printf( "%s\n", (status==0) ? "ok" : "not ok" );
  return status;
}

int
local_test_one_file( char * programfile ) {
  int status;
  FILE * infile, * outfile;
  printf( "yaml_compose_roundtrip %s%.*s", programfile,
    50 - strlen(programfile),
    ".................................................." );
  char * viv_command;
  viv_command = (char *) malloc( 7 + strlen(programfile) );
  sprintf( viv_command, "./viv %s", programfile );
  status = local_run_viv( viv_command, temp_filename1 );
  free( viv_command );
  if ( status == 0 ) { /* test more only if viv returned success */
    infile = fopen( temp_filename1, "r" );
    assert( infile != NULL );
    outfile = fopen( temp_filename2,"w");
    assert( outfile != NULL );
    /* convert parse events back to original yaml doc */
    yaml_compose_roundtrip( infile, outfile ); /* TODO: return status */
    fclose( infile );
    fclose( outfile );
    status = local_run_diff( temp_filename1, temp_filename2 );
  }
  printf( "%s\n", (status==0) ? "ok" : "not ok" );
  return status;
}


/* dispatch tests according to the command line arguments */
int
main( int argc, char *argv[] ) {
  int optind, option, status, pass=0, fail=0;
  optind = local_options( argc, argv );
  if ( strlen(commandline) > 0 ) {
    status = local_test_commandline( commandline );
  }
  else {
    for ( option = optind; option < argc; option++ ) {
      status = local_test_one_file( argv[option] );
      if ( status ) { ++fail; }
      else          { ++pass; }
    }
    if ( argc > 2 ) {
      printf( "totals %d/%d pass %d fail\n", pass, argc - 1, fail );
    }
  }
  return 0;
}
