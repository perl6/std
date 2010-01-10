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
#include "../src/graph_traverse.h"  /* graph_node */
/* #include "../src/yaml_compose_internal.h" */ /* graph_node */

/* show where to find the 'viv' Perl 6 parser */
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
local_yaml_serialize( FILE * outfile, struct graph_node * graph ) {
  struct graph_traverse_event * spidey;
  int need_space = 0;
  int anchor = 0;
  struct graph_traverse_struct * spider;
  spider = graph_traverse_begin( graph );
  while ( (spidey = graph_traverse(spider)) != NULL ) {
    if ( spidey -> parentnode == NULL ) {
      fprintf( outfile, "---" );
    }
    else {
      switch ( spidey -> parentnode -> flags & YAML_KIND ) {
        case YAML_MAPPING:
          fprintf( outfile, "\n%*s%s:", (spidey -> map_nest_count - 1)
            * 2, "", spidey -> key );
          need_space = 1;
          break;
        case YAML_SEQUENCE:
          fprintf( outfile, "\n%*s-", (spidey -> map_nest_count - 1)
            * 2, "" );
          need_space = 1;
          break;
        default:
          fprintf( stderr, "bad node kind" );
          abort();
          break;
      }
      if ( (spidey -> node -> flags & YAML_KIND) == YAML_SCALAR ) {
        fprintf( outfile, " %s", spidey -> node -> content.scalar.text );
        need_space = 0;
      }
    }
    if ( spidey -> node -> flags & YAML_ANCHORED ) {
      fprintf( outfile, " &%d", ++anchor );
    }
    if ( spidey -> node -> type_tag != NULL ) {
      fprintf( outfile, " !%s", spidey -> node -> type_tag );
      need_space = 0;
    }
  }
  graph_traverse_end( spider );
}

/* The main body of the test begins here, to do a depth first */
/* traversal of the AST graph. */
void
yaml_compose_roundtrip( FILE * infile, FILE * outfile ) {
  struct graph_node * graph;
  graph = yaml_compose( infile );
  assert( (graph -> flags & YAML_KIND) == YAML_MAPPING );
  local_yaml_serialize( outfile, graph );
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
    47 - (int)strlen(commandline),
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
    50 - (int)strlen(programfile),
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
