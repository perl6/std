// link_codegen.cpp
// Traverse the whole AST and point the data member of each node to the
// corresponding Codegen function
#include "villCompiler.h"
#include "yaml_parse.h"   // MAPPING SCALAR SEQUENCE

// declare prototypes for functions called only within this file
void local_link_mapping(    struct graph_node * );
void local_link_scalar(     struct graph_node * );
void local_link_sequence(   struct graph_node * );
// global variables shared by multiple functions in this file
int need_space =  0;
int indent     = -1;   // only for diagnostic printing
int local_debug_flag;  // controls trace printing of tree walk

// receive a tag string and return a pointer to the Codegen()
// function of matching name.  If no name matches, return NULL.
// TODO 1: design something better to return if no name matches.
// TODO 2: (LHF) do a faster lookup, eg binary search in a sorted list
function_pointer
local_Codegen( char * type_tag ) {
  // Codegen is a pointer to a function that receives a compiler object
  // and an AST node object as paramaters and returns a pointer to a
  // Function or a Value object.
  function_pointer Codegen;
  if ( strcmp( type_tag, "!perl/hash:NAME" ) == 0 ) {
    Codegen = NULL;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::arglist" ) == 0 ) {
    Codegen = &arglist;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::args" ) == 0 ) {
    Codegen = &args;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::backslash__S_n" ) == 0 ) {
    Codegen = &backslash__S_n;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::comp_unit" ) == 0 ) {
    Codegen = &comp_unit;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::decint" ) == 0 ) {
    Codegen = &decint;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::declarator" ) == 0 ) {
    Codegen = &declarator;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::desigilname" ) == 0 ) {
    Codegen = &desigilname;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::dottyop" ) == 0 ) {
    Codegen = &dottyop;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::dotty__S_Dot" ) == 0 ) {
    Codegen = &dotty__S_Dot;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::eat_terminator" ) == 0 ) {
    Codegen = &eat_terminator;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::escape__S_Back" ) == 0 ) {
    Codegen = &escape__S_Back;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::identifier" ) == 0 ) {
    Codegen = &identifier;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::infixish" ) == 0 ) {
    Codegen = &infixish;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::infix__S_Comma" ) == 0 ) {
    Codegen = &infix__S_Comma;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::infix__S_Equal" ) == 0 ) {
    Codegen = &infix__S_Equal;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::infix__S_Plus" ) == 0 ) {
    Codegen = &infix__S_Plus;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::integer" ) == 0 ) {
    Codegen = &integer;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::longname" ) == 0 ) {
    Codegen = &longname;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::methodop" ) == 0 ) {
    Codegen = &methodop;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::modifier_expr" ) == 0 ) {
    Codegen = &modifier_expr;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::modifier" ) == 0 ) {
    Codegen = &modifier;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::name" ) == 0 ) {
    Codegen = &name;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::nibbler" ) == 0 ) {
    Codegen = &nibbler;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::noun__S_term" ) == 0 ) {
    Codegen = &noun__S_term;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::noun__S_value" ) == 0 ) {
    Codegen = &noun__S_value;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::number__S_numish" ) == 0 ) {
    Codegen = &number__S_numish;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::numish" ) == 0 ) {
    Codegen = &numish;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::POST" ) == 0 ) {
    Codegen = &POST;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::quote__S_Double_Double" ) == 0 ) {
    Codegen = &quote__S_Double_Double;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::scoped" ) == 0 ) {
    Codegen = &scoped;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::scope_declarator__S_my" ) == 0 ) {
    Codegen = &scope_declarator__S_my;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::semiarglist" ) == 0 ) {
    Codegen = &semiarglist;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::sigil__S_At" ) == 0 ) {
    Codegen = &sigil__S_At;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::sigil__S_Dollar" ) == 0 ) {
    Codegen = NULL;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::statement" ) == 0 ) {
    Codegen = &statement;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::statementlist" ) == 0 ) {
    Codegen = &statementlist;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::statement_mod_cond__S_if" ) == 0 ) {
    Codegen = &statement_mod_cond__S_if;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::statement_mod_loop__S_for" ) == 0 ) {
    Codegen = &statement_mod_loop__S_for;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::Str" ) == 0 ) {
    Codegen = &Str;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::SYM_dotty__S_Dot" ) == 0 ) {
    Codegen = &SYM_dotty__S_Dot;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::SYM_infix__S_Comma" ) == 0 ) {
    Codegen = &SYM_infix__S_Comma;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::SYM_infix__S_Equal" ) == 0 ) {
    Codegen = &SYM_infix__S_Equal;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::SYM_infix__S_Plus" ) == 0 ) {
    Codegen = &SYM_infix__S_Plus; // TODO: (LHF) Minus Star Slash etc
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::termish" ) == 0 ) {
    Codegen = &termish;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::term__S_dotty" ) == 0 ) {
    Codegen = &term__S_dotty;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::term__S_identifier" ) == 0 ) {
    Codegen = &term__S_identifier;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::term__S_scope_declarator" ) == 0 ) {
    Codegen = &term__S_scope_declarator;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::term__S_value" ) == 0 ) {
    Codegen = &term__S_value;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::term__S_variable" ) == 0 ) {
    Codegen = &term__S_variable;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::twigil__S_Star" ) == 0 ) {
    Codegen = &twigil__S_Star;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::value__S_number" ) == 0 ) {
    Codegen = &value__S_number;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::value__S_quote" ) == 0 ) {
    Codegen = &value__S_quote;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::variable" ) == 0 ) {
    Codegen = &variable;
  }
  else if ( strcmp( type_tag, "!perl/hash:VAST::variable_declarator" ) == 0 ) {
    Codegen = &variable_declarator;
  }
  else if ( type_tag == NULL ) {
    fprintf( stderr, "\nnode has no type tag\n" );
    Codegen = NULL;
  } else {
    fprintf( stderr, "\n%s is not yet implemented in vill\n", type_tag );
    Codegen = NULL;
  }
  return Codegen;
}

Value * local_codegen_null( struct graph_node * )
{
  Value * result = NULL;
  return result;
}

void
local_link_mapping( struct graph_node * node ) {
  ++indent;
  if ( node -> flags & AST_VISITED ) {
    // This node has been already been visited, therefore do not
    // process all its contents again.
    // If making a debug trace, it should mean that the subsequent visit
    // occurred because of an alias (circular reference).
    assert( node -> flags & YAML_ANCHORED ); // what an alias visits
    local_debug_flag && fprintf( stderr, " *#" );
  }
  else {
    // This is the first visit to this node
    node -> flags |= AST_VISITED; // don't repeat this visit, not even recursively
    // Look up the tag name and point the * data field to the
    // corresponding *Codegen() function 
    local_debug_flag && (node -> flags & YAML_ANCHORED) &&
      fprintf( stderr, " &#" );
    if ( node -> type_tag != NULL ) {
      local_debug_flag && fprintf( stderr, " !%s", node -> type_tag );
      if ( node -> data == NULL ) {
        node -> data = (void *) local_Codegen( node -> type_tag );
      }
    }
    else {
      // the node has no tag.  Perhaps it could use some trace output.
      // TODO: provide some kind of default *Codegen() function pointer
      node -> data = (void *) &local_codegen_null;
    }
    // The current node has been processed, now recurse through its children
    struct mapping_list_entry * map_entry;
    map_entry = node -> content.mapping.head;
    while ( map_entry != NULL ) {
      local_debug_flag &&
        fprintf( stderr, "\n%*s%s:", indent * 2, "", map_entry -> key );
      switch ( map_entry -> node -> flags & YAML_KIND ) {
        case YAML_MAPPING:
          local_link_mapping( map_entry -> node );
          break;
        case YAML_SCALAR:
          need_space = 1;
          assert( map_entry -> node -> content.scalar.text != NULL );
          local_link_scalar( map_entry -> node );
          break;
        case YAML_SEQUENCE:
          local_link_sequence( map_entry -> node );
          break;
        case 0:
//      case NOT_YET_ASSIGNED:
          fprintf( stderr, "\nNOT YET ASSIGNED" );
          break;
        default:
          fprintf( stderr, "\nMAPPING_ENTRY_BAD_KIND" );
          abort();
        break;
      }
      map_entry = map_entry -> next;
    }
  } // first visit
  --indent;
}

void
local_link_sequence( struct graph_node * node ) {
  ++indent;
  if ( node -> flags & AST_VISITED ) {
    // This node has been already been visited, therefore do not
    // process all its contents again.
    // If making a debug trace, it should mean that the subsequent visit
    // occurred because of an alias (circular reference).
    assert( node -> flags & YAML_ANCHORED ); // what an alias visits
    local_debug_flag && fprintf( stderr, " *#" );
  }
  else {
    // This is the first visit to this node
    node -> flags |= AST_VISITED; // don't repeat this visit, not even recursively
    // Look up the tag name and point the * data field to the
    // corresponding *Codegen() function 
    local_debug_flag && (node -> flags & YAML_ANCHORED) &&
      fprintf( stderr, " &#" );
    if ( node -> type_tag != NULL ) {
      local_debug_flag && fprintf( stderr, " !%s", node -> type_tag );
      if ( node -> data == NULL ) {
        node -> data = (void *) local_Codegen( node -> type_tag );
      }
    }
    else {
      // the node has no tag.
      // TODO: provide some kind of default *Codegen() function pointer
      node -> data = (void *) &local_codegen_null;
    }
    // The current node has been processed, now recurse on its children
    struct sequence_list_entry * sequence_entry;
    sequence_entry = node -> content.sequence.head;
    if ( sequence_entry == NULL ) {
      // treat the trivial empty sequence specially
      // TODO: find out why there is no local_debug_flag here, or add it
      fprintf( stderr, " []" );
    }
    else { // sequence_entry != NULL
      while ( sequence_entry != NULL ) {
        local_debug_flag && fprintf( stderr, "\n%*s-", (indent - 1) * 2, "" );
        switch ( sequence_entry -> node -> flags & YAML_KIND ) {
          case YAML_MAPPING:
            need_space = 1;
            --indent; // cheat the nesting because the "- " is 2 columns.
            local_link_mapping( sequence_entry -> node );
            ++indent; // the yaml spec says this is ok ;-) so do I.
            break;
          case YAML_SCALAR:
            need_space = 1;
            local_link_scalar( sequence_entry -> node );
            break;
          case YAML_SEQUENCE:
            need_space = 1;
            local_link_sequence( sequence_entry -> node );
            break;
          case 0:
//          fprintf( stderr, "\nNOT YET ASSIGNED" );
            break;
          default:
            fprintf( stderr, "\nSEQUENCE_ENTRY_BAD_KIND" );
            abort();
            break;
        }
        sequence_entry = sequence_entry -> next;
      }
    }
  }
  --indent;
}

void
local_link_scalar( struct graph_node * node ) {
  if ( need_space ) {
    local_debug_flag && fprintf( stderr, " " );
    need_space = 0;
  }
  assert( (node -> flags & YAML_KIND) == YAML_SCALAR );
  // The text needs quoting in single quotes if it begins or
  // ends with a space or a double quote character, or begins
  // with an exclamation mark or hash, or has zero length.
  size_t scalar_len = strlen( node -> content.scalar.text );
  char first = * ( node -> content.scalar.text );
  char last  = * ( node -> content.scalar.text + scalar_len - 1 );
  int need_single_quotes = (
    first == ' ' || last == ' ' || first == '"' || last == '"' ||
    first == '!' || first == '#' || scalar_len == 0
  );
  local_debug_flag && fprintf( stderr, "%s%s%s", need_single_quotes
    ?"'":"", node -> content.scalar.text, need_single_quotes ?"'":"" );
  if ( node -> type_tag != NULL ) {
    fprintf( stderr, "\nTYPE_TAG:%s", node -> type_tag );
    abort();
  }
}

// Link the data pointer of each node to the Codegen() function whose
// name matches the last part of the type_tag.
//
// Recursively walk the entire graph (AST) node by node the same way
// that yaml_compose_roundtrip() does.  Actually, not quite.  This one
// walks recursively, which is fairly simple and fast.  A still ongoing
// refactoring is changing yaml_compose_roundtrip() to not use recursion
// but instead call graph_traverse() which uses an internal state stack,
// thus implementing Continuation Passing Style.  Why so?  For greater
// code re-use through graph_traverse(), when it eventually gets done.
void
villCompiler::link_codegen() {
  assert( AST != NULL );
  assert( (AST -> flags & YAML_KIND) == YAML_MAPPING );
  local_debug_flag = debug_flags & DEBUG_SYNTAXTREE;
  local_link_mapping( AST );
}
