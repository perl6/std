// infix__S_Plus.cpp
#include "villCompiler.h"
#include <string>

// infix__S_Tilde
void
villCompiler::infix__S_Tilde() {
  std::string line;
  while ( (line=get_line()) != "" ) {
    if ( line.substr(line.size()-13,13) == "statementlist" ) {
      statementlist();
    }
  }
}
