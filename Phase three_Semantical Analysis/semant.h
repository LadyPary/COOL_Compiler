// NOTE: lines with "+" or between "+++"'s are the codes that I added. 

#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#include <map> // +
#include <vector> // +

using std::map; //+
using std::vector; //+


#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// typedefs, for easier use of method_class and attr_class.
typedef method_class *Method;
typedef attr_class *Attr;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();

public:
  ClassTable(Classes);
  ostream& error_stream;
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
  ostream& semant_error_(tree_node *t, Class_ c);
  // +++
  // other utility stuff:
  // maps a class name to it's tree_node for faster and easier look ups!
  map <Symbol, Class_> classDict; 
  void addClass(Class_ newClass);

  // maps a class name to a vector of it's parents' names!
  map <Symbol, vector<Symbol>> inherDict; 
  // +++
};

#endif
