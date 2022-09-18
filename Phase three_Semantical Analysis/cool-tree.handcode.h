//
// The following include files must come first.

#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H

#include <iostream>
#include "tree.h"
#include "cool.h"
#include "stringtab.h"
#include <symtab.h>
#include <map>
#include <vector>

using std::map;
using std::vector;

#define yylineno curr_lineno;
extern int yylineno;

// forward declare ClassTable  \\+
class ClassTable;

inline Boolean copy_Boolean(Boolean b) {return b; }
inline void assert_Boolean(Boolean) {}
inline void dump_Boolean(ostream& stream, int padding, Boolean b)
	{ stream << pad(padding) << (int) b << "\n"; }

void dump_Symbol(ostream& stream, int padding, Symbol b);
void assert_Symbol(Symbol b);
Symbol copy_Symbol(Symbol b);

class Program_class;
typedef Program_class *Program;
class Class__class;
typedef Class__class *Class_;
class Feature_class;
typedef Feature_class *Feature;
class Formal_class;
typedef Formal_class *Formal;
class Expression_class;
typedef Expression_class *Expression;
class Case_class;
typedef Case_class *Case;

typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;
typedef list_node<Feature> Features_class;
typedef Features_class *Features;
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;

#define Program_EXTRAS                          \
virtual void semant() = 0;			\
virtual void dump_with_types(ostream&, int) = 0; \
virtual void SemanticAnalysis(ClassTable *&classTable)=0;


#define program_EXTRAS                          \
void semant();     				\
void dump_with_types(ostream&, int);  \
void SemanticAnalysis(ClassTable *&classTable);


// some structs to use for storing useful info.

struct MethodSign {
  // has a map of formal arguments <name:Symbol, type_decl: Symbol>
  map <Symbol, Symbol> formalArgs;

  // has a vector of declared types of the formal arguments in order 
  // of appearance: <type_decl: Symbol>
  vector <Symbol> formalArgsTypesOrdered;

  // has a return type
  Symbol returnType;
};

struct ClassFeatures {
  // has a map of (methodName:Symbol->methodSign:MethodSign)
  map <Symbol, MethodSign> methodEnv; 
  // has a map of (attrName:Symbol-> type_decl:Symbol)
  map <Symbol, Symbol> attrEnv; 
};

struct TypeCheckingEnvironment {
  // has a symbol table of (attrName:Symbol-> type_decl:Symbol)
  SymbolTable<Symbol ,Symbol> *objectEnvironment;
  // has a map of (methodName:Symbol->methodSign:MethodSign)
  map < Class_ , map <Symbol, MethodSign> > methodEnvironment; 
  // has a current class
  Class_ currentClass;
};

#define Class__EXTRAS                   \
virtual Symbol get_filename() = 0;      \
virtual void dump_with_types(ostream&,int) = 0;  \
virtual Symbol getName() = 0;       \
virtual Symbol getParent() = 0;   \
virtual void objParent() = 0; \
virtual Features getFeatures() = 0; \
virtual void TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable) = 0;

#define class__EXTRAS                                 \
Symbol get_filename() { return filename; }             \
void dump_with_types(ostream&,int);        \
Symbol getName(); \
Symbol getParent(); \
void objParent(); \
Features getFeatures(); \
void TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable);


#define Feature_EXTRAS                                        \
virtual void dump_with_types(ostream&,int) = 0; \

#define Feature_SHARED_EXTRAS             \
void dump_with_types(ostream&,int);    \

// Extras for methods and attributes! (why don't they have any??)
#define method_EXTRAS                        \
Symbol getName();                          \
Formals getFormals();                       \
Symbol getRetType(); \
void TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable);

#define attr_EXTRAS                         \
Symbol getName();                          \
Symbol getDecType(); \
void TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable);

#define Formal_EXTRAS                              \
virtual void dump_with_types(ostream&,int) = 0; \
virtual Symbol getName()=0;                          \
virtual Symbol getDecType()=0; \
virtual void TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable)=0; 

#define formal_EXTRAS                           \
void dump_with_types(ostream&,int); \
Symbol getName();                          \
Symbol getDecType(); \
void TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable); 


#define Case_EXTRAS                             \
virtual void dump_with_types(ostream& ,int) = 0;\
virtual Symbol TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable)=0; \
virtual Symbol getDecType() = 0; 


#define branch_EXTRAS                                   \
void dump_with_types(ostream& ,int);\
Symbol TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable); \
Symbol getDecType(); 

#define Expression_EXTRAS                    \
Symbol type;                                 \
Symbol get_type() { return type; }           \
Expression set_type(Symbol s) { type = s; return this; } \
virtual void dump_with_types(ostream&,int) = 0;  \
void dump_type(ostream&, int);               \
Expression_class() { type = (Symbol) NULL; } \
virtual Symbol TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable)=0; 

#define Expression_SHARED_EXTRAS           \
void dump_with_types(ostream&,int); 
Symbol TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable); 



#endif
