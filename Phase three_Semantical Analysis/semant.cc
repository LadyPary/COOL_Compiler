#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <algorithm> //used only for 'count' on a vector.
#include "semant.h"
#include "utilities.h"

extern int semant_debug;
extern char *curr_filename;

using std::endl;
using std::count;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool, // basic class
    concat,
    cool_abort,
    copy,
    Int, // basic class
    in_int,
    in_string,
    IO, // basic class
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object, // basic class
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str, // basic class
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void){
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
    }

//////////////////////////////////////////////////////////////////////
//
//  Building the inheritance graph and catching some fatal errors.
//
//////////////////////////////////////////////////////////////////////
ClassTable::ClassTable(Classes classes) : semant_errors(0), error_stream(cerr), classDict(), inherDict() {  
    // ClassTable here is used for representing the inheritance graph.

    /* Fill this in */
    install_basic_classes();

    /* Parser passes a list of Class_ nodes in classes to semant! 
       Let's add these bad boys to our look up dict for future refrence! */
    /* As explained in tree.h, we use this loop to iterate on the classes */
    for(int i = classes->first(); classes->more(i); i = classes->next(i)){
        // ... operate on classes->nth(i) ...
        // Check for redefinitions here!
        if (classDict.find((classes->nth(i))->getName()) == classDict.end()) {
            // class does not exist!
            addClass(classes->nth(i));
        }
        else {
            Symbol name = (classes->nth(i))->getName();
            if ((name != Object) && (name != IO)   &&
                (name != Int)    && (name != Bool) &&
                (name != Str)    && (name != SELF_TYPE)){
            // Class C was previously defined.
            // The cases excluded above are REDEFINITIONS of basic classes 
            // which are handeled below.
            semant_error(classes->nth(i));
            error_stream << "Class " << ((classes->nth(i))->getName())->get_string() << " was previously defined." << endl;
            (classes->nth(i))->objParent(); // err recovery
            }
        }
    }

    /* Now it's time to check the inheritance rules! BOOM LET'S GOOOO 

       What do we check?
        1. Class C was previously defined. (handeled above)
        2. Class C, or an ancestor of C, is involved in an inheritance cycle.
        3. Class C cannot inherit class Int/Bool/String/SELF_TYPE. 
        4. Class C inherits from an undefined class A. 
        5. Redefinition of basic class Object/IO/Int/Bool/String/SELF_TYPE.

       What do we do?
        Recover 'till the end of classes and
        Print all relevant error messages then, 
        ABORT!

    */

    /* second loop for validation purposes */ 
    for(int i = classes->first(); classes->more(i); i = classes->next(i)){
        // ... operate on classes->nth(i) ...
        Class_ curClass = classes->nth(i);
        Symbol curName = curClass->getName();
        Symbol curParent = curClass->getParent();

        if ((curName == Object) || (curName == IO)   ||
            (curName == Int)    || (curName == Bool) ||
            (curName == Str)    || (curName == SELF_TYPE)){
                // Redefinition of basic class Object/IO/Int/Bool/String/SELF_TYPE.
                semant_error(curClass);
                error_stream << "Redefinition of basic class "<<  curName->get_string() << "." << endl;
                curClass->objParent(); // err recovery
                curParent = Object;
           }

        if ((curParent == Int)    || (curParent == Bool) ||
            (curParent == Str)    || (curParent == SELF_TYPE)){
                // Class C cannot inherit class Int/Bool/String/SELF_TYPE.
                semant_error(curClass);
                error_stream << "Class " << curName->get_string() << " cannot inherit class "<< curParent->get_string() <<"."  << endl;
                curClass->objParent(); // err recovery
                curParent = Object;
           }

        if (classDict.find(curParent) == classDict.end()) {
            // Class C inherits from an undefined class A.
            semant_error(curClass);
            error_stream << "Class " << curName->get_string() << " inherits from an undefined class " <<  curParent->get_string() << "."  << endl;
            curClass->objParent(); // err recovery  
            curParent = Object;
            }

        vector<Symbol> grannyList;
        grannyList.push_back(curParent);

        while (curParent != Object){
            // get the grandpa!
            curParent = (classDict.find(curParent)->second)->getParent();
            grannyList.push_back(curParent);
            if (curParent == curName){
                // Class C, or an ancestor of C, is involved in an inheritance cycle.
                semant_error(curClass);
                error_stream << "Class " << curName->get_string() << ", or an ancestor of "<< curName->get_string() <<", is involved in an inheritance cycle."  << endl;
                curClass->objParent(); // err recovery  
                curParent = Object;
            }
        }
        grannyList.push_back(Object);
        inherDict[curName]=grannyList;
    }

    // Let's add the basic objects to our inheritance map
    for (auto basicClass: classDict){
        Symbol basicName  = basicClass.first;
        if ((basicName == IO)   || (basicName == Int) || 
            (basicName == Bool) || (basicName == Str)){        
            
            // their only parent is Object
            Symbol curParent  = (basicClass.second)->getParent();
        
            vector<Symbol> grannyList;
            grannyList.push_back(curParent);
            inherDict[basicName]=grannyList;
            }
        }

    if (semant_errors) {
	    cerr << "Compilation halted due to static semantic errors." << endl;
	    exit(1);
        }
    }

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    // Class_ -> object, class_ -> constructor, 

    Class_ Object_class =
	class_(Object, //name
	       No_class, // parent
	       append_Features( // feauters
			       append_Features( // constructor method(name : Symbol; formals : Formals; return_type : Symbol; expr: Expression) : Feature;
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))
                           ),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))
                   ),
	       filename); //filename

    addClass(Object_class);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))
                                              ),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))
                           ),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))
                   ),
	       filename);  

    addClass(IO_class);

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,   //constructor attr(name, type_decl : Symbol; init : Expression) : Feature;
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    addClass(Int_class);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);
    
    addClass(Bool_class);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))
                                           ),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))
                                   ),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))
                                      ),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))
                              ),
	       filename);
    addClass(Str_class);
    }
void ClassTable::addClass(Class_ newClass){
    // adds a new class node to the look up dictionary!
    classDict[newClass->getName()] = newClass;
    }

//////////////////////////////////////////////////////////////////////
// 
// Helper functions for semantic analysis and type checking.
//
//////////////////////////////////////////////////////////////////////
void addMethod(Class_ curClass, Method newMethod, map<Symbol, MethodSign> &methodEnv, ClassTable *&classTable){
    // adds a new method to the methodEnv!
    MethodSign tempMethodSign;
    map <Symbol, Symbol> formalArgs;
    vector<Symbol> formalArgsTypesOrdered;

    Formals curFormals = newMethod->getFormals();
    for(int i = curFormals->first(); curFormals->more(i); i = curFormals->next(i)){
       Formal curFormal = curFormals->nth(i);
       Symbol curName = curFormal->getName();
       Symbol curType = curFormal->getDecType();

       // some Formal semant check!
        if (curType == SELF_TYPE){
            //Formal parameter c cannot have type SELF_TYPE.
            classTable->semant_error(curClass);
            classTable->error_stream << "Formal parameter " << curName->get_string() << " cannot have type SELF_TYPE." << endl;
            curType = Object; // err recovery  
            }
        else if (classTable->classDict.find(curType) == classTable->classDict.end()) {
            // Class N of formal parameter a is undefined.
            classTable->semant_error(curClass);
            classTable->error_stream << "Class " << curType->get_string() << " of formal parameter " <<  curName->get_string() << " is undefined."  << endl;
            curType = Object; // err recovery  
            }
        if (formalArgs.find(curName) != formalArgs.end()) {
            // Formal parameter a is multiply defined.
            classTable->semant_error(curClass);
            classTable->error_stream << "Formal parameter " << curName->get_string() << " is multiply defined in method " <<  newMethod->getName()->get_string() << "."  << endl;
            }

        if ( curName == self) {
            // 'self' cannot be the name of a formal parameter.
            classTable->semant_error(curClass);
            classTable->error_stream << "'self' cannot be the name of a formal parameter." << endl;
            }
        
        formalArgs[curName]=curType;
        formalArgsTypesOrdered.push_back(curType);
    }

    Symbol curMethodName = newMethod->getName();
    Symbol retType = newMethod->getRetType();
    if (retType!=SELF_TYPE){
        if (classTable->classDict.find(retType) == classTable->classDict.end()) {
            // Undefined return type N in method e.
            classTable->semant_error(curClass);
            classTable->error_stream << "Undefined return type " << retType->get_string() << " in method " <<  newMethod->getName()->get_string() << "."  << endl;
            retType = Object; // err recovery  
        }
        
    }
    
    if (methodEnv.find(curMethodName) != methodEnv.end()) {
        // Method c is multiply defined.
        classTable->semant_error(curClass);
        classTable->error_stream << "Method " << curMethodName->get_string() << " is multiply defined in class " <<  curClass->getName()->get_string() << "."  << endl;
    }

    tempMethodSign.formalArgs = formalArgs;
    tempMethodSign.formalArgsTypesOrdered = formalArgsTypesOrdered;
    tempMethodSign.returnType = retType;
    methodEnv[curMethodName] = tempMethodSign;
    }

void addAttribute(Class_ curClass, Attr newAttr, map <Symbol, Symbol> &attrEnv, ClassTable *&classTable){
    // adds a new attr to the attrEnv!
    Symbol curName = newAttr->getName();
    Symbol curType = newAttr->getDecType();


    // some attributes semant check!
    if (curType!=SELF_TYPE){
        if (classTable->classDict.find(curType) == classTable->classDict.end()) {
            // Class N of attribute c is undefined.
            if ( curType != prim_slot ){
                classTable->semant_error(curClass);
                classTable->error_stream << "Class " << curType->get_string() << " of attribute " <<  curName->get_string() << " is undefined."  << endl;
                curType = Object; // err recovery  
            }
        }
    }

    if (attrEnv.find(curName) != attrEnv.end()) {
        // Attribute b is multiply defined in class.
        classTable->semant_error(curClass);
        classTable->error_stream << "Attribute " << curName->get_string() << " is multiply defined in class " <<  curClass->getName()->get_string() << "."  << endl;
        }

    if ( curName == self) {
            // 'self' cannot be the name of an attribute.
            classTable->semant_error(curClass);
            classTable->error_stream << "'self' cannot be the name of an attribute." << endl;
        }
    
    attrEnv[curName] = curType;

    }

Symbol join(Symbol class1, Symbol class2, Class_ currentClass, ClassTable *&classTable){
    // join here is implemented as explained in section 7.5 of The COOL Manual

    // Vector of parents are of the form: <class.parent, class.parent.parent, ..., Object>
    // We should find the first common parent in the vectors
    vector<Symbol> class1parents = classTable->inherDict[class1];
    vector<Symbol> class2parents = classTable->inherDict[class2];

    if (class1==class2){
        return class1;
        }

    if (class1 == SELF_TYPE){
        // join(SELF_TYPE{D}, A) = join(D, A)
        return join(currentClass->getName(), class2, currentClass, classTable);
        }
    if (class2 == SELF_TYPE){
        // join(A, SELF_TYPE{D}) = join(A, D)
        return join(class1, currentClass->getName(), currentClass, classTable);
        }

    for(size_t i = 0; i<class1parents.size(); i++){
        for(size_t j = 0; j<class2parents.size(); j++){
            if (class1parents[i]==class2parents[j]){
                return class1parents[i];
                }
            }
        }
    return Object; 
    }

bool isSubType(Symbol class1, Symbol class2, Class_ currentClass, ClassTable *&classTable){
    // SubType here is implemented as explained in section 4 of The COOL Manual

    // is class1 <= class2 ? Returns true if class2 is in the parents of class1
    vector<Symbol> class1parents = classTable->inherDict[class1];
    
    if (class2==Object){
        return true;
        } 

    if (class1==class2){
        return true;
        }

    if (class1 == SELF_TYPE){
        // SELF_TYPE{D} <= A if D<= A
        return isSubType(currentClass->getName(), class2, currentClass, classTable);
        }
    if (class2 == SELF_TYPE){
        // no class can be a sub type of SELF_TYPE
        return false;
        }
    if (count(class1parents.begin(), class1parents.end(), class2)) {
        // is sub type
        return true;
        }
    else {
        // is not sub type
        return false;
        }
    }


//////////////////////////////////////////////////////////////////////
// 
// Start of the SemanticAnalysis
//
//////////////////////////////////////////////////////////////////////
void program_class::SemanticAnalysis(ClassTable *&classTable){
    // Now it's time to check some semantic properties on features of non basic classes.
    // First we gather all methods and attributes and do some of the semantic checks.
    // Then we check for inheritance incompatibility and inconsistency of declared vs inferred types.

    // Env: class:Symbol -> ClassFeatures
    // ClassFeatures.methodEnv: [methodName:Symbol->methodSign:MethodSign], 
    // ClassFeatures.attrEnv: [attrName:Symbol-> type_decl:Symbol]

    map <Symbol, ClassFeatures> Env; 
    Class_ currentClass;

    map <Symbol, Class_> classDict = classTable->classDict;
    map <Symbol, vector<Symbol>> inherDict = classTable->inherDict;

    for (auto curClass : classDict){
        Symbol curClassName = curClass.first;
        Class_ curClassNode = curClass.second;

        map <Symbol, MethodSign> methodEnv; 
        map <Symbol, Symbol> attrEnv; 
        // Let's gather all the features and do some basic semantic checks!

        Features curFeautures = curClassNode->getFeatures();

        for(int i = curFeautures->first(); curFeautures->more(i); i = curFeautures->next(i)){
            Feature curFeautre = curFeautures->nth(i);
            // Let's distinguish Method vs Attribute

            Method curMethod = dynamic_cast<Method>(curFeautre);
            if (curMethod != NULL) {
                // We have a method!
                addMethod(curClassNode, curMethod, methodEnv, classTable);
            }
            else {
                // We have an attribute!
                Attr curAttr = dynamic_cast<Attr>(curFeautre);
                addAttribute(curClassNode, curAttr, attrEnv, classTable);
            }
        }
        // adds the features to the Env of each class!
        ClassFeatures tempFeature;
        tempFeature.methodEnv =  methodEnv;
        tempFeature.attrEnv   =  attrEnv;
        Env[curClassName] = tempFeature;
        } 

    // Check if we have the class Main!
    if (classDict.find( Main ) == classDict.end()) {
        // Class Main is not defined. 
        classTable->semant_error();
        classTable->error_stream << "Class Main is not defined." << endl;
        }
    else if ((Env[Main].methodEnv).find( main_meth ) == (Env[Main].methodEnv).end()){
        // No 'main' method in class Main.
        classTable->semant_error(classDict[Main]);
        classTable->error_stream << "No 'main' method in class Main." << endl;
        } 


    // In this second part, we check some further semantical restrictions on features.
    for (auto curClass : Env){
        map <Symbol, MethodSign> methodEnv = (curClass.second).methodEnv;
        map <Symbol, Symbol> attrEnv = (curClass.second).attrEnv;
        vector<Symbol> grannies = inherDict[curClass.first];
        Class_ curClassNode = classDict[curClass.first];

        // Methods:
        // Inheritance problems
        //      There are some limitations on redefining inherited methods!
        for (auto curMethod : methodEnv){ 
            Symbol methodName = curMethod.first;
            MethodSign methodSign = curMethod.second;

            // let's first check if a method is in fact redefined.
            for (auto curParent : grannies){ 
                if ((Env[curParent].methodEnv).find( methodName ) != (Env[curParent].methodEnv).end()){
                    // We have a redefined method!
                    MethodSign prevMethodSign = (Env[curParent].methodEnv)[methodName];

                    if (methodSign.returnType != prevMethodSign.returnType){
                    // In redefined method c, return type String is different from original return type Int.
                        classTable->semant_error(curClassNode);
                        classTable->error_stream << "In redefined method "<< methodName->get_string() << ", return type " << 
                        methodSign.returnType->get_string() << " is different from original return type " << 
                        prevMethodSign.returnType->get_string() << "." << endl;
                    }
                    vector <Symbol> methodFormalTypes = methodSign.formalArgsTypesOrdered;
                    vector <Symbol> prevMethodFormalTypes = prevMethodSign.formalArgsTypesOrdered;
                    if (methodFormalTypes.size() != prevMethodFormalTypes.size()){
                    // Incompatible number of formal parameters in redefined method c.
                        classTable->semant_error(curClassNode);
                        classTable->error_stream << "Incompatible number of formal parameters in redefined method "<<
                        methodName->get_string() << "." << endl;

                    }
                    else{
                        for ( auto  i = methodFormalTypes.cbegin(), end_i = methodFormalTypes.cend(), 
                                    j = prevMethodFormalTypes.cbegin();
                                    i != end_i; i++,j++){
                                    if ( (*i) != (*j) ){
                                        // In redefined method c, parameter type String is different from original type Int.
                                        classTable->semant_error(curClassNode);
                                        classTable->error_stream << "In redefined method "<< methodName->get_string() << 
                                        ", parameter type " << (*i)->get_string() << " is different from original type "<< 
                                        (*j)->get_string() <<"."<< endl;
                                        }

                                    }
                        }
                    }
            }
        }
        // Attributes:
        // Inheritance problems
        //      We cannot redefine inherited attributes!
        for (auto curAttr : attrEnv){
            Symbol attrName = curAttr.first;
            // Check if the attribute is redefined
            for (auto curParent : grannies){ 
                if ((Env[curParent].attrEnv).find( attrName ) != (Env[curParent].attrEnv).end()){
                    // We have a redefined attribute!
                    //   Attribute b is an attribute of an inherited class.
                    classTable->semant_error(curClassNode);
                    classTable->error_stream << "Attribute "<< attrName->get_string() << 
                    " is an attribute of an inherited class." << endl;
                    }
                }
            }
        }

    // The type checking environment has 
    //                        objectEnv(SymbolTable handels this), 
    //                        methodEnv(ClassFeatures->methodEnv), 
    //                        currentClass(Class_).

    // METHOD ENVIRONMENT
    // Before going any further let's prepare the method environment.
    map < Class_ , map <Symbol, MethodSign> > methodEnvironment; 

    // To be able to find M(class, f)=f.signature, where f could be
    // inherited from a parent of class, We have to create a lookup table,
    // map<class:Class_, map <methodName:Symbol, MethodSign>.
    // Since there could be legal redefines of methods (we reported err 
    // for illegal one's above!), we have to start from the oldest parent and
    // add methods, up to the current class.
    for (auto curClass : classDict){
        Symbol curClassName = curClass.first;
        Class_ curClassNode = curClass.second;

        vector<Symbol> grannies = inherDict[curClassName];
        // add the symbol to the front for the condition of if.
        grannies.insert(grannies.begin(), curClassName);
        
        map <Symbol, MethodSign> curClassMethods = Env[curClassName].methodEnv; 

        for(int i = grannies.size() - 1; i >= 0; i--){
            Symbol curParent = grannies[i];
            
            if (curParent!= curClassName){
                // Add all the inherited methods to the method environment of
                // the current class.
                map <Symbol, MethodSign> curParentMethods = Env[curParent].methodEnv; 
                for (auto curMethod : curParentMethods){
                    Symbol curMethodName = curMethod.first;
                    MethodSign curMethodSign = curMethod.second;
                    methodEnvironment[curClassNode][curMethodName]=curMethodSign;
                }
            }
            else{
                // The inherited methods are over. 
                // Time to add the methods of the current class or
                // over write any inherited methods.
                for (auto curMethod : curClassMethods){
                    Symbol curMethodName = curMethod.first;
                    MethodSign curMethodSign = curMethod.second;
                    methodEnvironment[curClassNode][curMethodName]=curMethodSign;
                }
        
            }
        }
        }


    // Now that everything works smoothly on our features(only the built-in or inheritance stuff!), 
    // Let's begin our recursive decent algorithm for type checking!
    // This is THE LAST SUPPER, I mean pass :))))

    // Build the scope first 
    // below code used from symtab_example.cc
    // scope maps idName: Symbol -> typeId: Symbol
    SymbolTable<Symbol ,Symbol> *objectEnvironment = new SymbolTable<Symbol ,Symbol>();
    // methods on the symbol table are: 
    //   map->enterscope();         
    //       enter a scope; required before any symbols can be added.
    //   map->addid(Symbol, Symbol);      
    //       add a couple of entries mapping name to age.
    //       the second argument must be a pointer to an Symbol.
    //   map->probe(Symbol) != NULL  
    //       check whether Symbol is in the current scope;
    //   map->lookup(Symbol) != NULL
    //       check whether Symbol is in any scope; lookup returns a pointer to the typeId.
    //   map->exitscope();
    //       leave a scope

    //  VERY IMPORTANT CATCH ON USING SymbolTable OBJECTS:
    //      make sure the second var is pointing to a memory loc 
    //      that is stable in the whole run! Not local or temp vars.

    for (auto curClass : classDict){
        Symbol curClassName = curClass.first;
        Class_ curClassNode = curClass.second;
        vector<Symbol> grannies = inherDict[curClassName];
        map <Symbol, MethodSign> curClassMethods = Env[curClassName].methodEnv; 
        map <Symbol, Symbol> curClassAttrs = Env[curClassName].attrEnv; 
        
        // OBJECT ENVIRONMENT

        // Let's prepare the scope, 
        // We should add all the attrs of each parent to the scope!
        // Since there would be no redefinitions(check before), it doesn't matter 
        // whether to start from the current class or the oldest parent, they would
        // be all smashed into one scope.

        objectEnvironment->enterscope();

        for (auto iter = grannies.rbegin(); iter != grannies.rend(); ++iter) {
            map <Symbol, Symbol> &curParentAttrs = Env[*iter].attrEnv;

            for (auto jiter = curParentAttrs.begin(); jiter != curParentAttrs.end(); ++jiter) {
                objectEnvironment->addid(jiter->first, &(jiter->second));
            } 
       }

        for (auto jiter = curClassAttrs.begin(); jiter != curClassAttrs.end(); ++jiter) {
            
            objectEnvironment->addid(jiter->first, &(jiter->second));

        }

        // CURRENT CLASS 
        Class_ currentClass = curClassNode;

        TypeCheckingEnvironment environment;
        environment.objectEnvironment = objectEnvironment;
        environment.methodEnvironment = methodEnvironment;
        environment.currentClass = currentClass;

        // Now we have the objectEnvironment, the methodEnvironment and, the currentClass.
        // LET THE SEMANTIC ANALYSIS BEGIN!
        // We use a recursive decent algorithm to pass through all nodes.
        // Starting from class nodes, down to their features, until we get to the leaves.
        // Note that I'm passing down the classTable only for the error routines and classDict, inherDict!

        curClassNode->TypeCheck(environment, classTable);

        // After we are done with that class node, we should exit the scope. 
        environment.objectEnvironment->exitscope();
            }
        }

// The type inferring logic rules from section 12.2 of The COOL Manuals
// are the reference for the below codes.

void class__class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    // When type checking a class, we should type check each one of it's features (methods, attr).
    // Some premitive type checking on features have been done in previous passes. 
    // Here we are only concerned with scope checking and compatibility of declared and inferred types.
    Features classFeatures = (environment.currentClass)->getFeatures();
    for(int i = classFeatures->first(); classFeatures->more(i); i = classFeatures->next(i)){
        Feature curFeature = features->nth(i);
        
        // Let's distinguish Method vs Attribute
        Method curMethod = dynamic_cast<Method>(curFeature);
        if (curMethod != NULL) {
            // We have a method!         
            // This is where the recurse happen!
            curMethod->TypeCheck(environment, classTable);
            }

        else {
            // We have an attribute!
            Attr curAttr = dynamic_cast<Attr>(curFeature);
            curAttr->TypeCheck(environment, classTable);
            }
        }
    }

// -- Features --
// [Method]
void method_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){ 
    // Note that a huge part of type checking on methods is already done in prev passes.

    // The rule for typing methods, checks the body of the method in an environment where O{C} is 
    // extended with bindings for the formal parameters and self. The type of the method body must 
    // conform to the declared return type.

    // So first we should add all the formals to the scope
    (environment.objectEnvironment)->enterscope();
    (environment.objectEnvironment)->addid(self, &SELF_TYPE);

    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        Formal curFormal = formals->nth(i);
        Symbol curType = curFormal->getDecType();
        curFormal->TypeCheck(environment, classTable);;
        }
    
    // Now evaluate the body!
    Symbol bodyType = expr->TypeCheck(environment, classTable);

    if ( !isSubType(bodyType, return_type, environment.currentClass ,classTable) ){
        if (No_type!=bodyType){
            // Inferred return type String of method e does not conform to declared return type Int.
            classTable->semant_error_(this, environment.currentClass);
            classTable->error_stream << "Inferred return type "<< 
            bodyType->get_string() << " of method " << name->get_string() << 
            " does not conform to declared return type " << return_type->get_string() << 
            "." << endl;
            }
        }
    (environment.objectEnvironment)->exitscope();
    }

void formal_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){ 
    // used only to add formals to the scope
    (environment.objectEnvironment)->addid(name, &type_decl);
    }

// [Attr-Init] and [Attr-No-Init]
void attr_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    // Note that a huge part of type checking on attributes is already done in prev passes.

    // The two rules for type checking attribute defininitions are similar the rules for let.
    // The essential diference is that attributes are visible within their initialization expressions.
    // Note that self is bound in the initialization.
    
    // Here we are only concered with the attributes with initialization.
    (environment.objectEnvironment)->enterscope();
    (environment.objectEnvironment)->addid(self, &SELF_TYPE);
    
    if ( dynamic_cast<no_expr_class*>(init) == NULL) {
        // if the attribute has an initialization

        Symbol initType = init->TypeCheck(environment, classTable);


        if (!isSubType(initType, type_decl, environment.currentClass ,classTable)) {
            //Inferred type String of initialization of attribute b does not conform to declared type Int.
            classTable->semant_error_(this, environment.currentClass);
            classTable->error_stream << "Inferred type "<< 
            initType->get_string() << " of initialization of attribute " << name->get_string() << 
            " does not conform to declared type " << type_decl->get_string() << 
            "." << endl;
        }
    }
    (environment.objectEnvironment)->exitscope();
    }

// [ASSIGN]
Symbol assign_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){ 
    Symbol exprType = expr->TypeCheck(environment, classTable);
    Symbol *lookupPtr = environment.objectEnvironment->lookup(name);
    Symbol originalType = *lookupPtr;

    if ( lookupPtr==NULL ){
        // Undeclared identifier e.
        classTable->semant_error_(this, environment.currentClass);
        classTable->error_stream << "Undeclared identifier " << 
        name->get_string() <<"." << endl;
        type = Object; // err recovery
        return type;
        }

    if ( exprType==SELF_TYPE ){
        // Cannot assign to 'self'.
        classTable->semant_error_(this, environment.currentClass);
        classTable->error_stream << "Cannot assign to 'self'."<< endl;
        exprType = Object; // err recovery
    }
    else if ( !isSubType(exprType, originalType, environment.currentClass ,classTable) ) {
        //Inferred type String of assignment of attribute b does not conform to declared type Int.
        classTable->semant_error_(this, environment.currentClass);
        classTable->error_stream << "Inferred type "<< 
        exprType->get_string() << " of assignment of attribute " << name->get_string() << 
        " does not conform to declared type " << originalType->get_string() << 
        "." << endl;
        }

    type = exprType;
    return type;
    }

// -- Dispatch --
// [Dispatch]
Symbol dispatch_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){ 
    // Dispatch expressions are the most complex to type check. e0.f(e1,...,en) : Tn+1
    //      To type check a dispatch, each of the subexpressions must first be type checked.
    //      The type T0 of e0 determines which declaration of the method f is used.
    //      The argument types of the dispatch must conform to the declared argument types.
    //      Note that the type of the result of the dispatch is either the declared return type or T0
    //      in the case that the declared return type is SELF_TYPE. 
    Symbol exprType = expr->TypeCheck(environment, classTable); // e0:T0

    Class_ methodClass; 

    if (exprType==SELF_TYPE){
        methodClass = environment.currentClass;
        }
    else {
        methodClass = classTable->classDict[exprType];
        }    

    map <Symbol, MethodSign> classMethods =  environment.methodEnvironment[methodClass];
    MethodSign methodSign;

    if (classMethods.find(name) == classMethods.end()) {
            // Dispatch to undefined method f.
            classTable->semant_error_(this, environment.currentClass);
            classTable->error_stream << "Dispatch to undefined method " << 
            name->get_string()<< endl;
            type = Object; // err recovery
            return type;
            }

        else {
            methodSign = environment.methodEnvironment[methodClass][name];
            }

    vector<Symbol> argTypes = methodSign.formalArgsTypesOrdered;

    Expressions actualArgs = actual; // e1,...,en
    size_t actualArgsSize = actualArgs->len();
    if ( argTypes.size() != actualArgsSize){
        // Method f called with wrong number of arguments.
        classTable->semant_error_(this, environment.currentClass);
            classTable->error_stream << "Method " << 
            name->get_string() << " called with wrong number of arguments." << endl;
            type = Object; // err recovery
            return type;
        }

    int j = 0;
    for (int i = actualArgs->first(); actualArgs->more(i); i = actualArgs->next(i)) {
        Expression curExp = actualArgs->nth(i);
        Symbol curType = curExp->TypeCheck(environment, classTable); // e1:T1,...,en:Tn
        if (!isSubType(curType, argTypes[j], environment.currentClass ,classTable)){
            // In call of method f, type Int of parameter no. 2 does not conform to declared type Bool.
            classTable->semant_error_(this, environment.currentClass);
            classTable->error_stream << "In call of method " << 
            name->get_string() << ", type "<< curType->get_string() << " of the parameter no. " << 
            (j+1) << " does not conform to declared type "<<argTypes[j]->get_string()<< "." << endl;
            }

        j++;
        }

    Symbol methodRetType = methodSign.returnType;
    if (methodRetType==SELF_TYPE){
        type = exprType; // T0
        }
    else{
        type = methodRetType;
        }
    return type;
    }


// [StaticDispatch]
Symbol static_dispatch_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    // e0@T.f(e1,...,en) : Tn+1
    // The only diference in type checking a static dispatch is that the class T of
    // the method f is given in the dispatch, and the type T0 must conform to T.

    Symbol exprType = expr->TypeCheck(environment, classTable); // e0:T0

    Class_ methodClass = classTable->classDict[type_name];

    map <Symbol, MethodSign> classMethods =  environment.methodEnvironment[methodClass];
    MethodSign methodSign;

    if (classMethods.find(name) == classMethods.end()) {
            // Dispatch to undefined method f.
            classTable->semant_error_(this, environment.currentClass);
            classTable->error_stream << "Dispatch to undefined method " << 
            name->get_string()<< endl;
            type = Object; // err recovery
            return type;
            }
        else {
            methodSign = environment.methodEnvironment[methodClass][name];
            }
    
    if (!isSubType(exprType, type_name, environment.currentClass ,classTable)){
            // Type String of expression does not conform to static type Bool of dispatch f of class C.
            classTable->semant_error_(this, environment.currentClass);
            classTable->error_stream << "Type " << exprType->get_string() << 
            " of expression does not conform to static type " << 
            type_name->get_string() << " of dispatch " << name->get_string() << "." << endl;
            type = Object; // err recovery
            return type;
            }

    vector<Symbol> argTypes = methodSign.formalArgsTypesOrdered;

    size_t actualSize = actual->len(); // e1,...,en
    if ( argTypes.size() != actualSize){
        // Incompatible number of arguments on dispatch of method f.
        classTable->semant_error_(this, environment.currentClass);
            classTable->error_stream << "Incompatible number of arguments on dispatch of method " << 
            name->get_string() << "." << endl;
            type = Object; // err recovery
            return type;
        }

    int j = 0;
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        Expression curExp = actual->nth(i);
        Symbol curType = curExp->TypeCheck(environment, classTable); // e1:T1,...,en:Tn
        if (!isSubType(curType, argTypes[j], environment.currentClass ,classTable)){
            // In call of method f, type Int of parameter no. 1 does not conform to declared type Bool.
            classTable->semant_error_(this, environment.currentClass);
            classTable->error_stream << "In call of method " << 
            name->get_string() << ", type "<< curType->get_string() << " of the parameter no. " << 
            (j+1)<< " does not conform to declared type "<<argTypes[j]->get_string()<< "." << endl;
            }
        j++;
        }

    Symbol methodRetType = methodSign.returnType;
    if (methodRetType==SELF_TYPE){
        type = exprType; // T0
        }
    else{
        type = methodRetType;
        }
    return type;
    }

// -- Conditional, Loop, Case, Let --
// [If]
Symbol cond_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    // needs LUB
    Symbol typePred = pred->TypeCheck(environment, classTable);
    Symbol typeThen = then_exp->TypeCheck(environment, classTable);
    Symbol typeElse = else_exp->TypeCheck(environment, classTable);
    if (typePred != Bool){
        //Loop condition does not have type Bool.
        classTable->semant_error_(this, environment.currentClass);
        classTable->error_stream << "If condition does not have type Bool."<< endl;
    }
    type = join(typeThen, typeElse, environment.currentClass, classTable);
    return type;
    }
// [Loop]
Symbol loop_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    //The predicate of a loop must have type Bool; the type of the entire loop is always Object.
    Symbol typePred = pred->TypeCheck(environment, classTable);
    Symbol typeBody = body->TypeCheck(environment, classTable); // Is not used tho.
    if (typePred!=Bool){
        //Loop condition does not have type Bool.
        classTable->semant_error_(this, environment.currentClass);
        classTable->error_stream << "Loop condition does not have type Bool."<< endl;
    }
    type = Object;
    return type;
    }

// [Case]
Symbol typcase_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    // Each branch of a case is type checked in an environment where variable xi has type Ti. 
    // The type of the entire case is the join of the types of its branches. 
    // The variables declared on each branch of a case must all have distinct types.

    expr->TypeCheck(environment, classTable);

    // Keeps track of the variables declared on each branch of a case.
    vector<Symbol> varTypes; 

    // Join of the types of the branches.
    Symbol retType;

    for(int i = cases->first(); cases->more(i); i = cases->next(i)) {

        Case curBranch = cases->nth(i);
        Symbol varDecType = curBranch->getDecType();

        if ((i != cases->first()) && 
            (count(varTypes.begin(), varTypes.end(), varDecType))) {
            // duplicate type
            // "The variables declared on the branches of case do not have distinct types.
            classTable->semant_error_(this, environment.currentClass);
            classTable->error_stream << "The variables declared on the branches of case do not have distinct types." << endl;
            }
        else {
            // is not duplicate type
            varTypes.push_back(varDecType);
            } 

        Symbol branchType = curBranch->TypeCheck(environment, classTable);

        if (i == cases->first()){
            retType = join(branchType, branchType, environment.currentClass, classTable);
            }

        else {
            retType = join(branchType, retType, environment.currentClass, classTable);
            }
        }

        type = retType;
        return type;
    }

Symbol branch_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    // Each branch of a case is type checked in an environment where variable xi has type Ti. 
    (environment.objectEnvironment)->enterscope();
    (environment.objectEnvironment)->addid(name, &type_decl);

    Symbol branchType = expr->TypeCheck(environment, classTable);

    (environment.objectEnvironment)->exitscope();

    return branchType;
    }
// [Sequence]
Symbol block_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    Expressions exprList = body;
    for(int i = exprList->first(); exprList->more(i); i = exprList->next(i)){
        Expression curExpr = exprList->nth(i);
        type = curExpr->TypeCheck(environment, classTable);
    }
    return type; 
    }

// [Let-Init] and [Let-No-Init] 
Symbol let_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    // We give type rules only for a let with a single variable.
    // Typing a multiple let, is defined to be the same as typing Let (Let (Let ...)).

    if ( dynamic_cast<no_expr_class*>(init) == NULL) {
    // Type checking for [Let-Init]: let x:T0<-e1 in e2:T1
    //      First, the initialization e1 is type checked in an environment without a new definition for x.
    //      Thus, the variable x cannot be used in e1 unless it already has a definition in an outer scope.
    //      Second, the body e2 is type checked in the environment O extended with the typing x : T0.
    //      Third, note that the type of x may be SELF TYPE.
        Symbol initType = init->TypeCheck(environment, classTable);
        if (!isSubType(initType, type_decl, environment.currentClass ,classTable)) {
            //Inferred type String of initialization of variable b in Let, does not conform to declared type Int.
            classTable->semant_error_(this, environment.currentClass);
            classTable->error_stream << "Inferred type "<< 
            initType->get_string() << " of initialization of variable " << identifier->get_string() << 
            " in Let, does not conform to declared type " << type_decl->get_string() << 
            "." << endl;
            }
        }
    // Type checking for [Let-No-Init]: let x:T0 in e1:T1
    //      The rule for let with no initialization simply omits the conformance requirement.
    if (identifier==self){
        // 'self' cannot be bound in a 'let' expression.
        classTable->semant_error_(this, environment.currentClass);
        classTable->error_stream << "'self' cannot be bound in a 'let' expression."<< endl;
        }

    (environment.objectEnvironment)->enterscope();
    (environment.objectEnvironment)->addid(identifier, &type_decl);
    
    type = body->TypeCheck(environment, classTable);
    
    (environment.objectEnvironment)->exitscope();

    return type;
    }

// -- Arithmetic nodes -- 
// All type checkings are the same! except for complement.
// [Arith]
Symbol plus_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    Symbol typeExp1 = e1->TypeCheck(environment, classTable);
    Symbol typeExp2 = e2->TypeCheck(environment, classTable);
    
    if ( typeExp1 != Int || typeExp2 != Int ){
        // non-Int arguments: Int + String
        classTable->semant_error_(this, environment.currentClass);
        classTable->error_stream << "non-Int arguments: " << 
        typeExp1->get_string() << " + " <<
        typeExp2->get_string() << "." << endl;
        }
    type = Int;
    return type;
    }

// [Arith]
Symbol sub_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    Symbol typeExp1 = e1->TypeCheck(environment, classTable);
    Symbol typeExp2 = e2->TypeCheck(environment, classTable);
    
    if ( typeExp1 != Int || typeExp2 != Int ){
        // non-Int arguments: Int - String
        classTable->semant_error_(this, environment.currentClass);
        classTable->error_stream << "non-Int arguments: " << 
        typeExp1->get_string() << " - " <<
        typeExp2->get_string() << "." << endl;
        }
    type = Int;
    return type;
    }

// [Arith]
Symbol mul_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    Symbol typeExp1 = e1->TypeCheck(environment, classTable);
    Symbol typeExp2 = e2->TypeCheck(environment, classTable);
    
    if ( typeExp1 != Int || typeExp2 != Int ){
        // non-Int arguments: Int * String
        classTable->semant_error_(this, environment.currentClass);
        classTable->error_stream << "non-Int arguments: " << 
        typeExp1->get_string() << " * " <<
        typeExp2->get_string() << "." << endl;
        }
    type = Int;
    return type;
    }
    
// [Arith]
Symbol divide_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    Symbol typeExp1 = e1->TypeCheck(environment, classTable);
    Symbol typeExp2 = e2->TypeCheck(environment, classTable);
    
    if ( typeExp1 != Int || typeExp2 != Int ){
        // non-Int arguments: Int / String
        classTable->semant_error_(this, environment.currentClass);
        classTable->error_stream << "non-Int arguments: " << 
        typeExp1->get_string() << " / " <<
        typeExp2->get_string() << "." << endl;
        }

    type = Int;
    return type;
    }

// [Neg]
Symbol neg_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    Symbol typeExp = e1->TypeCheck(environment, classTable);
    if ( typeExp != Int ){
        // Argument of 'neg' has type Bool instead of Int.
        classTable->semant_error_(this, environment.currentClass);
        classTable->error_stream << "Argument of 'neg' has type " << 
        typeExp->get_string() << "instead of Int." << endl;
        }
    type = Int;
    return type;
    }
// [Not]
Symbol comp_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    Symbol typeExp = e1->TypeCheck(environment, classTable);    
    if ( typeExp != Bool ){
        // Argument of 'not' has type Int instead of Bool.
        classTable->semant_error_(this, environment.currentClass);
        classTable->error_stream << "Argument of 'not' has type " << 
        typeExp->get_string() << " instead of Bool." << endl;
        }
    type = Bool;
    return type;
    }

// -- Comparison nodes --
// [Equal]
Symbol eq_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    // The wrinkle in the rule for equality is that any types may be freely compared except 
    // Int, String and Bool, which may only be compared with objects of the same type.
    Symbol typeExp1 = e1->TypeCheck(environment, classTable);
    Symbol typeExp2 = e2->TypeCheck(environment, classTable);

    if ( typeExp1 == Int || typeExp1 == Str || typeExp1 == Bool ||
         typeExp2 == Int || typeExp2 == Str || typeExp2 == Bool){
        if (typeExp1 != typeExp2){
            // Illegal comparison with a basic type String and Int.
            classTable->semant_error_(this, environment.currentClass);
            classTable->error_stream << "Illegal comparison with a basic type " << 
            typeExp1->get_string() << " and " <<
            typeExp2->get_string() << "." << endl;
            }
        }
    type = Bool;
    return type;
    }

// [Compare]
Symbol lt_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    // same as leq
    Symbol typeExp1 = e1->TypeCheck(environment, classTable);
    Symbol typeExp2 = e2->TypeCheck(environment, classTable);
    if ( typeExp1 != Int || typeExp2 != Int ){
        // non-Int arguments: Int < String
        classTable->semant_error_(this, environment.currentClass);
        classTable->error_stream << "non-Int arguments: " << 
        typeExp1->get_string() << " < " <<
        typeExp2->get_string() << "." << endl;
        }
    type = Bool;
    return type;
    }

// [Compare]
Symbol leq_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    // same as le
    Symbol typeExp1 = e1->TypeCheck(environment, classTable);
    Symbol typeExp2 = e2->TypeCheck(environment, classTable);

    if ( typeExp1 != Int || typeExp2 != Int ){
        // non-Int arguments: Int < String
        classTable->semant_error_(this, environment.currentClass);
        classTable->error_stream << "non-Int arguments: " << 
        typeExp1->get_string() << " <= " <<
        typeExp2->get_string() << "." << endl;
        }
    type = Bool;
    return type;
    }

// -- Leaf nodes --
// [Int]
Symbol int_const_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    type = Int;
    return type;}
// [True] and [False] 
Symbol bool_const_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    type = Bool;
    return type;
    }
// [String]
Symbol string_const_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    type = Str;
    return type;}
// [New]
Symbol new__class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    // There are two cases for new, one for new SELF_TYPE and one for any other form.
    // Since we only have SELF_TYPEc, so directly returning type_name is okay.
    type = type_name;
    return type;
    }

// [Isvoid]
Symbol isvoid_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    // type check the body of expression
    e1->TypeCheck(environment, classTable);
    type = Bool;
    return type;
    }
// The special expression no_expr must be assigned the type No_type.
Symbol no_expr_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    type = No_type;
    return type;
    }
// [Var]
Symbol object_class::TypeCheck(TypeCheckingEnvironment environment, ClassTable *&classTable){
    // The rule for object identifiers is simply that if the environment 
    // assigns an identifier Id type T, then Id has type T.
    Symbol *lookupPtr = (environment.objectEnvironment)->lookup(name);
    if (lookupPtr == NULL){
        // Undeclared identifier e.
        classTable->semant_error_(this, environment.currentClass);
        classTable->error_stream << "Undeclared identifier " << 
        name->get_string() <<"." << endl;
        type = Object; // err recovery
        }
    else{
        type = *lookupPtr;
        }
    return type;
    }

/////////////////////
// 
// utility functions(for fetching or modifing node values) for tree_nodes: 
// (declared in cool-tree-handcode.h, those that need extra steps are defined here.)
//      3. objParent, sets the parent of the class to 'Object' for err recovery  
//
/////////////////////

Symbol class__class::getName()       { return name; }
Symbol class__class::getParent()     { return parent; }
void class__class::objParent()       { parent = Object; }
Features class__class::getFeatures() { return features; }

Symbol method_class::getName()       { return name; }                         
Formals method_class::getFormals()   { return formals; }               
Symbol method_class::getRetType()    { return return_type; } 

Symbol attr_class::getName()         { return name; }         
Symbol attr_class::getDecType()      { return type_decl; }

Symbol formal_class::getName()       { return name; }         
Symbol formal_class::getDecType()    { return type_decl; }

Symbol branch_class::getDecType()    { return type_decl; }

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c){                                                             
    return semant_error(c->get_filename(),c);
    }    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t){
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
    }

// Combining above two routines for a more convenient one!
ostream& ClassTable::semant_error_(tree_node *t, Class_ c){
    semant_error(c->get_filename(), t);
    return error_stream;
    }

ostream& ClassTable::semant_error(){                                                 
    semant_errors++;                            
    return error_stream;
    } 

////////////////////////////////////////////////////////////////////
//   This is the entry point to the semantic checker.
//
//     Your checker should do the following two things:
//     1) Check that the program is semantically correct
//     2) Decorate the abstract syntax tree with type information
//        by setting the `type' field in each Expression node.
//        (see `tree.h')
//
//     You are free to first do 1), make sure you catch all semantic
//     errors. Part 2) can be done in a second stage, when you want
//     to build mycoolc.
////////////////////////////////////////////////////////////////////
void program_class::semant(){
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis 
       Here we catch all the fatal errors with classes. 
       More info is provided in the ClassTable class. 
    */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */
    SemanticAnalysis(classtable);

    if (classtable->errors()) {
	    cerr << "Compilation halted due to static semantic errors." << endl;
	    exit(1);
        }
    }


