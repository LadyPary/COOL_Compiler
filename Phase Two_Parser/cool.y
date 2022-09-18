/*
*  cool.y
*              Parser definition for the COOL language.
*
*/
%{
  #include <iostream>
  #include "cool-tree.h"
  #include "stringtab.h"
  #include "utilities.h"
  
  extern char *curr_filename;
  
  
  /* Locations */
  #define YYLTYPE int              /* the type of locations */
  #define cool_yylloc curr_lineno  /* use the curr_lineno from the lexer
  for the location of tokens */
    
    extern int node_lineno;          /* set before constructing a tree node
    to whatever you want the line number
    for the tree node to be */
      
      
      #define YYLLOC_DEFAULT(Current, Rhs, N)         \
      Current = Rhs[1];                             \
      node_lineno = Current;
    
    
    #define SET_NODELOC(Current)  \
    node_lineno = Current;
    
    /* IMPORTANT NOTE ON LINE NUMBERS
    *********************************
    * The above definitions and macros cause every terminal in your grammar to 
    * have the line number supplied by the lexer. The only task you have to
    * implement for line numbers to work correctly, is to use SET_NODELOC()
    * before constructing any constructs from non-terminals in your grammar.
    * Example: Consider you are matching on the following very restrictive 
    * (fictional) construct that matches a plus between two integer constants. 
    * (SUCH A RULE SHOULD NOT BE  PART OF YOUR PARSER):
    
    plus_consts	: INT_CONST '+' INT_CONST 
    
    * where INT_CONST is a terminal for an integer constant. Now, a correct
    * action for this rule that attaches the correct line number to plus_const
    * would look like the following:
    
    plus_consts	: INT_CONST '+' INT_CONST 
    {
      // Set the line number of the current non-terminal:
      // ***********************************************
      // You can access the line numbers of the i'th item with @i, just
      // like you acess the value of the i'th exporession with $i.
      //
      // Here, we choose the line number of the last INT_CONST (@3) as the
      // line number of the resulting expression (@$). You are free to pick
      // any reasonable line as the line number of non-terminals. If you 
      // omit the statement @$=..., bison has default rules for deciding which 
      // line number to use. Check the manual for details if you are interested.
      @$ = @3;
      
      
      // Observe that we call SET_NODELOC(@3); this will set the global variable
      // node_lineno to @3. Since the constructor call "plus" uses the value of 
      // this global, the plus node will now have the correct line number.
      SET_NODELOC(@3);
      
      // construct the result node:
      $$ = plus(int_const($1), int_const($3));
    }
    
    */
    
    void yyerror(char *s);        /*  defined below; called for each parse error */
    extern int yylex();           /*  the entry point to the lexer  */
    
    /************************************************************************/
    /*                DONT CHANGE ANYTHING IN THIS SECTION                  */
    
    Program ast_root;	            /* the result of the parse  */
    Classes parse_results;        /* for use in semantic analysis */
    int omerrs = 0;               /* number of errors in lexing and parsing */
    %}
    
    /* A union of all the types that can be the result of parsing actions. */
    %union {
      Boolean boolean;
      Symbol symbol;
      Program program;
      Class_ class_;
      Classes classes;
      Feature feature;
      Features features;
      Formal formal;
      Formals formals;
      Case case_;
      Cases cases;
      Expression expression;
      Expressions expressions;
      char *error_msg;
    }
    
    /* 
    Declare the terminals; a few have types for associated lexemes.
    The token ERROR is never used in the parser; thus, it is a parse
    error when the lexer returns it.
    
    The integer following token declaration is the numeric constant used
    to represent that token internally.  Typically, Bison generates these
    on its own, but we give explicit numbers to prevent version parity
    problems (bison 1.25 and earlier start at 258, later versions -- at
    257)
    */
    %token CLASS 258 ELSE 259 FI 260 IF 261 IN 262 
    %token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
    %token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
    %token <symbol>  STR_CONST 275 INT_CONST 276 
    %token <boolean> BOOL_CONST 277
    %token <symbol>  TYPEID 278 OBJECTID 279 
    %token ASSIGN 280 NOT 281 LE 282 ERROR 283
    
    /*  DON'T CHANGE ANYTHING ABOVE THIS LINE, OR YOUR PARSER WONT WORK       */
    /**************************************************************************/
    
    /* Complete the nonterminal list below, giving a type for the semantic
    value of each non terminal. (See section 3.6 in the bison 
    documentation for details). */
    
    /* Declare types for the grammar's non-terminals. 
       The defaults are based on section 6.1 on page 3 and 4 of Cool support code.
       Others are needed for my CFG.*/

    /* default non-terminals */
    %type <program> program

    %type <class_> class
    %type <classes> class_list

    %type <feature> feature
    %type <features> feature_list

    %type <formal> formal
    %type <formals> formal_list

    %type <expression> expression
    %type <expressions> expression_list

    /* my non-terminals */
   %type <formals> formal_helper

    %type <expressions> arguments
    %type <expressions> arguments_list

    %type <expression> let_identifier

    %type <case_> branch
    %type <cases> branch_list
    
    /* Precedence declarations go here. */
    /* I. According to section 11.1 on page 17 of the cool manual, 
          "All binary operations are left-associative, with the exception of assignment,
           which is right-associative, and the three comparison operations, which do not associate. 
       
      II. The 'LET_' is for let expressions, this will solve the 18 shift/reduce conflicts and
          shifts right until there are no more expressions to match for the body of the let expression
          and then would reduce.*/

    %right LET_
    %right ASSIGN
    %left NOT
    %nonassoc LE '<' '='  
    %left '+' '-'
    %left '*' '/'
    %left ISVOID 
    %left '~' 
    %left '@'
    %left '.'

    %%
    /* 
    Save the root of the abstract syntax tree in a global variable.
    */
    program	: class_list	
            { @$ = @1; ast_root = program($1); }
            ;
    
    /* 
    -------- Clases --------
    */
    class_list :  class			/* single class */
               { $$ = single_Classes($1); parse_results = $$; }

               |  class_list class	/* several classes */
               { $$ = append_Classes($1, single_Classes($2)); parse_results = $$; }
               ;
    
    /* If no parent is specified, the class inherits from the Object class. */
    class : CLASS TYPEID '{' feature_list '}' ';'
          { $$ = class_($2, idtable.add_string("Object"), $4, stringtable.add_string(curr_filename)); }
                      /*name,        parent,           features,            filename  */
          | CLASS TYPEID INHERITS TYPEID '{' feature_list '}' ';'
          { $$ = class_($2,$4,$6,stringtable.add_string(curr_filename)); }

          /* recovering from errors in class definitions (ans3) 
             I. If there is an error in a class definition but the class is terminated properly 
               and the next class is syntactically correct, the parser should be able to restart
               at the next class definition. 
            II. special token error is a terminal symbol that is always defined and reserved for error handling.
                In essence, error is like the .* pattern in flex. It recognizes everything until a specified delimiter
                is reached. Usually you have the error production read everything until the parse can get back to 
                a normal parsing state.  
           III. yyerrok is used as an action to solve the type clash warnings.  
                yyerrok says, ‘‘The old error is finished. If something else goes wrong, it is to be regarded as a new error.'’*/

	        /* As I figured in coolac parser, "class WRONG inherits WRONG/RIGHT {}" is valid,
              thus we should recover from it but "class WRONG {}" is invalid and the parser should
              halt. I implemented this behaviour. */
           | CLASS error INHERITS TYPEID '{' feature_list '}' ';' /* go to the next class */
	        { yyerrok; }
	        | CLASS TYPEID INHERITS error '{' feature_list '}' ';' /* go to the next class */
	        { yyerrok; }
	        | CLASS  error INHERITS error '{' feature_list '}' ';' /* go to the next class */
	        { yyerrok; }
	        | CLASS error '{' feature_list '}' ';' /* go to the next class */
	        { yyerrok; }
           ;

    /* 
    -------- Features --------
    */ 
    /* Feature list may be empty, but no empty features in list. 
       Features are the body of the class : attributes and methods */
    feature_list : feature_list feature
                 { $$ = append_Features($1, single_Features($2)); }

                 /* Feature list may be empty */
                 | /* empty */
                 {  $$ = nil_Features(); }
                 ;

              /* attribute */
    feature :  OBJECTID ':' TYPEID ';'  /* no init ~> xcar : Int; */
            { $$ = attr($1, $3, no_expr()); } /* no_expr for optional expressions */
            |  OBJECTID ':' TYPEID ASSIGN expression ';'  /* with init ~> xcar : Int <- 3 */
            { $$ = attr($1, $3, $5); }

             /* methods ~> init(hd : Int, tl : List) : Cons {...} ; */
            | OBJECTID '(' formal_list ')' ':' TYPEID '{' expression '}' ';'
            { $$ = method($1, $3, $6, $8);}

          /* recovering from errors in features */
	       /* go to the next method */
	         | OBJECTID '(' formal_list ')' ':'  error '{' expression '}' ';'
            { yyerrok; }
            | OBJECTID '('    error    ')' ':'  error '{' expression '}' ';'
            { yyerrok; }
            | error    '('    error    ')' ':'  error '{' expression '}' ';'
            { yyerrok; }

         /* go to the next attribute */ 
            | OBJECTID ':' TYPEID ASSIGN error ';'
            { yyerrok; }
            | OBJECTID ':' error  ASSIGN error ';'
            { yyerrok; }
            | error    ':' error  ASSIGN error ';'
            { yyerrok; }

            | OBJECTID ':' error ';'
            { yyerrok; }
            | error    ':' error ';'
            { yyerrok; }
                  ;    
    /* 
    -------- Formals --------
    */
    /* Formals list may be empty. formals ~> hd : Int, tl : List 
       These are the formal parameters (i.e. arguments) of methods 
       when we define them. */

       /* The point of using three rules is to avoid expressions such as
       " , tl : List " to be valid expressions. We do this by making sure that 
       the formal_list would either be empty or start with an non-empty formal.*/ 
    formal_list : formal formal_helper
                { $$ = append_Formals(single_Formals($1), $2) ; }
                | /* empty */
                {  $$ = nil_Formals(); }
                ;

    formal_helper : formal_helper ',' formal 
                  { $$ = append_Formals($1, single_Formals($3)); }
                  | /* empty */
                  {  $$ = nil_Formals(); }
                  ;

    /* formal ~> hd : Int, tl : List */
    formal : OBJECTID ':' TYPEID
           { $$ = formal($1, $3); }
           ;

    /* 
    -------- Expressions --------
    */
   /* Based on Figure 1 on page 16 of cool manual. 
    I. Expressions are the largest syntactic category in Cool.
   II. Constants, Identifiers, Assignment, Dispatch, 
       Conditionals, Loops, Blocks, Let, Case, New, Isvoid,
       and, Arithmetic and Comparison Operations. */

   expression_list: expression ';'
                  { $$ = single_Expressions($1); }
                  | expression_list  expression ';'
                  { $$ = append_Expressions($1, single_Expressions($2)); }
                  /* error handling */
                  | error ';' /* go to the next expression */
                  { yyerrok; }
                  ;

              /* Assignment ~> <id> <- <expr> */
   expression: OBJECTID ASSIGN expression
             { $$ = assign($1, $3); }

             /* Dispatch ~> <expr>.<id>(<expr>,...,<expr>)
                            <id>(<expr>,...,<expr>)  [shorthand for self.<id>(<expr>,...,<expr>)].
                            <expr>@<type>.id(<expr>,...,<expr>) */
             | expression '.' OBJECTID '(' arguments_list ')'
             { $$ = dispatch($1, $3, $5); }
             | OBJECTID '(' arguments_list ')'
             { $$ = dispatch( object( idtable.add_string("self") ), $1, $3); }
             | expression '@' TYPEID '.' OBJECTID '(' arguments_list ')'
             { $$ = static_dispatch($1, $3, $5, $7); }

             /* Conditionals ~> if <expr> then <expr> else <expr> fi */
             | IF expression THEN expression ELSE expression FI
             { $$ = cond($2, $4, $6); }

             /* Loops ~> while <expr> loop <expr> pool */
             | WHILE expression LOOP expression POOL
             { $$ = loop($2, $4); }

             /* Blocks ~> {<expr>; ... <expr>;} */
             | '{' expression_list '}'
             { $$ = block($2); }

             /* Let ~> let <id1> : <type1> [ <- <expr1> ],
                           ...,
                           <idn> : <typen> [ <- <exprn> ] 
                       in <expr> */
             | LET let_identifier
             { $$ = $2; }

             /* Case ~> case <expr0> of
                            <id1> : <type1> => <expr1>; ...
                            <idn> : <typen> => <exprn>;
                        esac */
             | CASE expression OF branch_list ESAC 
             { $$ = typcase($2, $4); }
	     /* recover from error in expression of case */
             | CASE error OF branch_list ESAC 
             { yyerrok; }

             /* New ~> new <type> */
             | NEW TYPEID
             { $$ = new_($2); }

             /* Isvoid ~> isvoid expr */
             | ISVOID expression
             { $$ = isvoid($2); }

             /* Arithmetic and Comparison ~> expr1 <op> expr2 */
             /* + */
             | expression '+' expression
             { $$ = plus($1, $3); }
             /* - */
             | expression '-' expression
             { $$ = sub($1, $3); }
             /* * */
             | expression '*' expression
             { $$ = mul($1, $3); }
             /* / */
             | expression '/' expression
             { $$ = divide($1, $3); }
             /* ~ */
             | '~' expression
             { $$ = neg($2); }
             /* < */
             | expression '<' expression
             { $$ = lt($1, $3); }
             /* <= */
             | expression LE expression
             { $$ = leq($1, $3); }
             /* = */
             | expression '=' expression
             { $$ = eq($1, $3); }
             /* not */
             | NOT expression
             { $$ = comp($2); }

             /* ( ) */
             | '(' expression ')'
             { $$ = $2; }

             /* => Identifiers <= */
             | OBJECTID 
             { $$ = object($1);}

             /* Literals */
             /* int */
             | INT_CONST
             { $$ = int_const($1);}
             /* int */
             | BOOL_CONST
             { $$ = bool_const($1);}
             /* int */
             | STR_CONST
             { $$ = string_const($1);}
             ;

    /* This is for handeling let expressions. 
            <id1> : <type1> [<- <expr1>] in <expr> 
                  OR
            <id1> : <type1> [<- <expr1>], <another let expr>  

        Since "LET_" is defined to be right associative, it would group 
        the let expressions from right to left, also it has the highest
        priority for a shift move, so in case of a shift/reduce conflict
        it would shift instead of reducing. This way it can extend the 
        body expression as much as possible.
    */ 
    let_identifier: OBJECTID ':' TYPEID ASSIGN expression IN expression          %prec LET_
                  { $$ = let($1, $3, $5, $7); } 
                  | OBJECTID ':' TYPEID IN expression                            %prec LET_
                  { $$ = let($1, $3, no_expr(), $5); } 
                  | OBJECTID ':' TYPEID ASSIGN expression ',' let_identifier    
                  { $$ = let($1, $3, $5, $7); } 
                  | OBJECTID ':' TYPEID ',' let_identifier                   
                  { $$ = let($1, $3, no_expr(), $5); } 

                  /* error handling */
 		  /* go to the next variable */
                  | OBJECTID ':' TYPEID ASSIGN error ',' let_identifier
                  { yyerrok; }
                  | OBJECTID ':' error  ASSIGN error ',' let_identifier
                  { yyerrok; }
                  |  error   ':' error  ASSIGN error ',' let_identifier
                  { yyerrok; }
		  | OBJECTID ':' error ',' let_identifier
                  { yyerrok; }
		  |  error   ':' error ',' let_identifier
                  { yyerrok; }
                  ;

    /* This is for the arguments in each method call. 
       ~> <expr>,...,<expr> 
       The point of using two rules is to avoid expressions such as
       ",<exp>" to be valid expressions. We do this by making sure that 
       the arguments_list would either be empty or start with an non-empty expression. */
    arguments_list : expression arguments
                   { $$ = append_Expressions(single_Expressions($1), $2); }
                   | /* empty */
                   { $$ = nil_Expressions(); }
                   ;
    arguments : arguments ',' expression 
              { $$ = append_Expressions($1, single_Expressions($3)); }
              | /* empty */
              { $$ = nil_Expressions(); }
              ;
   
    /* This is for branches in a case expression.
       ~> <id1> : <type1> => <expr1>; 
           ...
          <idn> : <typen> => <exprn>; */
    branch_list: branch 
               { $$ = single_Cases($1); }

               | branch_list branch
               { $$ = append_Cases($1, single_Cases($2)); }
               ;

    branch: OBJECTID ':' TYPEID DARROW expression ';'
          { $$ = branch($1, $3, $5); }
                         
          /* error handling */
          | OBJECTID ':' TYPEID DARROW error ';' /* go to the next branch */
          { yyerrok; }
          | OBJECTID ':' error  DARROW expression ';' /* go to the next branch */
          { yyerrok; }
          | OBJECTID ':' error  DARROW error ';' /* go to the next branch */
          { yyerrok; }
          |  error   ':' error  DARROW error ';' /* go to the next branch */
          { yyerrok; }
          ;
    
    /* end of grammar */
    %%
    
    /* This function is called automatically when Bison detects a parse error. */
    void yyerror(char *s)
    {
      extern int curr_lineno;
      
      cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
      << s << " at or near ";
      print_cool_token(yychar);
      cerr << endl;
      omerrs++;
    
      if(omerrs>50) {fprintf(stdout, "More than 50 errors\n"); exit(1);}
    }
    