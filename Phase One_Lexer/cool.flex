/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* a char array to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
int commentMatch  = 0;
int append(char newChar);

%}

/*
 * Start conditions
 */
%x string
%x oneLineComment
%x nestedComment
%x longString
%x badString

/*
 * Define names for regular expressions here.
 */
DIGIT               [0-9]
CHAR_LOW            [a-z]
CHAR_UP             [A-Z]
CHAR                ({CHAR_LOW}|{CHAR_UP})
UNDERSCORE           "_"
NEW_LINE             "\n"
WHITE_SPACE         (" "|"\f"|"\r"|"\t"|"\v")

/* special syntactic symbols definitions */
SYMBOLS  ("."|"@"|"~"|"*"|"/"|"+"|"-"|"<"|"="|"("|")"|"{"|"}"|","|":"|";")
DARROW_ "=>"
ASSIGN_ "<-"
LE_     "<="

/* Keywords definitions */
CLASS_    (?i:CLASS)
ELSE_     (?i:ELSE)
FI_       (?i:FI)
IF_       (?i:IF)
IN_       (?i:IN)
INHERITS_ (?i:INHERITS)
LET_      (?i:LET)
LOOP_     (?i:LOOP)
POOL_     (?i:POOL)
THEN_     (?i:THEN)
WHILE_    (?i:WHILE)
CASE_     (?i:CASE)
ESAC_     (?i:ESAC)
OF_       (?i:OF)
NEW_      (?i:NEW)
ISVOID_   (?i:ISVOID)
NOT_      (?i:NOT)

/* Constants definitions */
BOOL_CONST_T  "t"(?i:RUE)
BOOL_CONST_F  "f"(?i:ALSE)
INT_CONST_ {DIGIT}+

/* Identifiers definitions */
TYPEID_    (("SELF_TYPE")|({CHAR_UP}+({CHAR}|{DIGIT}|{UNDERSCORE})*))
OBJECTID_  (("self")|({CHAR_LOW}+({CHAR}|{DIGIT}|{UNDERSCORE})*))

%%
 /*
  *  One line comments
  *  Any characters between two dashes “--” and the next newline 
  *  (or EOF, if there is no next newline) are treated as comments.
  */
"--"  BEGIN(oneLineComment);
<oneLineComment>{\n   { curr_lineno++; BEGIN(INITIAL); }
                 .    {}
}
   
 /*
  *  Nested comments
  *  Comments may also be written by enclosing text in (∗ ... ∗) 
  *  and may be nested.
  */
"*)"  { cool_yylval.error_msg = "Unmatched *)"; return ERROR; }
"(*"  { BEGIN(nestedComment); commentMatch++; }
<nestedComment>{\n      curr_lineno++;
                "(*"    commentMatch++;
                "*)"    {commentMatch--;
                        if (commentMatch<0){
                          cool_yylval.error_msg = "Unmatched *)";
                          return ERROR;
                          BEGIN(INITIAL);}
                        else if (commentMatch==0){
                          BEGIN(INITIAL);}}
                <<EOF>> {cool_yylval.error_msg = "EOF in comment";
                         BEGIN(INITIAL);
                         return ERROR;}
                .       {}
}

 /*
  *  The single-character operators.
  */
{SYMBOLS}		{ return (*yytext); }

 /*
  *  The multiple-character operators.
  */
{DARROW_}		{ return (DARROW); }
{ASSIGN_}		{ return (ASSIGN); }
{LE_}		    { return (LE); }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
{CLASS_}    { return (CLASS); }
{ELSE_}     { return (ELSE); }
{FI_}       { return (FI); }
{IF_}       { return (IF); }
{IN_}       { return (IN); }
{INHERITS_} { return (INHERITS); }
{LET_}      { return (LET); }
{LOOP_}     { return (LOOP); }
{POOL_}     { return (POOL); }
{THEN_}     { return (THEN); }
{WHILE_}    { return (WHILE); }
{CASE_}     { return (CASE); }
{ESAC_}     { return (ESAC); }
{OF_}       { return (OF); }
{NEW_}      { return (NEW); }
{ISVOID_}   { return (ISVOID); }
{NOT_}      { return (NOT); }

 /*
  *  Integer constants
  */
{INT_CONST_}   { cool_yylval.symbol = idtable.add_string(yytext);
                 return (INT_CONST);}
 /*
  *  Boolean constants
  */
{BOOL_CONST_T} { cool_yylval.boolean = true;
	               return (BOOL_CONST);}
{BOOL_CONST_F} { cool_yylval.boolean = false;
	               return (BOOL_CONST);}

 /*
  *  Type and object identifiers
  */
{TYPEID_}      { cool_yylval.symbol = idtable.add_string(yytext);
                 return (TYPEID);}
{OBJECTID_}    { cool_yylval.symbol = idtable.add_string(yytext);
                 return (OBJECTID);}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */
"\""  {string_buf_ptr = string_buf; BEGIN(string);} 
<string>{"\""   { *string_buf_ptr = '\0';
                  BEGIN(INITIAL);
                  cool_yylval.symbol = stringtable.add_string(string_buf);
                  return (STR_CONST); }

        <<EOF>> { BEGIN(INITIAL); 
                  cool_yylval.error_msg = "EOF in string constant";
                  return (ERROR); }

        "\\?\0" { BEGIN(badString);
		  cool_yylval.error_msg = "String contains null character";
                  return (ERROR); }

        "\n"    { BEGIN(INITIAL);
                  curr_lineno++;
                  cool_yylval.error_msg = "Unterminated string constant";  
                  return ERROR; }

        "\\n"   {append('\n');}
        "\\\n"  {append('\n'); curr_lineno++;}
        "\\t"    append('\t');
        "\\b"    append('\b');
        "\\f"    append('\f');

        \\[^ntbf0]  append(yytext[1]);
        .           append(yytext[0]);
}     
  
<longString>{"\"" { BEGIN(INITIAL); 
		    cool_yylval.error_msg = "String constant too long";
    		    return ERROR; }
            \n    { curr_lineno++; 
		    BEGIN(INITIAL); 
                    cool_yylval.error_msg = "String constant too long";
    		    return ERROR; }
	  \\\n      curr_lineno++;
            .     {}
}
<badString>{"\""  BEGIN(INITIAL);
            \n    { curr_lineno++; BEGIN(INITIAL); }
	  \\\n      curr_lineno++;
            .     {}
}

 /*
  * Whitespaces would be ignored unless it's /n (new line).
  */
{NEW_LINE}      { curr_lineno++; }
{WHITE_SPACE}+  {}

 /*
  * Invalid character (one that can’t begin any token)
  * a string containing just that character should be returned as the error string.
  */
.      { cool_yylval.error_msg = yytext;
			   return (ERROR); }
%%

int append(char newChar){
  if (string_buf_ptr+1 > &string_buf[MAX_STR_CONST-1]){
    BEGIN(longString);}
  else{
    *string_buf_ptr = newChar;
    string_buf_ptr++;
    return 0;}
}