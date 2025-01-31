%{
#include <string>
#include "node.h"
#include "parser.hpp"

#define SAVE_TOKEN  yylval.string = new std::string(yytext, yyleng)
#define TOKEN(t)    (yylval.token = t)

void yyerror(char *);
%}

%option noyywrap

%x	COMMENT COMMENT_END

%%
 /* Single-line comment */
\/\/.*                          ;

 /* Multiline comment */
\/\*				            BEGIN COMMENT;
<COMMENT>[^\*]
<COMMENT>\*			            BEGIN COMMENT_END;
<COMMENT_END>[^\/]	            BEGIN COMMENT;
<COMMENT_END>\/		            BEGIN 0;

 /* Whitespaces */
[ \t\n]					        ;

"if"                            return TOKEN(IF);
"else"                          return TOKEN(ELSE);
"while"                         return TOKEN(WHILE);
"extern"                        return TOKEN(TEXTERN);
"return"				        return TOKEN(TRETURN);
[a-zA-Z_][a-zA-Z0-9_]*  SAVE_TOKEN; return TIDENTIFIER;
[0-9]+\.[0-9]* 			    SAVE_TOKEN; return TDOUBLE;
[0-9]+					        SAVE_TOKEN; return TINTEGER;

"="						          return TOKEN(TEQUAL);
"=="				          	return TOKEN(TCEQ);
"!="			          		return TOKEN(TCNE);
"<"				          		return TOKEN(TCLT);
"<="	          				return TOKEN(TCLE);
">"				          		return TOKEN(TCGT);
">="					          return TOKEN(TCGE);

"&&"					        return TOKEN(TLOGAND);
"||"					        return TOKEN(TLOGOR);
"!"					            return TOKEN(TLOGNOT);


"("	          					return TOKEN(TLPAREN);
")"					          	return TOKEN(TRPAREN);
"{"         						return TOKEN(TLBRACE);
"}"					          	return TOKEN(TRBRACE);

"."         						return TOKEN(TDOT);
","				          		return TOKEN(TCOMMA);
";"				          		return TOKEN(TSEMICOLN);

"+"				          		return TOKEN(TPLUS);
"-"		          				return TOKEN(TMINUS);
"*"		          				return TOKEN(TMUL);
"/"				          		return TOKEN(TDIV);

.                      yyerror("invalid character"); yyterminate();

%%

void yyerror(char *s) {
    fprintf(stderr, "line %d: %s\n", yylineno, s);
}
