%{
	#include "node.h"
    #include <cstdio>
    #include <cstdlib>
	NBlock *programBlock; /* the top level root node of our final AST */

	extern int yylex();
	void yyerror(const char *s) { std::printf("Error: %s\n", s);std::exit(1); }
%}

/* Represents the many different ways we can access our data */
%union {
	Node *node;
	NBlock *block;
	NExpression *expr;
	NStatement *stmt;
	NIdentifier *ident;
	NVariableDeclaration *var_decl;
	std::vector<NVariableDeclaration*> *varvec;
	std::vector<NExpression*> *exprvec;
	std::string *string;
	int token;
}

/* Define our terminal symbols (tokens). This should
   match our tokens.l lex file. We also define the node type
   they represent.
 */
%token <string> TIDENTIFIER TINTEGER TDOUBLE
%token <token> TCEQ TCNE TCLT TCLE TCGT TCGE TEQUAL
%token <token> TLOGAND TLOGOR TLOGNOT
%token <token> TLPAREN TRPAREN TLBRACE TRBRACE TCOMMA TDOT
%token <token> TPLUS TMINUS TMUL TDIV
%token <token> TRETURN TEXTERN
%token <token> TSEMICOLN IF ELSE WHILE

%nonassoc IFX
%nonassoc ELSE

/* Define the type of node our nonterminal symbols represent.
   The types refer to the %union declaration above. Ex: when
   we call an ident (defined by union type ident) we are really
   calling an (NIdentifier*). It makes the compiler happy.
 */
%type <ident> ident
%type <expr> numeric expr 
%type <varvec> func_decl_args
%type <exprvec> call_args
%type <block> program stmts block
%type <stmt> stmt var_decl func_decl extern_decl conditional loop

/* Operator precedence */
%right TEQUAL
%right TLOGNOT
%left TLOGOR
%left TLOGAND
%left TCEQ TCNE
%left TCLT TCLE TCGT TCGE 
%left TPLUS TMINUS
%left TMUL TDIV

%nonassoc UMINUS

%start program

%%

program : stmts { programBlock = $1; }
		;
		
stmts : stmt { $$ = new NBlock(); $$->statements.push_back($<stmt>1); }
	  | stmts stmt { $1->statements.push_back($<stmt>2); }
	  ;

stmt : var_decl TSEMICOLN
	 | func_decl 
	 | extern_decl
	 | conditional
	 | loop
	 | expr TSEMICOLN { $$ = new NExpressionStatement(*$1); }
	 | TRETURN expr TSEMICOLN { $$ = new NReturnStatement(*$2); }
     ;

block : TLBRACE stmts TRBRACE { $$ = $2; }
	  | TLBRACE TRBRACE { $$ = new NBlock(); }
	  ;

var_decl : ident ident { $$ = new NVariableDeclaration(*$1, *$2); }
		 | ident ident TEQUAL expr { $$ = new NVariableDeclaration(*$1, *$2, $4); }
		 ;

conditional:
    IF TLPAREN expr TRPAREN block %prec IFX { $$ = new NConditionalStatement(*$3, *$5, * new NBlock()); }
    | IF TLPAREN expr TRPAREN block ELSE block { $$ = new NConditionalStatement(*$3, *$5, *$7); }
	;
    
loop: 
	WHILE TLPAREN expr TRPAREN block { $$ = new NLoopStatement(*$3, *$5); }
	;

extern_decl : TEXTERN ident ident TLPAREN func_decl_args TRPAREN TSEMICOLN
                { $$ = new NExternDeclaration(*$2, *$3, *$5); delete $5; }
            ;

func_decl : ident ident TLPAREN func_decl_args TRPAREN block 
			{ $$ = new NFunctionDeclaration(*$1, *$2, *$4, *$6); delete $4; }
		  ;
	
func_decl_args : /*blank*/  { $$ = new VariableList(); }
		  | var_decl { $$ = new VariableList(); $$->push_back($<var_decl>1); }
		  | func_decl_args TCOMMA var_decl { $1->push_back($<var_decl>3); }
		  ;

ident : TIDENTIFIER { $$ = new NIdentifier(*$1); delete $1; }
	  ;

numeric : TINTEGER { $$ = new NInteger(atol($1->c_str())); delete $1; }
		| TDOUBLE { $$ = new NDouble(atof($1->c_str())); delete $1; }
		;
	
expr:
	numeric
	| ident { $<ident>$ = $1; }
    | TLPAREN expr TRPAREN { $$ = $2; }
	| TLOGNOT expr { $$ = new NUnaryOperator($1, *$2); }
	| TMINUS expr %prec UMINUS { $$ = new NUnaryOperator($1, *$2); }
	| expr TPLUS expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
	| expr TMINUS expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
	| expr TMUL expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
	| expr TDIV expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
	| expr TCEQ expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
	| expr TCNE expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
	| expr TCLT expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
	| expr TCLE expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
	| expr TCGT expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
	| expr TCGE expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
	| expr TLOGOR expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
	| expr TLOGAND expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
	| ident TEQUAL expr { $$ = new NAssignment(*$<ident>1, *$3); }
	| ident TLPAREN call_args TRPAREN { $$ = new NMethodCall(*$1, *$3); delete $3; }
	;
	
call_args : /*blank*/  { $$ = new ExpressionList(); }
		  | expr { $$ = new ExpressionList(); $$->push_back($1); }
		  | call_args TCOMMA expr  { $1->push_back($3); }
		  ;


%%
