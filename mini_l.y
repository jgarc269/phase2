%{
 #include <stdio.h>
 #include <stdlib.h>
 void yyerror(const char *msg);
 extern int currLine;
 extern int currPos;
 FILE * yyin;
%}

%union{
  char* ival;
  double dval;
}

%start prog_start
%token <ival> IDENT
%token <dval> NUMBER


%token FUNCTION
%token BEGIN_PARAMS
%token END_PARAMS
%token BEGIN_LOCALS
%token END_LOCALS
%token BEGIN_BODY
%token END_BODY
%token INTEGER
%token ARRAY
%token OF
%token IF
%token THEN
%token ENDIF
%token ELSE
%token WHILE
%token DO
%token FOR
%token BEGINLOOP
%token ENDLOOP
%token CONTINUE
%token READ
%token WRITE
%token TRUE
%token FALSE
%token RETURN
%token SEMICOLON
%token COLON
%token COMMA
%token L_PAREN
%token R_PAREN
%token L_SQUARE_BRACKET
%token R_SQUARE_BRACKET

%left AND
%left OR
%left SUB
%left ADD
%left MULT
%left DIV
%left MOD
%left EQ
%left NEQ
%left LT
%left GT
%left LTE
%left GTE

%right ASSIGN
%right NOT
%right UMINUS

%%

/* Grammer Rules */

prog_start:		Function pro_start {printf("pro_start -> Function pro_start\n");} 
				| Function {printf("pro_start -> Function\n");} 
            	| {printf("prog_start -> EPSILON\n");}
            	;

Function: 		/*empty*/ {printf("Function -> EPSILON/n");}
				| FUNCTION IDENT SEMICOLON BEGIN_PARAMS Declaration SEMICOLON END_PARAMS BEGIN_LOCALS Declaration SEMICOLON END_LOCALS BEGIN_BODY Statement SEMICOLON ENDBODY{printf("Function -> FUNCTION IDENT SEMICOLON BEGIN_PARAMS Declaration END_PARAMS BEGIN_LOCALS Declaration END_LOCALS BEGIN_BODY Statement END_BODY\n");}
				;

Declaration:	/*empty*/ {printf("Declaration -> EPSILON/n");}
				| IDENT COLON INTEGER {printf("Declaration -> IDENT COLON INTEGER/n");}
				| IDENT COMMA IDENT COLON INTEGER {printf("Declaration -> IDENT COMMA IDENT COLON INTEGER/n");}
				| IDENT COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER {printf("Declaration -> IDENT COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER/n");}
				| IDENT COMMA IDENT COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER {printf("Declaration -> IDENT COMMA IDENT COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER/n");}

				| Declaration SEMICOLON Declaration {printf("Declaration -> Declaration SEMICOLON Declaration/n");}
				;

Statement:		Var ASSIGN Expression {printf("Statement -> var ASSIGN Expression/n");}

				| IF Bool-Expr THEN Statement SEMICOLON ENDIF {printf("Statement -> IF bool-exp THEN Statement SEMICOLON ENDIF/n");}
				| IF Bool-Expr THEN Statement SEMICOLON ELSE Statement SEMICOLON ENDIF {printf("Statement -> IF bool-exp THEN Statement SEMICOLON ELSE Statement SEMICOLON ENDIF/n");}
				
				| WHILE Bool-Expr BEGINLOOP Statement SEMICOLON ENDLOOP {printf("Statement -> WHILE Bool-Expr BEGINLOOP Statement SEMICOLON ENDLOOP/n");}
				
				| DO BEGINLOOP Statement SEMICOLON ENDLOOP WHILE Bool-Expr {printf("Statement -> DO BEGINLOOP Statement SEMICOLON ENDLOOP WHILE Bool-Expr/n");}

				| FOR Var ASSIGN NUMBER SEMICOLON Bool-Expr SEMICOLON Var ASSIGN Expression BEGINLOOP Statement SEMICOLON ENDLOOP {printf("Statement -> FOR Var ASSIGN NUMBER SEMICOLON Bool-Expr SEMICOLON Var ASSIGN Expression BEGINLOOP Statement SEMICOLON ENDLOOP/n");}

				| READ Var {printf("Statement -> WRITE Var/n");}
				| WRITE Var {printf("Statement -> WRITE Var/n");}
				| CONTINUE {printf("Statement -> CONTINUE/n");}
				| RETURN Expression {printf("Statement -> Expression/n");}

				| Statement SEMICOLON Statement {printf("Statement -> Statement SEMICOLON Statement/n");}
				; 

Bool-Expr:		/*empty*/ {printf("Bool-Expr -> EPSILON/n");}
				| Relation-And-Expr {printf("Bool-Expr -> Relation-And-Expr/n");}
				| Relation-And-Expr OR Relation-And-Expr {printf("Bool-Expr -> Relation-And-Expr OR Relation-And-Expr/n");}
				;

Relation-And-Expr:		/*empty*/ {printf("Relation-And-Expr -> EPSILON/n");}
						| Relation-Expr {printf("Relation-And-Expr -> Relation-Expr/n");}
						| Relation-Expr AND Relation-Expr {printf("Relation-And-Expr -> Relation-Expr AND Relation-And-Expr/n");}
						;

Relation-Expr:		Expression Comp Expression {printf("Relation-Expr -> Expression Comp Expression/n");}
					| TRUE {printf("Relation-Expr -> TRUE/n");}
					| FALSE {printf("Relation-Expr -> FALSE/n");}
					| L_PAREN Bool-Expr R_PAREN {printf("Relation-Expr -> L_PAREN Bool-Expr R_PAREN/n");}
					| NOT Expression Comp Expression {printf("Relation-Expr -> NOT Expression Comp Expression /n");}
					| NOT TRUE {printf("Relation-Expr -> NOT TRUE/n");}
					| NOT FALSE {printf("Relation-Expr -> NOT FALSE/n");}
					| NOT L_PAREN Bool-Expr R_PAREN {printf("Relation-Expr -> NOT L_PAREN Bool-Expr R_PAREN/n");}
					;
		
Comp: 		EQ {printf("Comp -> EQ/n");}
			| NEQ {printf("Comp -> NEQ/n");}
			| LT {printf("Comp -> LT/n");}
			| GT {printf("Comp -> GT/n");}
			| LTE {printf("Comp -> LTE/n");}
			| GTE {printf("Comp -> GTE/n");}
			;

Expression: 		/*empty*/ {printf("Expression -> EPSILON/n");}
					| Multiplicative-Expr {printf("Expression -> Multiplicative-Expr/n");}
					| Multiplicative-Expr SUB Multiplicative-Expr {printf("Expression -> Multiplicative-Expr SUB Multiplicative-Expr/n");}
					| Multiplicative-Expr ADD Multiplicative-Expr {printf("Expression -> Multiplicative-Expr ADD Multiplicative-Expr/n");}
					
					| Expression COMMA Expression {printf("Expression -> COMMA Expression/n");}
					;

Multiplicative-Expr:		/*empty*/ {printf("Multiplicative-Expr -> EPSILON/n");}
							| Term {printf("Multiplicative-Expr -> Term/n");}
							| Term MOD Term {printf("Multiplicative-Expr -> Term MOD Term/n");}
							| Term DIV Term {printf("Multiplicative-Expr -> Term MOD Term/n");}
							| Term MULT Term {printf("Multiplicative-Expr -> Term MOD Term/n");} 
							;

Term:		/*empty*/ {printf("Term -> EPSILON/n");}
			| Var {printf("Term -> Var/n");}
			| NUMBER {printf("Term -> NUMBER/n");}
			| L_PAREN Expression R_PAREN {printf("Term -> L_PAREN Expression R_PAREN/n");}
			| UMINUS Var {printf("Term -> UMINUS Var/n");}
			| UMINUS NUMBER {printf("Term -> UMINUS NUMBER/n");}
			| UMINUS L_PAREN Expression R_PAREN {printf("Term -> UMINUS L_PAREN Expression R_PAREN/n");}
			| IDENT L_PAREN Expression R_PAREN {printf("Term -> IDENT L_PAREN Expression R_PAREN/n");}
			;

Var:		/*empty*/ {printf("Var -> EPSILON/n");}
			| IDENT {printf("Var -> IDENT/n");}
			| IDENT L_PAREN Expression R_PAREN {printf("Var -> IDENT L_PAREN Expression R_PAREN/n");}
			| Var COMMA Var {printf("Var -> Var COMMA/n");}
			;

Ident:      IDENT
			{printf("Ident -> IDENT %s \n", $1);}
%%

void yyerror(const char *msg) {
   printf("** Line %d, position %d: %s\n", currLine, currPos, msg);
}
