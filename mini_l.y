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


prog_start:	Function prog_start {printf("prog_start -> Functions\n");}
			| /*empty*/ {printf("program -> EPSILON\n");}
			;

Function: 		FUNCTION Ident SEMICOLON BEGIN_PARAMS Declaration_loop SEMICOLON END_PARAMS BEGIN_LOCALS Declaration_loop SEMICOLON END_LOCALS BEGIN_BODY Statement SEMICOLON END_BODY{printf("Function -> FUNCTION Ident SEMICOLON BEGIN_PARAMS Declaration_loop END_PARAMS BEGIN_LOCALS Declaration_loop END_LOCALS BEGIN_BODY Statement END_BODY\n");}
				;

Declaration_loop: 	Declaration SEMICOLON Declaration_loop {printf("Declaration_loop -> Declaration SEMICOLON Declaration_loop\n");}
					;

Declaration:	/*empty*/ {printf("Declaration -> EPSILON\n");}
				| Ident COLON INTEGER {printf("Declaration -> Ident COLON INTEGER\n");}
				| Ident COMMA Ident COLON INTEGER {printf("Declaration -> Ident COMMA Ident COLON INTEGER\n");}
				| Ident COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER {printf("Declaration -> Ident COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER\n");}
				| Ident COMMA Ident COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER {printf("Declaration -> Ident COMMA Ident COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER\n");}

				| Declaration SEMICOLON Declaration {printf("Declaration -> Declaration SEMICOLON Declaration\n");}
				;

Statement:		Var ASSIGN Expression {printf("Statement -> var ASSIGN Expression\n");}

				| IF Bool-Expr THEN Statement_loop SEMICOLON ENDIF {printf("Statement -> IF bool-exp THEN Statement SEMICOLON ENDIF\n");}
				| IF Bool-Expr THEN Statement_loop SEMICOLON ELSE Statement_loop SEMICOLON ENDIF {printf("Statement -> IF bool-exp THEN Statement_loop SEMICOLON ELSE Statement_loop SEMICOLON ENDIF\n");}
				
				| WHILE Bool-Expr BEGINLOOP Statement_loop SEMICOLON ENDLOOP {printf("Statement -> WHILE Bool-Expr BEGINLOOP Statement_loop SEMICOLON ENDLOOP\n");}
				
				| DO BEGINLOOP Statement_loop SEMICOLON ENDLOOP WHILE Bool-Expr {printf("Statement -> DO BEGINLOOP Statement_loop SEMICOLON ENDLOOP WHILE Bool-Expr\n");}

				| FOR Var ASSIGN NUMBER SEMICOLON Bool-Expr SEMICOLON Var ASSIGN Expression BEGINLOOP Statement_loop SEMICOLON ENDLOOP {printf("Statement -> FOR Var ASSIGN NUMBER SEMICOLON Bool-Expr SEMICOLON Var ASSIGN Expression BEGINLOOP Statement_loop SEMICOLON ENDLOOP\n");}

				| READ Var {printf("Statement -> READ Var\n");}
				| WRITE Var {printf("Statement -> WRITE Var\n");}
				| CONTINUE {printf("Statement -> CONTINUE\n");}
				| RETURN Expression {printf("Statement -> Expression\n");}
				; 

Statement_loop: 	Statement SEMICOLON Statement_loop {printf("Statement_loop -> Statement SEMICOLON Statement_loop\n");}
					| Statement SEMICOLON {printf("Statement_loop -> Statement SEMICOLON\n");}
					;

Bool-Expr:		Relation-And-Expr Bool-Expr-loop {printf("Bool-Expr -> Relation-And-Expr\n");}
				;

Bool-Expr-loop: 		OR Relation-And-Expr Bool-Expr-loop {printf("Bool-Expr -> Relation-And-Expr OR Relation-And-Expr\n");}
						| /*empty*/ {printf("Bool-Expr-loop -> EPSILON\n");}
						;

Relation-And-Expr:		Relation-Expr Relation-And-Expr-loop{printf("Relation-And-Expr -> Relation-Expr\n");}
						;

Relation-And-Expr-loop: 		AND Relation-Expr Relation-And-Expr-loop {printf("Relation-And-Expr-loop -> AND Relation-Expr Relation-And-Expr-loop\n");}
								|/*empty*/ {printf("Relation-And-Expr-loop -> EPSILON\n");}
								;

Relation-Expr:		Expression Comp Expression {printf("Relation-Expr -> Expression Comp Expression\n");}
					| TRUE {printf("Relation-Expr -> TRUE\n");}
					| FALSE {printf("Relation-Expr -> FALSE\n");}
					| L_PAREN Bool-Expr R_PAREN {printf("Relation-Expr -> L_PAREN Bool-Expr R_PAREN\n");}
					| NOT Expression Comp Expression {printf("Relation-Expr -> NOT Expression Comp Expression \n");}
					| NOT TRUE {printf("Relation-Expr -> NOT TRUE\n");}
					| NOT FALSE {printf("Relation-Expr -> NOT FALSE\n");}
					| NOT L_PAREN Bool-Expr R_PAREN {printf("Relation-Expr -> NOT L_PAREN Bool-Expr R_PAREN\n");}
					;
		
Comp: 		EQ {printf("Comp -> EQ\n");}
			| NEQ {printf("Comp -> NEQ\n");}
			| LT {printf("Comp -> LT\n");}
			| GT {printf("Comp -> GT\n");}
			| LTE {printf("Comp -> LTE\n");}
			| GTE {printf("Comp -> GTE\n");}
			;

Expression: 		/*empty*/ {printf("Expression -> EPSILON\n");}
					| Multiplicative-Expr {printf("Expression -> Multiplicative-Expr\n");}
					| Multiplicative-Expr SUB Multiplicative-Expr {printf("Expression -> Multiplicative-Expr SUB Multiplicative-Expr\n");}
					| Multiplicative-Expr ADD Multiplicative-Expr {printf("Expression -> Multiplicative-Expr ADD Multiplicative-Expr\n");}
					;

Expression_loop: 	Expression COMMA Expression_loop {printf("Expression -> COMMA Expression\n");}
					| /*empty*/ {printf("Expression_loop-Expr -> EPSILON\n");};

Multiplicative-Expr:	Term Multiplicative-Expr-loop {printf("Multiplicative-Expr-> Multiplicative-Expr-loop\n");};

Multiplicative-Expr-loop:		/*empty*/ {printf("Multiplicative-Expr-loop -> EPSILON\n");}
							| MOD Term Multiplicative-Expr-loop {printf("Multiplicative-Expr-loop -> Term MOD Term\n");}
							| DIV Term Multiplicative-Expr-loop {printf("Multiplicative-Expr-loop -> Term MOD Term\n");}
							| MULT Term Multiplicative-Expr-loop {printf("Multiplicative-Expr-loop -> Term MOD Term\n");} 
							;

Term:		/*empty*/ {printf("Term -> EPSILON\n");}
			| Var {printf("Term -> Var\n");}
			| NUMBER {printf("Term -> NUMBER\n");}
			| L_PAREN Expression R_PAREN {printf("Term -> L_PAREN Expression R_PAREN\n");}
			| UMINUS Var {printf("Term -> UMINUS Var\n");}
			| UMINUS NUMBER {printf("Term -> UMINUS NUMBER\n");}
			| UMINUS L_PAREN Expression R_PAREN {printf("Term -> UMINUS L_PAREN Expression R_PAREN\n");}
			| Ident L_PAREN Expression_loop R_PAREN {printf("Term -> Ident L_PAREN Expression R_PAREN\n");}
			;

Var:		/*empty*/ {printf("Var -> EPSILON\n");}
			| Ident {printf("Var -> Ident\n");}
			| Ident L_PAREN Expression R_PAREN {printf("Var -> Ident L_PAREN Expression R_PAREN\n");}
			| Var COMMA Var {printf("Var -> Var COMMA\n");}
			;

Ident:      IDENT
			{printf("Ident -> IDENT %s \n", $1);}
%%

int main(int argc, char **argv) {
   if (argc > 1) {
      yyin = fopen(argv[1], "r");
      if (yyin == NULL){
         printf("syntax: %s filename\n", argv[0]);
      }//end if
   }//end if
   yyparse(); // Calls yylex() for tokens.
   return 0;
}

void yyerror(const char *msg) {
   printf("** Line %d, position %d: %s\n", currLine, currPos, msg);
}