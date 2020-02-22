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
%error-verbose
%start prog_start

%token <ival> IDENT
%token <dval> NUMBER

%token FUNCTION BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY END_BODY
%token INTEGER ARRAY OF IF THEN ENDIF ELSE WHILE DO FOR BEGINLOOP ENDLOOP CONTINUE
%token READ WRITE TRUE FALSE RETURN SEMICOLON COLON COMMA L_PAREN R_PAREN
%token L_SQUARE_BRACKET R_SQUARE_BRACKET

%left AND OR SUB ADD MULT DIV MOD
%left EQ NEQ LT GT LTE GTE

%right ASSIGN NOT

%%

/* Grammer Rules */


prog_start:	Function prog_start {printf("prog_start -> Function\n");}
			| %empty {printf("prog_start -> EPSILON\n");}
			;

Function: 		FUNCTION Ident SEMICOLON BEGIN_PARAMS Declaration_loop SEMICOLON END_PARAMS BEGIN_LOCALS Declaration_loop SEMICOLON END_LOCALS BEGIN_BODY Statement_loop SEMICOLON END_BODY{printf("Function -> FUNCTION Ident SEMICOLON BEGIN_PARAMS Declaration_loop END_PARAMS BEGIN_LOCALS Declaration_loop END_LOCALS BEGIN_BODY Statement_loop END_BODY\n");}
				;

Declaration_loop: 	Declaration SEMICOLON Declaration_loop {printf("Declaration_loop -> Declaration SEMICOLON Declaration_loop\n");}
					| %empty {printf("Declaration_loop -> EPSILON\n");}
					;

Declaration:	Ident_loop COLON INTEGER {printf("Declaration -> Ident_loop COLON INTEGER\n");}
				| Ident_loop COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER {printf("Declaration -> Ident_loop COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER\n");}
				;

Ident_loop: 	Ident {printf("Ident_loop -> Ident \n");}
				| Ident COMMA Ident_loop {printf("Ident_loop -> Ident COMMA Ident_loop\n");}
				;

Statement: 	Statement1 {printf("Statement -> Statement1\n");}
			| Statement2 {printf("Statement -> Statement2\n");}
			| Statement3 {printf("Statement -> Statement3\n");}
			| Statement4 {printf("Statement -> Statement4\n");}
			| Statement5 {printf("Statement -> Statement5\n");}
			| Statement6 {printf("Statement -> Statement6\n");}
			| Statement7 {printf("Statement -> Statement7\n");}
			| Statement8 {printf("Statement -> Statement8\n");}
			| Statement9 {printf("Statement -> Statement9\n");}
			;

Statement1:  	Var ASSIGN Expression {printf("Statement1 -> Var ASSIGN Expression\n");}
				;
		
Statement2: 	IF Bool-Expr THEN Statement_loop ElseStatement ENDIF {printf("Statement -> IF Bool-Expr THEN Statement_loop ElseStatement ENDIF\n");}
				;

Statement3: 	WHILE Bool-Expr BEGINLOOP Statement_loop SEMICOLON ENDLOOP {printf("Statement -> WHILE Bool-Expr BEGINLOOP Statement_loop SEMICOLON ENDLOOP\n");}
				;

Statement4:		DO BEGINLOOP Statement_loop SEMICOLON ENDLOOP WHILE Bool-Expr {printf("Statement -> DO BEGINLOOP Statement_loop SEMICOLON ENDLOOP WHILE Bool-Expr\n");}
				;

Statement5: 	FOR Var ASSIGN NUMBER SEMICOLON Bool-Expr SEMICOLON Var ASSIGN Expression BEGINLOOP Statement_loop SEMICOLON ENDLOOP {printf("Statement -> FOR Var ASSIGN NUMBER SEMICOLON Bool-Expr SEMICOLON Var ASSIGN Expression BEGINLOOP Statement_loop SEMICOLON ENDLOOP\n");}
				;

Statement6: 	READ Var {printf("Statement -> READ Var\n");}
				;

Statement7: 	WRITE Var {printf("Statement -> WRITE Var\n");}
				;

Statement8: 	CONTINUE {printf("Statement -> CONTINUE\n");}
				;

Statement9: 	RETURN Expression {printf("Statement -> Expression\n");}
				;

Statement_loop: 	Statement SEMICOLON Statement_loop {printf("Statement_loop -> Statement SEMICOLON Statement_loop\n");}
					| Statement SEMICOLON {printf("Statement_loop -> Statement SEMICOLON\n");}
					;

ElseStatement: 		ELSE Statement_loop {printf("ElseStatement -> ELSE Statement_loop\n");}
					| %empty {printf("ElseStatement -> EPSILON\n");}
					;

Bool-Expr:		Relation-And-Expr {printf("Bool-Expr -> Relation-And-Expr\n");}
				;

Relation-And-Expr:		Relation-Expr {printf("Relation-And-Expr -> Relation-Expr\n");}
						| Relation-Expr AND Relation-And-Expr  {printf("Relation-And-Expr -> Relation-Expr AND Relation-And-Expr\n");}
						;


Relation-Expr:		NOT Relation-Expr_loop {printf("Relation-Expr -> NOT Relation-Expr_loop\n");}
					| Relation-Expr_loop {printf("Relation-Expr -> Relation-Expr_loop\n");}
					;

Relation-Expr_loop: 	Expression Comp Expression {printf("Relation-Expr_loop -> Expression Comp Expression\n");}
                 		| TRUE	{printf("Relation-Expr_loop -> TRUE\n");}
						| FALSE {printf("Relation-Expr_loop -> FALSE\n");}
						| L_PAREN Bool-Expr R_PAREN {printf("Relation-Expr_loop -> L_PAREN Bool-Expr R_PAREN\n");}
						;
		
Comp: 		EQ {printf("Comp -> EQ\n");}
			| NEQ {printf("Comp -> NEQ\n");}
			| LT {printf("Comp -> LT\n");}
			| GT {printf("Comp -> GT\n");}
			| LTE {printf("Comp -> LTE\n");}
			| GTE {printf("Comp -> GTE\n");}
			;

Expression: 		 Multiplicative-Expr {printf("Expression -> Multiplicative-Expr\n");}
					| Multiplicative-Expr SUB Expression {printf("Expression -> Multiplicative-Expr SUB Expression\n");}
					| Multiplicative-Expr ADD Expression {printf("Expression -> Multiplicative-Expr ADD Expression\n");}
					;

Expression_loop: 	Expression COMMA Expression_loop {printf("Expression -> COMMA Expression\n");}
					| Expression {printf("Expression_loop -> Expression\n");};
					| %empty {printf("Expression_loop -> EPSILON\n");};
					;

Multiplicative-Expr:		Term  {printf("Multiplicative-Expr -> Term\n");}
							| MOD Term Multiplicative-Expr {printf("Multiplicative-Expr -> Term MOD Term\n");}
							| DIV Term Multiplicative-Expr {printf("Multiplicative-Expr -> Term MOD Term\n");}
							| MULT Term Multiplicative-Expr {printf("Multiplicative-Expr-> Term MOD Term\n");} 
							;

Term:		 Var {printf("Term -> Var\n");}
			| NUMBER {printf("Term -> NUMBER\n");}
			| L_PAREN Expression R_PAREN {printf("Term -> L_PAREN Expression R_PAREN\n");}
			| SUB Var {printf("Term -> SUB Var\n");}
			| SUB NUMBER {printf("Term -> SUBB NUMBER\n");}
			| SUB L_PAREN Expression R_PAREN {printf("Term -> SUB L_PAREN Expression R_PAREN\n");}
			| Ident L_PAREN Expression_loop R_PAREN {printf("Term -> Ident L_PAREN Expression R_PAREN\n");}
			;

Var:			 Ident {printf("Var -> Ident\n");}
				| Ident L_SQUARE_BRACKET Expression R_SQUARE_BRACKET {printf("Var -> Ident L_SQUARE_BRACKET Expression R_SQUARE_BRACKET\n");}
				;

Var_loop: 		Var {printf("Var_loop -> Var\n");}
				| Var COMMA Var_loop {printf("Var_loop -> Var COMMA Var_loop\n");}
				;

Ident:      IDENT
			{printf("Ident -> IDENT %s \n", $1);}
%%

void yyerror(const char* msg)
{
  extern int currLine;
  extern char* yytext;

  printf("ERROR: %s at symbol \"%s\" on line %d\n", msg, yytext, currLine);
  exit(1);
}
