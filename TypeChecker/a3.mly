%{
    open A1
%}

/*
- Tokens (token name and rules) are modified wrt to A2. Please make necessary changes in A3
- LP and RP are left and right parenthesis
- Write grammar rules to recognize
  - >= <= from GT EQ LT tokens
  - if then else fi
*/
/* Tokens are defined below.  */
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ
LET IN END BACKSLASH DOT DEF SEMICOLON COLON PARALLEL LOCAL TUNIT TFUNC TTUPLE TINT TBOOL EOF
%start def_parser exp_parser
%type <A1.definition> def_parser /* Returns definitions */
%type <A1.exptree> exp_parser /* Returns expression */
%%
/* The grammars written below are dummy. Please rewrite it as per the specifications. */

/* Implement the grammar rules for expressions, which may use the parser for definitions */
exp_parser:
  main_expression EOF   { $1 }
;

main_expression:
	int_exp										{ $1 }
	| bool_exp									{ $1 }
	| tup_exp									{ $1 }
	| proj_exp									{ $1 }
	| ifte_exp									{ $1 }
	| LET definition IN main_expression END		{ Let($2, $4) }
	| func_call									{ $1 }
	| LP main_expression RP						{ InParen($2) }
;

int_exp:
	int_exp MINUS term					{ Sub($1, $3) }
	| int_exp PLUS term 				{ Add($1, $3) }
	| term 								{ $1 }
;

term:
	term TIMES factor  					{ Mult($1, $3) }
	| term DIV factor 					{ Div($1, $3) }
	| term REM factor 					{ Rem($1, $3) }
	| factor 							{ $1 }
;

factor:
	ABS factor 		 					{ Abs($2) }
	| factor_til						{ $1 }
;

factor_til:
	TILDA factor_til					{ Negative($2) }
	| int_const 						{ $1 }
;

int_const:
	LP int_exp RP						{ InParen($2) }
	| INT 								{ N($1) }
	| ID								{ Var($1) }
	| ifte_exp							{ $1 }
	| proj_exp							{ $1 }
;

bool_exp:
	bool_term DISJ bool_exp 	 		{ Disjunction($1, $3) }
	| bool_term					   		{ $1 }
;

bool_term:
	bool_factor CONJ bool_term	 		{ Conjunction($1, $3) }
	| bool_factor			     		{ $1 }
;

bool_factor:	
	NOT bool_factor				 		{ Not($2) }
	| bool_const				 		{ $1 }
;

bool_const:
	int_exp GT EQ int_exp				{ GreaterTE($1, $4) }
	| int_exp GT int_exp				{ GreaterT($1, $3) }
	| int_exp LT EQ int_exp				{ LessTE($1, $4) }
	| int_exp LT int_exp				{ LessT($1, $3) }
	| int_exp EQ int_exp				{ Equals($1, $3) }
	| LP bool_exp RP					{ InParen($2) }
	| BOOL 						 		{ B($1) }
	| ID						 		{ Var($1) }
	| ifte_exp							{ $1 }
	| proj_exp							{ $1 }
;

tup_exp:
	LP main_expression COMMA tup_term RP { Tuple((List.length $4) +1, [$2]@$4) }
;

tup_term:
	main_expression COMMA tup_term      { [$1]@($3) }
	| main_expression			        { [$1] } 
;

proj_exp:
	PROJ LP INT COMMA INT RP tup_exp	{ Project(($3, $5),$7) }
	| PROJ LP INT COMMA INT RP proj_exp	{ Project(($3, $5),$7) }
	| PROJ LP INT COMMA INT RP main_expression { Project(($3, $5), $7) }
;

ifte_exp:
	IF bool_exp THEN main_expression ELSE main_expression FI { IfThenElse($2, $4, $6) }
;

/* Implement the grammar rules for definitions, which may use the parser for expression  */
def_parser:
	definition EOF								{ $1 }
;

definition:
	DEF ID EQ main_expression					{ Simple($2, $4) }
	| LOCAL definition IN definition END		{ Local($2, $4) }
	| par_list 									{ Parallel($1) }
	| seq_list		                            { Sequence($1) }
;

seq_list:
	definition SEMICOLON definition				{ [$1]@[$3] }
	| definition SEMICOLON seq_list				{ [$1]@$3 }


par_list:
	definition PARALLEL definition				{ [$1]@[$3] }
	| definition PARALLEL par_list				{ [$1]@$3 }
;

func_abstract:
	BACKSLASH ID COLON typer DOT main_expression			{ FunctionAbstraction($2, $4, $6) }
;

typer:
	TUNIT										{Tunit}
	| TINT 										{Tint}
	| TBOOL										{Tbool}	
	| TTUPLE LP typelist RP						{Ttuple($3)}
	| TFUNC LP typer COMMA typer RP 			{Tfunc($3, $5)}
;

typelist:
	typer 										{[$1]}
	| typer COMMA typelist						{[$1]@($3)}
;

func_call:
	func_abstract LP main_expression RP 		{ FunctionCall($1, $3) }
	| func_abstract								{ $1 }
	| ID LP main_expression RP					{ FunctionCall(Var($1), $3) }
;



