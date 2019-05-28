%{
    open A1
%}


%token <int> INT
%token <bool> BOOL
%token <string> ID
%token TILDA PLUS MINUS TIMES DIV CONJ DISJ EQ LP RP IF THEN ELSE FI COMMA BACKSLASH DOT 
TUNIT TFUNC TTUPLE TINT TBOOL COLON CMP EOF
%start exp_parser
%type <A1.expr> exp_parser
%%

exp_parser:
  main_expression EOF   { $1 }
;

main_expression:
	int_exp								{ $1 }
	| bool_exp							{ $1 }
	| ifte_exp							{ $1 }
	| func_call							{ $1 }
	| LP main_expression RP				{ $2 }
;

int_exp:
	| int_exp PLUS term 				{ Plus($1, $3) }
	| term 								{ $1 }
;

term:
	term TIMES int_const  				{ Mult($1, $3) }
	| int_const 						{ $1 }
;

int_const:
	LP int_exp RP						{ $2 }
	| INT 								{ Integer($1) }
	| ID								{ V($1) }
	| ifte_exp							{ $1 }
;

bool_exp:
	bool_term DISJ bool_exp 	 		{ Or($1, $3) }
	| bool_term					   		{ $1 }
;

bool_term:
	bool_const CONJ bool_term	 		{ And($1, $3) }
	| bool_const			     		{ $1 }
;

bool_const:
	CMP int_exp 						{ Cmp($2) }
	| BOOL 						 		{ Bool($1) }
	| ID						 		{ V($1) }
	| ifte_exp							{ $1 }
;

ifte_exp:
	IF bool_exp THEN main_expression ELSE main_expression FI { If_Then_Else($2, $4, $6) }
;

func_abstract:
	BACKSLASH ID DOT main_expression			{ Lambda(V($2), $4) }
;

typer:
	TUNIT										{Tunit}
	| TINT 										{Tint}
	| TBOOL										{Tbool}	
	| TFUNC LP typer COMMA typer RP 			{Tfunc($3, $5)}
;

func_call:
	func_abstract LP main_expression RP 		{ App($1, $3) }
	| func_abstract								{ $1 }
	| ID LP main_expression RP					{ App(V($1), $3) }
;
