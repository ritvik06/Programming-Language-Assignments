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
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ EOF
%start main
%type <A1.exptree> main /* Return type */
%%
/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/

main:
	or_expression EOF {$1}
;
or_expression:
	or_expression DISJ and_expression { Disjunction($1,$3) }
	|and_expression { $1 }
;
and_expression:
	and_expression CONJ not_expression { Conjunction($1,$3) }
	|not_expression { $1 }
;
not_expression:
	NOT not_expression { Not($2) }
	|comp_expression { $1 }
;
comp_expression:      
	|comp_expression GT EQ addsub_expression { GreaterTE($1,$4) }
	|comp_expression LT EQ addsub_expression { LessTE($1,$4) }
	|comp_expression EQ addsub_expression { Equals($1,$3) }
	|comp_expression GT addsub_expression { GreaterT($1,$3) }
	|comp_expression LT addsub_expression { LessT($1,$3) }
	|addsub_expression { $1 }
;
addsub_expression:
	addsub_expression MINUS divmultrem_expression { Sub($1,$3) }
	|addsub_expression PLUS divmultrem_expression { Add($1,$3) }
	|divmultrem_expression	{ $1 }
;
divmultrem_expression:
	divmultrem_expression REM abs_expression { Rem($1,$3)}	
	|divmultrem_expression TIMES abs_expression { Mult($1,$3) }
	|divmultrem_expression DIV abs_expression { Div($1,$3)}	
	|abs_expression	{ $1 }
;
abs_expression:
	ABS abs_expression { Abs($2) }
	|tilda_expression { $1 }
;
tilda_expression:
	TILDA tilda_expression { Negative($2) }
	|if_expression { $1 }
;
if_expression:
	IF or_expression THEN or_expression ELSE or_expression FI { IfThenElse($2,$4,$6) }
	|proj_expression { $1 }	
;
proj_expression:
	|PROJ LP INT COMMA INT RP tup_expression    { Project(($3,$5),$7) }
	| tup_expression 	 { $1 }
;
tup_expression:	
	const				{ $1 }
	|LP comma_expression RP	{ Tuple(List.length $2,$2) }
;
comma_expression:
	comma_expression COMMA or_expression	{ $1@[$3] }
	|or_expression COMMA or_expression 		 { [$1;$3] }
;
const:
	ID                                 { Var($1) }      /* To be interpreted as a variable name with string as tokenised */
    | INT                              { N($1) }        /* To be interpreted as an integer with its value as tokenised   */
    | BOOL 							   { B($1) }
    |LP or_expression RP 						{ InParen($2) }
;
