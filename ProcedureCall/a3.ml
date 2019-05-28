%{
    open A0
%}

%token LP RP COMMA COLON EQ EOF
%token <int> INT
%token <string> ID
%start main             /* the entry point */
%type <A0.expr> main                     /* Specifying the type to be returned for the grammar symbol main */
%%
main:
    main_expression EOF                { $1 }
;
main_expression:
    ID LP term COMMA term RP		   { FunctionCall($1, $3, $5) }
    | ID COLON EQ term				   { VariableSet($1, $4) }
;

term:
    ID                                 { V($1) }      /* To be interpreted as a variable name with string as tokenised */
    | INT                              { N($1) }      /* To be interpreted as an integer with its value as tokenised   */
;