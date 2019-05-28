{exception InvalidToken of string;;
exception InvalidToken of char;;
let func s = s.[0];;
type token =
   INT of int          (* integer constant, positive or negative w/o leading zeros *)
|  TRUE                (* boolean constant "T" *)
|  FALSE               (* boolean constant "F" *)
|  ABS                 (* unary operator, "abs" *)
|  PLUS                (* arithmetic plus, "+" *)
|  MINUS               (* arithmetic minus, "-" *)
|  MUL                 (* arithmetic multiply, "*" *)
|  DIV                 (* integer div, "div" *)
|  MOD                 (* remainder, "mod" *)
|  EXP                 (* exponentiation, "^" *)
|  LP                  (* left paren, "(" *)
|  RP                  (* right paren, ")" *)
|  NOT                 (* boolean NOT, "not" *)
|  AND                 (* boolean AND, "/\ " *)
|  OR                  (* boolean OR, "\/" *)
|  EQ                  (* equal to, "=" *)
|  GTA                 (* greater than, ">" *)
|  LTA                 (* less than, "<" *)
|  GEQ                 (* greater than/equal to, ">=" *)
|  LEQ                 (* less than/equal to, "<=" *)
|  IF                  (* keyword "if" *)
|  THEN                (* keyword "then" *)
|  ELSE                (* keyword "else" *)
|  ID of string        (* variable identifier, alphanumeric string with first char lowercase *)
|  DEF                 (* definition construct, "def" *)
|  DELIMITER;;         (* delimiter, ";" *)
}



let whitespace = [' ' '\t']+
let digit = ['0'-'9']
let digits = digit+
let integer = '-'? digits
let plus = "+"
let sub = '-'
let mult = '*'
let div = "div"	
let modulo = "mod"
let eq = "="
let exp = '^'
let lp = "("
let rp = ")"
let truee = "T"
let falsee = "F"
let neg = "not" 
let andd = "/\\"
let orr =  "\\/"
let gta = '>'
let lta = '<'
let geq = ">="
let leq = "<="
let iff = "if"
let thenn = "then"
let elsee = "else"
let letter = ['a'-'z']
let letterC = ['a'-'z' 'A'-'Z']
let id = letter letterC*
let func = "def"
let del = ";"

rule read = parse
	integer as n  {INT (int_of_string n)::(read lexbuf)} 
	|modulo {ABS::(read lexbuf)}
	|plus {PLUS::(read lexbuf)}
	|sub  {MINUS::(read lexbuf)}
	|mult {MUL::(read lexbuf)}
	|div {DIV::(read lexbuf)}
	|exp {EXP::(read lexbuf)}
	|lp {LP::(read lexbuf)}
	|rp {RP::(read lexbuf)}
	|eq {EQ::(read lexbuf)}
	|truee {TRUE::(read lexbuf)}
	|falsee {FALSE::(read lexbuf)}
	|neg {NOT::(read lexbuf)}
	|andd {AND::(read lexbuf)}
	|orr {OR::(read lexbuf)}
	|gta {GTA::(read lexbuf)}
	|lta {LTA::(read lexbuf)}
	|geq {GEQ::(read lexbuf)}
	|leq {LEQ::(read lexbuf)}
	|iff {IF::(read lexbuf)}
	|thenn {THEN::(read lexbuf)}
	|elsee {ELSE::(read lexbuf)}
	|id as s {ID(s)::(read lexbuf)}
	|func {DEF::(read lexbuf)}
	|del {DELIMITER::(read lexbuf)}
	|whitespace   {(read lexbuf)}
	|_ as err  {raise (InvalidToken err)}
	|eof  { [] }

{
  let scanner s = read (Lexing.from_string s)
}


(*
 # #use "Task2.ml";;

  # scanner "+ 5";;
  - : token list = [PLUS; INT 5]
  # scanner "-50";;
  - : token list = [INT (-50)]
  # scanner "+5";;
  - : token list = [PLUS; INT 5]
  # scanner "45-676";;           
  - : token list = [INT 45; INT (-676)]
  # scanner "45 - 76";;                                                                                                                                                       
  - : token list = [INT 45; MINUS; INT 76]
  # scanner "45/\\76";;
  - : token list = [INT 45; AND; INT 76]
  # scanner "45\\/76";;
  - : token list = [INT 45; OR; INT 76]
  # scanner "45 # 676";;
	Exception: InvalidToken.
  # scanner "This";;
  - : token list = [TRUE; ID "his"]

	Binary Ops have been implemented when a whitespace is provided when they are used,this will be taken care of when the context free grammar is implemented

*)