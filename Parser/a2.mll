(*{
  open A3
  exception Not_implemented
}

(*
  Below is a dummy implementation. Please note the following
  - Tokens are defined in A3.mly
  - Return type is token and not token list
  - End of buffer is indicated by EOF token below
  - There is no trailer. The scanner function is written in the wrapper file (test_a3.ml)
*)
rule read = parse
   eof                { EOF }
   | ['0'-'9']+ as n  { INT (int_of_string n) }
   | _                { raise Not_implemented }*)

{open A3
  exception InvalidToken of string;;
exception InvalidToken of char;;
let func s = s.[0];;
}



let whitespace = [' ' '\t']+
let digit = ['0'-'9']
let digits = digit+
let integer = '-'? digits
let plus = "+"
let sub = '-'
let mult = '*'
let tilda = '~'
let div = "div" 
let modulo = "abs"
let eq = "="
let exp = '^'
let lp = "("
let rp = ")"
let boolean = ("T"|"F")
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
let fi = "fi"
let letter = ['a'-'z']
let letterC = ['A'-'Z']
let und = ['_']
let aps = ['\'']
let lletter = (letterC|letter|und|aps|digit)
let id = (letterC)(lletter*) 
let func = "def"
let del = ";"
let comma = ','
let proj = "proj"
let rem = "rem"

rule read = parse
  integer as n  {INT (int_of_string n)} 
  |boolean as b {if (b='T') then BOOL(true) else BOOL(false)}
  |modulo {ABS}
  |tilda {TILDA}
  |plus {PLUS}
  |sub  {MINUS}
  |mult {TIMES}
  |div {DIV}
  |rem {REM}
  |lp {LP}
  |rp {RP}
  |eq {EQ}
  |neg {NOT}
  |andd {CONJ}
  |orr {DISJ}
  |gta {GT}
  |lta {LT}
  |iff {IF}
  |thenn {THEN}
  |elsee {ELSE}
  |fi {FI}
  |comma {COMMA}
  |proj {PROJ}
  |id as s {ID(s)}
  |whitespace   {(read lexbuf)}
  |_ as err  {raise (InvalidToken err)}
  |eof  { EOF }

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
