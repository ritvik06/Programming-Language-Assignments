
{open A3
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
let lett = "let"
let inn = "in"
let endd = "end"
let backslash = "\\"
let dot = "."
let def = "def"
let semic = ";"
let parallel = "||"
let local = "local"
let colon = ":"
let cmp = "cmp"


rule read = parse
  integer as n  {INT (int_of_string n)} 
  |boolean as b {if (b='T') then BOOL(true) else BOOL(false)}
  |tilda {TILDA}
  |plus {PLUS}
  |sub  {MINUS}
  |mult {TIMES}
  |div {DIV}
  |lp {LP}
  |rp {RP}
  |andd {CONJ}
  |orr {DISJ}
  |comma {COMMA}
  |iff {IF}
  |thenn {THEN}
  |elsee {ELSE}
  |fi {FI}
  |cmp {CMP}
  |dot {DOT}
  |backslash {BACKSLASH}
  |eq {EQ}
  |id as s {ID(s)}
  | "Tint" {TINT}
  | "Tbool" {TBOOL}
  | "Tunit" {TUNIT}
  | "Ttuple" {TTUPLE}
  | "Tfunc" {TFUNC}
  | colon    {COLON}
  |whitespace {read lexbuf}
  |eof {EOF}
  |_ as err  {raise (InvalidToken err)}
