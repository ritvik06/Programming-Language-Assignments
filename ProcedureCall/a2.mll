
{
open Parser        
exception Not_Implemented
}

let digit = ['0'-'9']
let digits = (digit+)
let integer = ['-']?digits+
let alpha = ['a'-'z' 'A'-'Z']
let id = (alpha+)
let colon = [':']

rule read = parse
  [' ' '\t' '\n']  	{read lexbuf} 
  | '('               	{LP}
  | ')'               	{RP}
  | ','               	{COMMA}
  | integer as n 		{INT (int_of_string n)}
  | id as s 			{ID (s)}
  | colon 				{COLON}
  | '='					{EQ}
  | eof {EOF}