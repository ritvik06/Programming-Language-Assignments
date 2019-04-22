#directory "_build";; (* Consider this folder when looking for files *)
#load "a1.cmo";;
#load "a2.cmo";;
#load "a3.cmo";;
open A1;;
open A2;;
open A3;;

let p1 =  App (Lambda (V "x", Mult (Integer 3, V "x")), Integer 4);;

let p2 = If_Then_Else
   (Cmp (Integer 7),
    App (Lambda (V "x", Plus (Integer 3, V "x")), Integer 31), 
    Integer 0);;

let p3 = If_Then_Else
    (Cmp (Integer 0),
    App (Lambda (V "x", Plus (Integer 3, V "x")), Integer 4),
        Integer 110);;

let str1 = "\\X.3*X(5)";;
let str2 = "\\X.X+3(5)";;
let str3 = "if cmp 7 then \\X.X+3(31) else 0 fi";;
let str4 = "if Y then 1 else 2 fi";;


let exp_parser s = A3.exp_parser A2.read (Lexing.from_string s) ;; 

let clos1 = Clos(exp_parser str1,[]);;
let clos2 = Clos(exp_parser str2,[]);;
let clos3 = Clos(exp_parser str3,[]);;
let clos4 = Clos(exp_parser str4,["Y",Clos(Bool(true),[])]);;


krivinemc clos1 [];;
krivinemc clos2 [];;
krivinemc clos3 [];;
krivinemc clos4 [];;


let com1 = compile (exp_parser str1);;
let com2 = compile (exp_parser str2);;
let com3 = compile (exp_parser str3);;
let com4 = compile (exp_parser str4);;


secd [] [] com1 [];;
secd [] [] com2 [];;
secd [] [] com3 [];;
secd [] ["Y",Bool(true)] com4 [];;








