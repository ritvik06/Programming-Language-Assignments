#directory "_build";; (* Consider this folder when looking for files *)
#load "a0.cmo";; (* Load the a0 bytecode *)
#load "lexer.cmo";;
#load "parser.cmo";;

open A0;;
open Lexer;;
open Parser;;

let result s = Parser.main Lexer.read (Lexing.from_string s);;

let call str = let res = result str in
	A0.called res;;

let return stack = A0.return stack;;

let set str = let res = result str in
	A0.seter res 
;;

call "P(5,3)";;
call "Q(b,19)";;
set "b:=7";;
displayCallStack callStack;;
displayreg 1;;
call "R(a,6)";;  (*Invalid Call*)
call "P(a,12)";;
call "R(w,6)";;
set "w:=1";;
set "a:=12";;
call "R(6,7)";;
set "w:=8";;
displayreg 2;;
displayCallStack callStack;;
displayreg 0;;
return callStack;;
displayCallStack callStack;;
displayreg 2;;

(* also can add call S inside P and then call R inside it by setting atleast i some value *)