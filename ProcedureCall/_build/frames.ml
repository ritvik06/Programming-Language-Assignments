type variable = Var of string * int

type frame = Data of string * (variable list)

type expr = V of string
	| N of int
	| FunctionCall of string * expr * expr

let displayRegister0 = Stack.create();;
let displayRegister1 = Stack.create();;
let displayRegister2 = Stack.create();;
let displayRegister3 = Stack.create();;

Stack.push (Data("main", [Var("a", 0); Var("b", 0); Var("c", 0)])) displayRegister0;;

let rec isMember str l = match l with
[] -> false
|(Var(x,i)::xs) -> if (x=str) then true else false
;;

let rec append l1 l2 = match l2 with
[] -> []
|(x::xs) -> 


let stackUpdate s a1 a2 = match str with
"P" -> 

let rec display display = match display with
"displayRegister0" -> displayRegister0
|"displayRegister1" -> displayRegister1
|"displayRegister2" -> displayRegister2
|"displayRegister3" -> displayRegister3
