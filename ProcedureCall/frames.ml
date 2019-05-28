exception Error
exception NotAccessibleError
exception InvalidCall

type variable = Var of string * int

type stack_frame = Data of string * (variable list)

type expr = V of string
	| N of int
	| FunctionCall of string * expr * expr
	| VariableSet of string * expr


let displayRegister0 = Stack.create();;
let displayRegister1 = Stack.create();;
let displayRegister2 = Stack.create();;
let displayRegister3 = Stack.create();;
let callStack = Stack.create();;

Stack.push (Data("main", [Var("a", 0); Var("b", 0); Var("c", 0)])) callStack;;
Stack.push (Data("main", [Var("a", 0); Var("b", 0); Var("c", 0)])) displayRegister0;;


let rec intForString str l = match l with 
	[] -> raise Error
	| (x::xs) -> let Var(y, i) = x in
					if y = str then i
					else intForString str xs


let callevel str = match str with
	"main" -> 0
	| "P"
	| "Q" -> 1
	| "W"
	| "V" -> 3
	| _ -> 2


let rec ifExist str l = match l with 
	[] -> false
	| (x::xs) -> let Var(y, i) = x in
				if y = str then true
				else ifExist str xs


let rec replace str l intToReplace = match l with
	[] -> raise NotAccessibleError
	| (x::xs) -> let Var(y, i) = x in
					if y = str then Var(y, intToReplace)::xs
					else x::(replace str xs intToReplace)


let rec ifNotPresent str l = match l with
	[] -> true
	| x::xs -> if x = str then false
				else ifNotPresent str xs


let rec app l1 l2 = match l2 with
	[] -> l1
	| (x::xs) -> let Var(y, i) = x in
		if (ifExist y l1) then app l1 xs
		else app (l1@[x]) xs


let rec find str level = match level with
	(-1) -> raise NotAccessibleError
	| 0 -> if (Stack.is_empty displayRegister0 = false) then
			let Data(ste, varlist) = Stack.top displayRegister0 in
			if (ifExist str varlist) then intForString str varlist
			else find str (level - 1)
			else raise NotAccessibleError
	| 1 -> if (Stack.is_empty displayRegister1 = false) then
			let Data(ste, varlist) = Stack.top displayRegister1 in
			if (ifExist str varlist) then intForString str varlist
			else find str (level - 1)
			else raise NotAccessibleError
	| 2 -> if (Stack.is_empty displayRegister2 = false) then
			let Data(ste, varlist) = Stack.top displayRegister2 in
			if (ifExist str varlist) then intForString str varlist
			else find str (level - 1)
			else raise NotAccessibleError
	| 3 -> if (Stack.is_empty displayRegister3 = false) then
			let Data(ste, varlist) = Stack.top displayRegister3 in
			if (ifExist str varlist) then intForString str varlist
			else find str (level - 1)
			else raise NotAccessibleError


let rec replaceCallStack proc varstr i l = match l with 
	[] -> raise NotAccessibleError
	| (x::xs) -> let Data(s, d) = x in
					if (ifExist varstr d) && proc = s then
					(Data(s, (replace varstr d i)))::xs
					else
					x::(replaceCallStack proc varstr i xs)


let rec empty stack = if ((Stack.is_empty stack) = false) then
	let x = Stack.pop stack in
	empty stack
	else
	true


let rec createStack l stack = match l with
	[x] -> Stack.push x stack
	| (x::xs) -> let z = Stack.push x stack in
		createStack xs stack
	| _ -> raise NotAccessibleError


let rec stackToList dis result = if (Stack.is_empty dis) then result
	else
		let x = Stack.pop dis in
		stackToList dis (result@[x]) 


let rec setVariable level str i = match level with
	0 -> let Data(s0, d0) = Stack.top displayRegister0 in
			if (ifExist str d0) then
			let x = Stack.pop displayRegister0 in
			let l = replaceCallStack s0 str i (stackToList (Stack.copy callStack) []) in
			let bhashta = empty callStack in
			let z = createStack (List.rev l) callStack in
			Stack.push (Data(s0, (replace str d0 i))) displayRegister0
			else raise NotAccessibleError 
	| 1 -> let Data(s1, d1) = Stack.top displayRegister1 in
			if (ifExist str d1) then
			let x = Stack.pop displayRegister1 in
			let l = replaceCallStack s1 str i (stackToList (Stack.copy callStack) []) in
			let bhashta = empty callStack in
			let z = createStack (List.rev l) callStack in
			Stack.push (Data(s1, (replace str d1 i))) displayRegister1
			else setVariable (level - 1) str i
	| 2 -> let Data(s2, d2) = Stack.top displayRegister2 in
			if (ifExist str d2) then
			let x = Stack.pop displayRegister2 in
			let l = replaceCallStack s2 str i (stackToList (Stack.copy callStack) []) in
			let bhashta = empty callStack in
			let z = createStack (List.rev l) callStack in
			Stack.push (Data(s2, (replace str d2 i))) displayRegister2
			else setVariable (level - 1) str i
	| 3 -> let Data(s3, d3) = Stack.top displayRegister3 in
			if (ifExist str d3) then
			let x = Stack.pop displayRegister3 in
			let l = replaceCallStack s3 str i (stackToList (Stack.copy callStack) []) in
			let bhashta = empty callStack in
			let z = createStack (List.rev l) callStack in
			Stack.push (Data(s3, (replace str d3 i))) displayRegister3
			else setVariable (level - 1) str i


let proceduresCanBeCalled stack = let Data(str, d) = Stack.top callStack in
	match str with
	"main" -> ["P"; "Q"]
 	| "P" -> ["P"; "Q"; "R"; "S"]
	| "Q" -> ["P"; "Q"; "T"; "U"]
	| "R" -> ["P"; "Q"; "R"; "S"; "V"]
	| "S" -> ["P"; "Q"; "R"; "S"]
	| "T" -> ["P"; "Q"; "U"; "W"; "T"]
	| "U" -> ["P"; "Q"; "T"; "U"]
	| "V" -> ["P"; "Q"; "R"; "S"; "V"]
	| "W" -> ["P"; "Q"; "T"; "U"; "W"]


let access stack = let Data(str, l1) = Stack.top stack in
	let i = callevel str in
	match i with
	0 -> let Data(s0, d0) = Stack.top displayRegister0 in
		d0
	| 1 -> let Data(s1, d1) = Stack.top displayRegister1
			and Data(s0, d0) = Stack.top displayRegister0 in
			app d1 d0 
	| 2 -> let Data(s2, d2) = Stack.top displayRegister2
			and Data(s1, d1) = Stack.top displayRegister1
			and Data(s0, d0) = Stack.top displayRegister0 in
			let l1 = app d2 d1 in
			app l1 d0
	| 3 -> let Data(s3, d3) = Stack.top displayRegister3
			and Data(s2, d2) = Stack.top displayRegister2
			and Data(s1, d1) = Stack.top displayRegister1
			and Data(s0, d0) = Stack.top displayRegister0 in
			let l1 = app d3 d2 in
			let l2 = app l1 d1 in
			app l2 d0

let callStackUpdateCall str i1 i2 = match str with
	| "P" -> let l = [Var("a", 0); Var("x", i1); Var("y", i2); Var("z", 0)] in
				Stack.push (Data("P", l)) callStack
	| "Q" -> let l = [Var("b", 0); Var("z", i1); Var("w", i2); Var("x", 0)] in
				Stack.push (Data("Q", l)) callStack
	| "R" -> let l = [Var("b", 0); Var("j", 0); Var("w", i1); Var("i", i2)] in
				Stack.push (Data("R", l)) callStack
	| "S" -> let l = [Var("m", 0); Var("n", 0); Var("z", i1); Var("w", i2)] in
				Stack.push (Data("S", l)) callStack
	| "T" -> let l = [Var("i", 0); Var("f", 0); Var("a", i1); Var("y", i2)] in
				Stack.push (Data("T", l)) callStack
	| "U" -> let l = [Var("p", 0); Var("g", 0); Var("c", i1); Var("z", i2)] in
				Stack.push (Data("U", l)) callStack
	| "V" -> let l = [Var("c", 0); Var("m", i1); Var("n", i2)] in
				Stack.push (Data("V", l)) callStack
	| "W" -> let l = [Var("j", 0); Var("h", 0); Var("m", i1); Var("p", i2)] in
				Stack.push (Data("W", l)) callStack


let displayRegistersUpdateCall str = let x = Stack.top callStack in
	match str with
	"main" -> Stack.push x displayRegister0
	| "P" 
	| "Q" -> Stack.push x displayRegister1
	| "W" 
	| "V" -> Stack.push x displayRegister3
	| _ -> Stack.push x displayRegister2

let called input =
	let Data(stre, xer) = Stack.top callStack in
	let p = proceduresCanBeCalled callStack in
	let FunctionCall(str, x, y) = input in
	if ifNotPresent str p then raise InvalidCall
else
	match (x,y) with
	(N(i1), N(i2)) -> let x = callStackUpdateCall str i1 i2 in	
						displayRegistersUpdateCall str
	| (V(s1), V(s2)) -> let i1 = find s1 ((callevel stre)) and
							i2 = find s2 ((callevel stre)) in
						let z = callStackUpdateCall str i1 i2 in	
						displayRegistersUpdateCall str
	| (V(s1), N(i2)) -> let i1 = find s1 ((callevel stre)) in
						let z = callStackUpdateCall str i1 i2 in	
						displayRegistersUpdateCall str
	| (N(i1), V(s2)) -> let i2 = find s2 ((callevel stre)) in
						let z = callStackUpdateCall str i1 i2 in	
						displayRegistersUpdateCall str

let rec stackToList dis result = if (Stack.is_empty dis) then result
	else
		let x = Stack.pop dis in
		stackToList dis (result@[x]) 


let displayreg dis = match dis with
	0 -> Stack.top displayRegister0
	| 1 -> Stack.top displayRegister1
	| 2 -> Stack.top displayRegister2
	| 3 -> Stack.top displayRegister3
	

let displayCallStack stack = stackToList (Stack.copy stack) []


let return stack = let Data(str, varlist) = Stack.pop stack in
	match str with
	"main" -> Stack.pop displayRegister0
	| "P" 
	| "Q" -> Stack.pop displayRegister1
	| "W" 
	| "V" -> Stack.pop displayRegister3
	| _ -> Stack.pop displayRegister2


let seter s = let VariableSet(s1, s2) = s in
	let Data(process, varrier) = Stack.top callStack in
	let level = callevel process in
	match s2 with
	N(i) -> setVariable level s1 i
	| V(s) -> let i = find s level in
				setVariable level s1 i

