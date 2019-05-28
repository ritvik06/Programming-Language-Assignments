open A0
type  exptree =  N of bigint
|  Plus of exptree *  exptree 
| Minus of exptree *  exptree 
|  Mult of exptree *  exptree 
| Div of exptree *  exptree 
| Rem of exptree *  exptree 
| Neg of  exptree  
| Abs of  exptree ;;

type opcode = CONST of bigint | PLUS | TIMES | MINUS | DIV | REM | ABS | UNARYMINUS ;;

let rec eval (tree:exptree) : bigint = match tree with
	 N num -> num
	| Plus (a,b) ->  add (eval a) (eval b)
	| Minus (a,b) -> sub (eval a) (eval b)	
	|  Mult (a,b) -> mult (eval a) (eval b)
	| Div (a,b) ->  div (eval a) (eval b)
	| Rem (a,b) ->  rem (eval a) (eval b)
	| Neg a ->  negate (eval a)
	| Abs a -> abs (eval a);;

let rec compile (tree:exptree) :opcode list = match tree with
	  N num -> [CONST(num)] 	
	| Plus (a,b) -> (compile a)@(compile b)@ [PLUS]
	| Minus (a,b) -> (compile a)@(compile b)@ [MINUS]
	|  Mult (a,b) -> (compile a)@(compile b)@ [TIMES]
	| Div (a,b) -> (compile a)@(compile b)@ [DIV]
	| Rem (a,b) -> (compile a)@(compile b)@ [REM]
	| Neg a -> (compile a)@ [UNARYMINUS] 
	| Abs a ->(compile a) @ [ABS];;

let rec stackmc (list1:bigint list) (oplist:opcode list) = match oplist with
	CONST(i)::xs -> stackmc (i::list1) xs
	|PLUS::xs -> (match list1 with
					(y1::(y2::ys)) -> stackmc ((add y1 y2)::ys) xs
					|_ -> List.hd list1
				)
	|TIMES::xs -> (match list1 with
					(y1::(y2::ys)) -> stackmc ((mult y1 y2)::ys) xs		
					|_ -> List.hd list1		
				)
	|MINUS::xs -> (match list1 with
					(y1::(y2::ys)) -> stackmc ((sub y1 y2)::ys) xs
					|_ -> List.hd list1				
				)
	|DIV::xs -> (match list1 with
					(y1::(y2::ys)) -> stackmc ((div y1 y2)::ys) xs
					|_ -> List.hd list1				
				)
	|REM::xs -> (match list1 with
					(y1::(y2::ys)) -> stackmc ((rem y1 y2)::ys) xs
					|_ -> List.hd list1				
				)
	|ABS::xs -> (match list1 with
					(y1::ys) -> stackmc ((abs y1)::ys) xs
					|[] -> (NonNeg,[0])
					)	
	|UNARYMINUS::xs -> (match list1 with
					(y1::ys) -> stackmc ((negate y1)::ys) xs
					|[] -> (NonNeg,[0])			
				)
	|[] -> (match list1 with
			 x::xs -> x
			|x::[] -> x
			|[] -> (NonNeg,[0])
			);;
				  	

 



