exception Not_implemented
exception Empty_table
exception Illegal_Entity
exception Illegal_Stack

type exptype = Tunit | Tint | Tbool | Tfunc of (exptype * exptype)
 	
type expr =
	V of string
	| Lambda of (expr * expr)
	| App of (expr * expr)
	| Plus of (expr * expr)
	| Mult of (expr * expr)
	| And of (expr * expr)
	| Or of (expr * expr)
	| Bool of bool
	| Integer of int  
	| Cmp of expr
	| If_Then_Else of (expr * expr * expr)


type opcode = VAR of string | NCONST of int | BCONST of bool | PLUS
	| MULT | AND | OR | IFTE | APP
	| RET | CLOS of (string * (opcode list)) | CMP

type answer = Num of int | Bool of bool | Cls of (string * (opcode list) * table2)
and table2 = (string * answer) list

type closure = Clos of (expr * table)
and table = (string * closure) list

let rec lookUp g str = match g with
	[] -> raise Empty_table
	|((s,clos)::xs) -> if s=str then clos else lookUp xs s
;;

let rec ifte x1 x2 x3 = if (x1=true) then x2 else x3;;


let rec krivinemc (clos:closure) stack = match clos with
 Clos(V(x), t) -> krivinemc (lookUp t x) stack
|Clos(Integer(i),t) -> clos
|Clos(Bool(b),t) -> clos
| Clos(Plus(e1, e2), t) -> let clos1 = (krivinemc (Clos(e1, t)) stack) in
							let clos2 = (krivinemc (Clos(e2, t)) stack) in
								(match (clos1, clos2) with
								((Clos(Integer(i1), t1)), Clos(Integer(i2), t2)) -> Clos(Integer(i1+i2), t)
							| _ -> raise Illegal_Entity)
| Clos(Mult(e1, e2), t) -> let clos1 = (krivinemc (Clos(e1, t)) stack) in
							let clos2 = (krivinemc (Clos(e2, t)) stack) in
								(match (clos1, clos2) with
								((Clos(Integer(i1), t1)), Clos(Integer(i2), t2)) -> Clos(Integer(i1*i2), t)
							| _ -> raise Illegal_Entity)

| Clos(Or(e1, e2), t) -> let clos1 = (krivinemc (Clos(e1, t)) stack) in
							let clos2 = (krivinemc (Clos(e2, t)) stack) in
								(match (clos1, clos2) with
								((Clos(Bool(b1), t1)), Clos(Bool(b2), t2)) -> Clos(Bool(b1 || b2), t)
							| _ -> raise Illegal_Entity)


| Clos(And(e1, e2), t) -> let clos1 = (krivinemc (Clos(e1, t)) stack) in
							let clos2 = (krivinemc (Clos(e2, t)) stack) in
								(match (clos1, clos2) with
								((Clos(Bool(b1), t1)), Clos(Bool(b2), t2)) -> Clos(Bool(b1 && b2), t)
							| _ -> raise Illegal_Entity)

|Clos(Cmp(e),t) -> (let clos2 = (krivinemc (Clos(e, t)) stack) in 
					(match clos2 with
					(Clos(Integer(i), t1)) -> if(i>0) then Clos(Bool(true),t) else Clos(Bool(false),t)
					|_ -> raise Illegal_Entity
					)
				   )
|Clos(If_Then_Else(e1,e2,e3),t) -> (let clos2 = (krivinemc (Clos(e1, t)) stack) in 
									(match clos2 with 
									Clos(Bool(true),t1) -> (krivinemc (Clos(e2, t)) stack)
								    |Clos(Bool(false),t1) -> (krivinemc (Clos(e3, t)) stack)
								    )
								  )
|Clos(App(e1,e2),t)  -> (krivinemc (Clos(e1, t)) ((Clos(e2, t))::stack))
| Clos(Lambda(V(x), e), t) -> (match stack with
		(Clos(e1, t1)::s') -> krivinemc (Clos(e, (x, Clos(e1, t))::t1)) s'
		| _ -> raise Illegal_Entity)
;;

let rec compile exp = match exp with
V(s) -> [VAR(s)]		
| Integer(i) -> [NCONST(i)]
| Bool(b) -> [BCONST(b)]
| Plus(e1, e2) -> (compile e1)@(compile e2)@[PLUS]
| Mult(e1, e2) -> (compile e1)@(compile e2)@[MULT]
| And(e1, e2) -> (compile e1)@(compile e2)@[AND]
| Or(e1, e2) -> (compile e1)@(compile e2)@[OR]
| Cmp(e) -> (compile e)@[CMP]
| If_Then_Else(e1, e2, e3) -> (compile e1)@(compile e2)@(compile e3)@[IFTE]
| Lambda(V(x), e) -> [CLOS(x, (compile e)@[RET])]
| App(e1, e2) -> (compile e1)@(compile e2)@[APP]

let rec secd s e c d = match c with
[] -> s
|VAR(str)::xs -> secd ((lookUp e str)::s) e xs d
|NCONST(i)::xs -> secd (Num(i)::s) e xs d
|BCONST(b)::xs -> secd (Bool(b)::s) e xs d
|AND::xs   ->   (match s with
              (y1::(y2::ys)) -> (match y1 with
                            Bool(x1)  -> (match y2 with
                            Bool(x2)  ->  secd (Bool(x1&x2)::ys) e xs d 
              |_ -> raise Illegal_Stack))
            )
|OR::xs     ->   (match s with
              (y1::(y2::ys)) -> (match y1 with
                            Bool(x1)  -> (match y2 with
                            Bool(x2)  ->  secd (Bool(x1 or x2)::ys) e xs d))
              |_ -> raise Illegal_Stack
             )

|PLUS::xs    ->   (match s with
              (y1::(y2::ys)) -> (match y1 with
                            Num(x1)  -> (match y2 with
                            Num(x2)  ->  secd (Num(x1 + x2)::ys) e xs d))
              |_ -> s
            )

|MULT::xs    ->   (match s with
              (y1::(y2::ys)) -> (match y1 with
                            Num(x1)  -> (match y2 with
                            Num(x2)  ->  secd (Num(x1 * x2)::ys) e xs d))
              |_ -> s
            )
|CMP::xs    ->    (match s with
				    y::ys -> (match y with
				    		   Num(a) -> if(a>0) then secd (Bool(true)::ys) e xs d else secd (Bool(false)::ys) e xs d
				    		  |_ -> raise Illegal_Stack
				    )
				    |_ -> s
			)
            
|IFTE::xs   -> (match s with
              (y1::(y2::(y3::ys))) -> (match y1 with
                            Num(x1)  -> (match y2 with   
                            Num(x2)  ->(match y3 with
                            Bool(x3)  ->  secd (Num(ifte x3 x2 x1)::ys) e xs d)))
              |_ -> s
            )
| CLOS(x,c')::xs -> secd (Cls(x, c', e)::s) e xs d
| APP::xs -> (match s with
	(y1::(y2::ys)) -> (match y2 with 
		Cls(x, c', e') -> secd [] ((x, y1)::e') c' ((ys, e, xs)::d)
		| _ -> raise Illegal_Stack)
	| _ -> raise Illegal_Stack)
	| (RET::xs) -> (match s with
		(a::s') -> (match d with
			((s1, e1, c1)::d') -> secd (a::s1) e1 c1 d'
			| _ -> raise Illegal_Stack)
		| _ -> raise Illegal_Stack)
;;
			









					 


