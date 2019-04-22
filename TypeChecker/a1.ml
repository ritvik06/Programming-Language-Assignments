(* Dummy implementation of A1 *)
open A0
exception Not_implemented
exception EmptyTuple
exception NoMatch
exception InvalidIndex

(* abstract syntax *)
type exptype = Tint | Tunit | Tbool | Ttuple of (exptype list) | Tfunc of (exptype * exptype)

type  exptree =
  Var of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
  | N of int      (* Integer constant *)
  | B of bool     (* Boolean constant *)
  (* unary operators on integers *)
  | Abs of exptree                   (* abs *)
  | Negative of exptree              (* unary minus ~ *)
  (* unary operators on booleans *)
  | Not of exptree
  (* binary operators on integers *)
  | Add of exptree * exptree         (* Addition + *)
  | Sub of exptree * exptree         (* Subtraction - *)
  | Mult of exptree * exptree        (* Multiplication * *)
  | Div of exptree * exptree         (* div *)
  | Rem of exptree * exptree         (* mod *)
  (* binary operators on booleans *)
  | Conjunction of exptree * exptree (* conjunction /\ *)
  | Disjunction of exptree * exptree (* binary operators on booleans \/ *)
  (* comparison operations on integers *)
  | Equals of exptree * exptree      (* = *)
  | GreaterTE of exptree * exptree   (* >= *)
  | LessTE of exptree * exptree      (* <= *)
  | GreaterT of exptree * exptree    (* > *)
  | LessT of exptree * exptree       (* < *)
  (* expressions using parenthesis *)
  | InParen of exptree               (* ( ) *)
  (* a conditional expression *)
  | IfThenElse of exptree * exptree * exptree (* if then else fi  *)
  (* creating n-tuples (n >= 0) *)
  | Tuple of int * (exptree list)
  (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Project of (int*int) * exptree   (* Proj((i,n), e)  0 < i <= n *)
  | Let of definition  * exptree
  | FunctionAbstraction of string * exptype * exptree
  | FunctionCall of exptree * exptree
(* definition *)
and definition =
    Simple of string * exptree
  | Sequence of (definition list)
  | Parallel of (definition list)
  | Local of definition * definition

(* opcodes of the stack machine (in the same sequence as above) *)
type opcode = VAR of string | NCONST of bigint | BCONST of bool | ABS | UNARYMINUS | NOT
  | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | EQS | GTE | LTE | GT | LT
  | PAREN | IFTE | TUPLE of int | PROJ of int*int | LET | FABS | FCALL
  | SIMPLEDEF | SEQCOMPOSE | PARCOMPOSE | LOCALDEF

(* The possible types of expressions in the language of expressions *)
(* The type of value returned by the definitional interpreter. *)
type value = NumVal of int | BoolVal of bool | TupVal of int * (value list)

(* The language should contain the following types of expressions:  integers and booleans *)
type answer = Num of bigint | Bool of bool | Tup of int * (answer list)

let rec val_int (x:value):int = match x with
NumVal x -> x;;

let rec val_bool (x:value):bool = match x with
BoolVal x -> x;;

let rec toAnswer v = match v with
  NumVal a     -> Num (mk_big a)
| BoolVal b    -> Bool b
| TupVal (n,xs) -> Tup (n, List.map toAnswer xs);;

let rec big_int (x:bigint):int = match x with
(NonNeg,l) -> ( let rec multiplier (l1:int list) (value:int) = match l1 with 
                  [] -> value            
                  |x::xs -> multiplier xs (value*10 + x)
                in multiplier l 0
              )
|(Neg,l) -> ( let rec multiplier (l1:int list) (value:int) = match l1 with 
                  [] -> value            
                  |x::xs -> multiplier xs (value*10 - x)
                in multiplier l 0
              );; 

let ifte (a:bool) (b:bigint) (c:bigint) :bigint = (if(a=true) then b else c);;

(* let rec eval ex rho = raise Not_implemented *)
let rec eval ex rho  = match ex with 
  (* Done ->    *)
    Var s -> rho s
    | N a -> NumVal a
    | B a  -> BoolVal a
    | Abs a -> if (val_int (eval a rho))<0 then NumVal(-1*(val_int (eval a rho))) else NumVal(val_int (eval a rho))
    | Negative a -> NumVal(-1*(val_int (eval a rho)))
    | Not a -> if((eval a rho)==BoolVal true) then BoolVal false else BoolVal true
    | Add (a,b) -> NumVal((val_int (eval a rho )) + (val_int (eval b rho)))
    | Sub (a,b) -> NumVal( (val_int (eval a rho)) - (val_int (eval b rho)))  
    | Mult (a,b) -> NumVal( (val_int (eval a rho)) * (val_int (eval b rho)))
    | Div (a,b) -> NumVal ((val_int (eval a rho)) / (val_int (eval b rho)))
    | Rem (a,b) ->  NumVal( (val_int (eval a rho)) mod (val_int (eval b rho)))
    | Conjunction(a,b) -> BoolVal( (val_bool (eval a rho)) & (val_bool (eval b rho)))
    | Disjunction(a,b) -> BoolVal( (val_bool (eval a rho)) or (val_bool (eval b rho)))
    | Equals (a,b) -> BoolVal((val_int (eval a rho))==(val_int (eval b rho)))
    |GreaterTE (a,b) -> BoolVal((val_int (eval a rho))>=(val_int (eval b rho)))
    |LessTE (a,b) -> BoolVal((val_int (eval a rho))<=(val_int (eval b rho)))
    |GreaterT (a,b) -> BoolVal((val_int (eval a rho))>(val_int (eval b rho)))
    |LessT (a,b) -> BoolVal((val_int (eval a rho))<(val_int (eval b rho)))
    |InParen (a) ->  (eval a rho)
    |IfThenElse (a,b,c) -> if ((eval a rho)=(BoolVal true)) then (eval b rho) else (eval c rho)      
    |Tuple (a,b) -> (if (a=0) then raise EmptyTuple
                    else if (a=1) then TupVal (1,[eval (List.hd b) rho])  
                  else (let rec makeTuple (l1:exptree list) (out:value list) = match l1 with
                        [] -> out 
                        |x::xs -> makeTuple xs (out@[(eval x rho)])

                      in TupVal(a, makeTuple b [])          
                        )               
                      )   
     |Project((i,n),e) ->( match e with 
                         Tuple(n,exlist) ->   (if (n=0) then raise EmptyTuple
                            else if(n=1) then (if (i=1) then (eval (List.hd exlist) rho) else raise InvalidIndex)
                            else  ( let rec projection a b l1 = match l1 with
                                    x::xs -> (if ((List.length xs)=(a-b)) then (eval x rho)
                                              else projection a b xs)
                                    in projection n i exlist
                                )
                          ));;

   let rec list_breaka (i:int) (l1:answer list) (l2:answer list) = 
                          if((List.length l1)==i) then l2
                        else list_breaka i (l1@[List.hd l2]) (List.tl l2)
  
   let rec list_breakb (i:int) (l1:answer list) (l2:answer list) = 
                          if((List.length l1)==i) then l1
                        else list_breaka i (l1@[List.hd l2]) (List.tl l2)
                       

    let rec stackmc (list1:answer list) binding (oplist:opcode list) = match oplist with
      VAR(s)::xs -> (stackmc ((binding s)::list1) binding xs)
      |NCONST(i)::xs -> (match i with
                            x -> stackmc ((Num x)::list1) binding xs)
      |BCONST(i)::xs -> (match i with
                            x -> stackmc (Bool(x)::list1) binding xs)
      |ABS::xs -> (match list1 with
              (y1::ys) -> (match y1 with
                            Num x -> stackmc (Num(abs x)::ys) binding xs)
              |[] -> Num (NonNeg,[0])
              ) 
      |UNARYMINUS::xs -> (match list1 with
              (y1::ys) -> (match y1 with
                            Num x -> stackmc (Num(minus x)::ys) binding xs)
              |[] -> Num (NonNeg,[0])     
            )
      |NOT::xs -> (match list1 with 
              (y1::ys) -> (match y1 with 
                            Bool x -> stackmc (Bool(not x)::ys) binding xs)
              |[] -> Num (NonNeg,[0])
              ) 
      |PLUS::xs -> (match list1 with
              (y1::(y2::ys)) -> (match y1 with
                            Num x1  -> (match y2 with
                            Num x2  ->  stackmc (Num(add x1 x2)::ys) binding xs))
              |_ -> List.hd list1
            )
      |MULT::xs -> (match list1 with
              (y1::(y2::ys)) -> (match y1 with
                            Num x1  -> (match y2 with
                            Num x2  ->  stackmc (Num(mult x1 x2)::ys) binding xs))
              |_ -> List.hd list1
            )
      |MINUS::xs -> (match list1 with
              (y1::(y2::ys)) -> (match y1 with
                            Num x1  -> (match y2 with
                            Num x2  ->  stackmc (Num(sub x2 x1)::ys) binding xs))
              |_ -> List.hd list1
            )
      |DIV::xs -> (match list1 with
              (y1::(y2::ys)) -> (match y1 with
                            Num x1  -> (match y2 with
                            Num x2  ->  stackmc (Num(div x2 x1)::ys) binding xs))
              |_ -> List.hd list1
            )
      |REM::xs -> (match list1 with
              (y1::(y2::ys)) -> (match y1 with
                            Num x1  -> (match y2 with
                            Num x2  ->  stackmc (Num(rem x2 x1)::ys) binding xs))
              |_ -> List.hd list1
            ) 
      |CONJ::xs -> (match list1 with
              (y1::(y2::ys)) -> (match y1 with
                            Bool x1  -> (match y2 with
                            Bool x2  ->  stackmc (Bool( x1 & x2)::ys) binding xs))
              |_ -> List.hd list1
            )   
      |DISJ::xs -> (match list1 with
              (y1::(y2::ys)) -> (match y1 with
                            Bool x1  -> (match y2 with
                            Bool x2  ->  stackmc (Bool( x1 or x2)::ys) binding xs))
              |_ -> List.hd list1
            ) 
      |EQS::xs -> (match list1 with
              (y1::(y2::ys)) -> (match y1 with
                            Num x1  -> (match y2 with
                            Num x2  ->  stackmc (Bool(eq x1 x2)::ys) binding xs))
              |_ -> List.hd list1
            )
       |GTE::xs -> (match list1 with
              (y1::(y2::ys)) -> (match y1 with
                            Num x1  -> (match y2 with
                            Num x2  ->  stackmc (Bool(geq x2 x1)::ys) binding xs))
              |_ -> List.hd list1
            )
       |LTE::xs -> (match list1 with
              (y1::(y2::ys)) -> (match y1 with
                            Num x1  -> (match y2 with
                            Num x2  ->  stackmc (Bool(leq x2 x1)::ys) binding xs))
              |_ -> List.hd list1
            )
       |GT::xs -> (match list1 with
              (y1::(y2::ys)) -> (match y1 with
                            Num x1  -> (match y2 with
                            Num x2  ->  stackmc (Bool(gt x2 x1)::ys) binding xs))
              |_ -> List.hd list1
            )
       |LT::xs -> (match list1 with
              (y1::(y2::ys)) -> (match y1 with
                            Num x1  -> (match y2 with
                            Num x2  ->  stackmc (Bool(lt x2 x1)::ys) binding xs))
              |_ -> List.hd list1
            )
       |PAREN::xs ->  stackmc list1 binding xs
       |IFTE::xs -> (match list1 with
              (y1::(y2::(y3::ys))) -> (match y1 with
                            Num x1  -> (match y2 with   
                            Num x2  ->(match y3 with
                            Bool x3  ->  stackmc (Num(ifte x3 x2 x1)::ys) binding xs)))
              |_ -> List.hd list1
            )
       (* |TUPLE(i)::xs ->  stackmc [(Tup(i,(list_breakb i [] list1)))]@(list_breaka i [] list1) binding xs *)
       |TUPLE(i)::xs -> stackmc [Tup(i,(* List.rev *) list1)] binding xs
       (* ((let rec makeTuple l1 out = match l1 with
                                  [] -> out
                                  |b -> makeTuple [] (out@eval(b)) 
                                  |b::bs -> makeTuple bs (out@eval(b))  
                        in stackmc Tup(i,makeTuple list1 []) xs ))
     *) |PROJ(i,n)::xs -> (match list1 with
                            Tup(k,anslist)::zs ->(if(n=0) then raise EmptyTuple
                                                   else if(n=1) then (if (k=1) then (stackmc [(List.hd anslist)] binding xs) else raise InvalidIndex)
                                                 else ( let rec projectstk a b l1 = match l1 with
                                    y::ys -> (if ((List.length ys)=(b-1)) then (stackmc (y::zs) binding xs)
                                              else projectstk a b ys)

                                    in projectstk n i anslist
                                )
                                                  )
                            |_ -> raise NoMatch)
        |[] -> (List.hd list1);;
            
  
  (* let stackmc list1 oplist = raise Not_implemented *)

  let rec compile (tree:exptree) :opcode list = match tree with
    Var str -> [VAR(str)]
    |  N num -> [NCONST(mk_big num)]  
    | B a -> [BCONST(a)]
    | Negative a -> (compile a)@ [UNARYMINUS] 
    | Abs a ->(compile a) @ [ABS] 
    | Not a -> (compile a) @ [NOT]
    | Add (a,b) -> (compile a)@(compile b)@ [PLUS]
    | Sub (a,b) -> (compile a)@(compile b)@ [MINUS]
    | Mult (a,b) -> (compile a)@(compile b)@ [MULT]
    | Div (a,b) -> (compile a)@(compile b)@ [DIV]
    | Rem (a,b) -> (compile a)@(compile b)@ [REM]
    | Conjunction(a,b) -> (compile a)@(compile b)@ [CONJ]
    | Disjunction(a,b) -> (compile a)@(compile b)@ [DISJ]
    | Equals(a,b) -> (compile a)@(compile b)@ [EQS]
    |GreaterTE(a,b) -> (compile a)@(compile b)@ [GTE]
    |LessTE(a,b) -> (compile a)@(compile b)@ [LTE]
    |GreaterT(a,b) -> (compile a)@(compile b)@ [GT]
    |LessT(a,b) -> (compile a)@(compile b)@ [LT]
    |InParen(a) -> (compile a)@ [PAREN]
    |IfThenElse(a,b,c) -> (compile a)@(compile b)@(compile c)@[IFTE]
    |Tuple(a,b) -> (let rec tupOp (list1:exptree list) (list2:opcode list) = match list1 with
                    [] -> list2
                    |x::xs -> tupOp xs (list2@(compile x))
                  in (tupOp b [])@[TUPLE(a)]
                    )
   |Project((i,n),e) -> (compile e)@[PROJ(i,n)];;