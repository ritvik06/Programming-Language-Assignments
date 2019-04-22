open A1
exception Not_implemented
exception Invalid_Exp
exception DidntEnter
exception Failure

let rho s = match s with 
   "X" -> NumVal 5
|  "Y" -> BoolVal true
|  "Z" -> TupVal (3, [NumVal 5; BoolVal true; NumVal 1]);;


 let rec find (table: (string *exptype) list) (str:string) = match table with
[] -> Tunit
|(a,b)::xs  -> (if (a=str) then b else (find xs str))
;;

let rec isMember (table: (string *exptype) list) (str:string) = match table with
[] -> false
|((a,b))::xs  -> (if (a=str) then true else (isMember xs str))
;;

let rec isSub (g: (string *exptype) list) (g_dash: (string *exptype) list) = match g_dash with
[] -> true
|((s,t)::ys) -> (if (isMember g s) then (isSub g ys) else false);;


let rec tupType (getType : (((string *exptype) list) -> exptree -> exptype )) (table: (string * exptype) list) (x_list:(exptree list)) = match x_list with
[] -> []
|x::xs -> (getType table x)::(tupType getType table xs)
;;

let rec remove_conflicts a x = match a with
|[] -> []
|h::t -> if(h==x) then h::(remove_conflicts t x) else raise Failure
;;

let rec same (g: (string *exptype) list) (g_dash: (string *exptype) list) :bool = (isSub g g_dash)&&(isSub g_dash g);;


let rec check_type a = match a with
| [] -> []
|h::t -> h::(remove_conflicts t h)
;;

let rec remove_same (g: (string *exptype) list) (out: (string *exptype) list) = match g with
[] -> out
|((s,t)::xs) -> if (isMember g s) then (remove_same xs out) else (remove_same xs ((s,t)::out));;



let rec get_def (getType : (((string *exptype) list) -> exptree -> exptype )) (g: (string * exptype) list) (d:definition) = match d with
Simple(s,x) -> (s,getType g x)::[]
|Sequence(list1) -> (match list1 with
					 [] -> []
					 |y::ys  -> (get_def (getType) g y) @ (get_def (getType) (g@(get_def (getType) g y)) (Sequence(ys)))	
					 )
|Parallel(list1) -> (match list1 with
					 [] -> []
					 |y::ys -> check_type (get_def (getType) g y)@(get_def (getType) g (Parallel(ys)))
					)
|Local(d1,d2) -> (get_def getType (g@(get_def getType g d1)) d2)

let rec union g g_dash = g@g_dash;;

 let rec getType (g: (string *exptype) list) (e : exptree) = match e with
 Var(s) -> find g s
 |N(x)-> Tint
 |B(x) -> Tbool
 |Abs(x) -> if (getType g x=Tint) then Tint else Tunit
 |Negative(x) ->if (getType g x=Tint) then Tint else Tunit
 |Not(x) -> if (getType g x=Tbool) then Tbool else Tunit
 |Add(n1,n2) -> if(((getType g n1)=Tint) && ((getType g n2)=Tint)) then Tint else Tunit
 |Sub(n1,n2) -> if(getType g n1=Tint && getType g n2=Tint) then Tint else Tunit
 |Mult(n1,n2) -> if(getType g n1=Tint && getType g n2=Tint) then Tint else Tunit
 |Div(n1,n2) -> if(getType g n1=Tint && getType g n2=Tint) then Tint else Tunit
 |Conjunction(n1,n2) -> if(getType g n1=Tbool && getType g n2=Tbool) then Tbool else Tunit
 |Disjunction(n1,n2) -> if(getType g n1=Tbool && getType g n2=Tbool) then Tbool else Tunit
 |Equals(n1,n2) -> if(getType g n1=Tint && getType g n2=Tint) then Tbool
 				   else if (getType g n1=Tbool && getType g n2=Tbool) then Tbool
 				   else Tunit 
 |GreaterTE(n1,n2) -> if(((getType g n1)=Tint) && ((getType g n2)=Tint)) then Tbool else Tunit 
 |GreaterT(n1,n2) -> if(getType g n1=Tint && getType g n2=Tint) then Tbool else Tunit 
 |LessTE(n1,n2) -> if(getType g n1=Tint && getType g n2=Tint) then Tbool else Tunit 
 |LessT(n1,n2) -> if(getType g n1=Tint && getType g n2=Tint) then Tbool else Tunit 
 |InParen(x) -> getType g x
 |IfThenElse (a,b,c) -> if (getType g a=Tbool) then (if (eval a rho)=(BoolVal true) then getType g b else getType g c) else Tunit
 |Tuple(n,list1) -> (if (n=0) then Tunit 
 					else if (n!=List.length(list1)) then Tunit 
 					else Ttuple(tupType getType g list1)) 
 |Project((i,n),e) ->(match eval e rho with
					   NumVal n -> Tint
					   |BoolVal b -> Tbool
					   |_ -> Tunit)
 |Let(d,x) -> (getType ((get_def getType g d)@g) x)
 |FunctionAbstraction(s,t,x) -> Tfunc(t,(getType ((s,t)::g) x))
 |FunctionCall(a1,a2) -> (match (getType g a1) with
 						 Tfunc(t1,t2) -> (match (getType g a2) with
 						 				  t3 -> if(t3=t1) then t2 else Tunit
 						 				  |_ -> Tunit
 										)
 						 |_           -> Tunit);;

(* 
 let rec hastype g e t = match e with
 Var(x) -> if(t=find g x) then true else false
|N(x) -> if (t=Tint) then true else false
|B(x) -> if(t=Tbool) then true else false
|Abs(x) -> if(t=Tint) then (if (hastype g x Tint) then true else false) else false
|Negative(x) -> if(t=Tint) then (if (hastype g x Tint) then true else false) else false
|Not(x) -> if (t=Tbool) then (if (hastype g x Tbool) then true else false) else false
|Add(n1,n2) -> if (t=Tint) then (let x = hastype g n1 Tint in 
 								let y = hastype g n2 Tint in 
 								if(x&&y=true) then true else false) else false
|Sub(n1,n2) -> if (t=Tint) then (let x = hastype g n1 Tint in 
 								let y = hastype g n2 Tint in 
 								if(x&&y=true) then true else  false) else false
|Mult(n1,n2) -> if (t=Tint) then (let x = hastype g n1 Tint in 
 								let y = hastype g n2 Tint in 
 								if(x&&y=true) then true else  false) else false
|Div(n1,n2) -> if (t=Tint) then (let x = hastype g n1 Tint in 
 								let y = hastype g n2 Tint in 
 								if(x&&y=true) then true else  false) else false
|Conjunction(n1,n2) -> if (t=Tbool) then (let x = hastype g n1 Tbool in 
 										  let y = hastype g n2 Tbool in 
 								          if(x&&y=true) then true else  false) else false
|Disjunction(n1,n2) -> if (t=Tbool) then (let x = hastype g n1 Tbool in 
 										  let y = hastype g n2 Tbool in 
 								          if(x&&y=true) then true else  false) else false	
|Equals(n1,n2) -> if(t=Tbool) then (if ((hastype g n1 Tbool)&&(hastype g n2 Tbool)=true)||((hastype g n1 Tint)&&(hastype g n2 Tint)=true) then true else false) else false
|GreaterTE(n1,n2) -> if(t=Tbool) then (if ((hastype g n1 Tint)&&(hastype g n2 Tint)=true) then true else false) else false
|GreaterT(n1,n2) -> if(t=Tbool) then (if ((hastype g n1 Tint)&&(hastype g n2 Tint)=true) then true else false) else false
|LessTE(n1,n2) -> if(t=Tbool) then (if ((hastype g n1 Tint)&&(hastype g n2 Tint)=true) then true else false) else false
|LessT(n1,n2) -> if(t=Tbool) then (if ((hastype g n1 Tint)&&(hastype g n2 Tint)=true) then true else false) else false
|InParen(x) -> if (t=getType g x) then true else false
|IfThenElse(a,b,c) -> if(t=getType g e) then true else false
|Let(d,a) -> if(t=getType g e) then true else false  *)
let rec hastype g e t = if ((getType g e)=t) then true else false;;



let rec yields g d g_dash  :bool = (same (remove_same(g@(get_def (getType) g d)) [] ) (remove_same g_dash []));;

