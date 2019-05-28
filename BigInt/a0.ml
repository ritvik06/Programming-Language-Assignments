exception Divide_by_Zero;;

type sign = Neg | NonNeg ;;
type bigint = sign*int list ;; 

let sign x = match x with
(s1,l1) -> if(s1=Neg) then Neg
			else NonNeg;;

(* function to invert a given int list *)
let rec inv l1 : int list= match l1 with
	[] -> []
	| x::xs -> (inv xs)@[x];; 

let rec negate (s1,l1) : (bigint) = match s1 with
	Neg -> (NonNeg,l1)
	|NonNeg -> (Neg,l1);;

(* Func to remive extra leading zeroes *)
let rec trim_zeroes l = match l with
	[] -> []
	| 0::xs -> trim_zeroes xs
	| x::xs -> x::xs ;;

(* minus: bigint -> bigint *)
let rec minus (x: bigint) : bigint = match x with
	(NonNeg , [] ) -> (Neg , [] )
	|(Neg , [] ) -> (NonNeg , [] )
	| (Neg , l) -> (NonNeg , trim_zeroes (l)) 
	| (NonNeg , l) -> (Neg , trim_zeroes (l));;

(* Absolute value *)
let rec abs (x: bigint) : bigint =  match x with
	(NonNeg , [] ) -> (NonNeg , [] )
	|(Neg , [] ) -> (NonNeg , [] )
	|(Neg , l) -> (NonNeg , trim_zeroes (l)) 
	| (NonNeg , l) -> (NonNeg , trim_zeroes (l));;
 
(* Comparison operations:  *)
  
 (* Equal *)
let rec eq (x: bigint) (y: bigint)  : bool = match x with
	( _, []) -> (match y with
				( _, [] ) -> true
				| ( _, y::ys) -> false )
	|(NonNeg, x1::lx) -> (match y with
				   ( NonNeg, y1::ly) -> if x1=y1 then (eq (NonNeg, lx) (NonNeg, ly))
				   						else false
				   | _ -> false )
	|(Neg, x1::lx) ->(match y with
				   ( Neg, y1::ly) -> if x1=y1 then (eq (Neg, lx) (Neg, ly))
				   						else false
				   | _ -> false);;

let rec length l = match l with
	[] -> 0
	| x::xs -> (length xs) + 1;;

(* Great_or_equal. *)
let rec geq ((s1,l1): bigint) ((s2,l2): bigint)  : bool = let l1 = (trim_zeroes l1) and l2 = (trim_zeroes l2) in match (s1,l1) with 
	( _, []) -> true
	|( Neg, x::xs) -> (match (s2,l2) with
				( NonNeg, y::ys ) -> false
				| (Neg, y::ys) -> (if (length l1) > (length l2) then false else if (length l1) < (length l2) then true 
				else (if x<y then true else if x>y then false else (geq (Neg,xs) (Neg,ys)) ) )
				| (_,_) -> false)
	|( NonNeg, x::xs) -> (match (s2,l2) with
				( Neg, y::ys ) -> true
				| (NonNeg, y::ys) -> (if (length l1) > (length l2) then true else if (length l1) < (length l2) then false
				else (if x<y then false else if x>y then true else (geq (NonNeg,xs) (NonNeg,ys))) )
				| (_,_) -> false );;

(* Less_or_equal.  *)
let rec leq ((s1,l1): bigint) ((s2,l2): bigint)  : bool = let l1 = (trim_zeroes l1) and l2 = (trim_zeroes l2) in
															match (s1,l1) with 
	( _, []) -> true
	|( Neg, x::xs) -> (match (s2,l2) with
				( NonNeg, y::ys ) -> true
				| (Neg, y::ys) -> (if (length (trim_zeroes l1)) > (length (trim_zeroes l2)) then true else if (length (trim_zeroes l1)) < (length (trim_zeroes l2)) then false else
									if x<y then false else if x>y then true else (leq (Neg,xs) (Neg,ys)) )
				| (_,_) -> false)
	|( NonNeg, x::xs) -> (match (s2,l2) with
				( Neg, y::ys ) -> false
				| (NonNeg, y::ys) -> (if (length (trim_zeroes l1)) > (length (trim_zeroes l2)) then false else if (length (trim_zeroes l1)) < (length (trim_zeroes l2)) then true else 
									  if x<y then true else if x>y then false else (leq (NonNeg,xs) (NonNeg,ys)) )
				| (_,_) -> false );;

(* Greater_than. *)
let rec gt ((s1,l1): bigint) ((s2,l2): bigint)  : bool =  let l1 = (trim_zeroes l1) and l2 = (trim_zeroes l2) in  match (s1,l1) with 
	( _, []) -> false
	|( Neg, x::xs) -> (match (s2,l2) with
				( NonNeg, y::ys ) -> false
				| (Neg, y::ys) -> (if (length l1) > (length l2) then false else if (length l1) < (length l2) then true 
				else (if x<y then true else if x>y then false else (geq (Neg,xs) (Neg,ys)) ) )
				| (_,_) -> false)
	|( NonNeg, x::xs) -> (match (s2,l2) with
				( Neg, y::ys ) -> true
				| (NonNeg, y::ys) -> (if (length l1) > (length l2) then true else if (length l1) < (length l2) then false
				else (if x<y then false else if x>y then true else (geq (NonNeg,xs) (NonNeg,ys))) )
				| (_,_) -> false );;

(* Less_than. *)
let rec lt ((s1,l1): bigint) ((s2,l2): bigint)  : bool =  let l1 = (trim_zeroes l1) and l2 = (trim_zeroes l2) in match (s1,l1) with 
	( _, []) -> false
	|( Neg, x::xs) -> (match (s2,l2) with 
					  (NonNeg,y::ys) -> true
					  | (Neg, y::ys) -> (if (length l1) > (length l2) then true else if (length l1) < (length l2) then false
					  					else (if x<y then false else if x>y then true else (lt (Neg,xs) (Neg,ys))) )
					  | (_,_) -> false )
	|( NonNeg, x::xs) -> (match (s2,l2) with
				( Neg, y::ys ) -> false
				| (NonNeg, y::ys) -> (if ((length l1) > (length l2)) then false else if ((length l1) < (length l2)) then true  
				else (if x<y then true else if x>y then false else (lt (NonNeg,xs) (NonNeg,ys))) )
				| (_,_) -> false );;

(* Helper function for add and sub *)
(* takes inverted list as input and returns inverted output of int list -  add and sub list *)
let rec add_list l1 l2 c = match l1 with
	[] -> (match l2 with
		  [] -> if (c=1) then [c] else []
		  | y::ys -> if (y+c>9) then (y+c-10)::(add_list [] ys 1)
		  		     else (y+c)::ys)
	| x::xs -> (match l2 with
			   [] -> if (x+c>9) then (x+c-10)::(add_list xs [] 1)
			   		 else (x+c)::xs
			   | y::ys -> if x+y+c>9 then (x+y+c-10)::(add_list xs ys 1)
			   			  else (x+y+c)::(add_list xs ys 0) );;
	
let rec sub_list l1 l2 c = match l1 with  
	[] -> []
	| x::xs -> (match l2 with
			   [] -> (if (x<c) then (x+10-c)::(sub_list xs [] 1) 
			   		 else (x-c)::(sub_list xs [] 0))
			   | y::ys -> (if ((x-c)>=y) then (x-c-y)::(sub_list xs ys 0)
			   			  else (10+x-c-y)::(sub_list xs ys 1)));;

(* Addition *)
let rec add ((s1,l1): bigint) ((s2,l2): bigint) : bigint = let l1 = (trim_zeroes l1) and l2 = (trim_zeroes l2) in match (s1,l1) with
	(_,[]) -> (s2,l2)
	| (NonNeg, l1) -> (if(s2=NonNeg) then (NonNeg, trim_zeroes (inv (add_list (inv l1) (inv l2) 0)))
					  else if(gt (NonNeg,l1) (NonNeg,l2)) then (NonNeg, trim_zeroes (inv (sub_list (inv l1) (inv l2) 0)))
					  	   else (Neg, trim_zeroes (inv (sub_list (inv l2) (inv l1) 0)))   )
	| (Neg, l1) ->( if(s2=Neg) then (Neg, trim_zeroes (inv (add_list (inv l1) (inv l2) 0)))
				   else if(gt (NonNeg,l1) (NonNeg,l2)) then (Neg, trim_zeroes (inv (sub_list (inv l1) (inv l2) 0)))
				   		else (NonNeg, trim_zeroes (inv (sub_list (inv l2) (inv l1) 0))) );;  

(* Subtraction *)
let rec sub ((s1,l1): bigint) ((s2,l2): bigint) : bigint = let l1 = (trim_zeroes l1) and l2 = (trim_zeroes l2) in match (s1,l1) with
	(_,[]) -> if (s2 = NonNeg) then (Neg, l2) else (NonNeg, l2)
	| (NonNeg, l1) -> (if(s2=Neg) then (NonNeg, trim_zeroes (inv (add_list (inv l1) (inv l2) 0)))
					  else if(gt (NonNeg,l1) (NonNeg,l2)) then (NonNeg, trim_zeroes (inv (sub_list (inv l1) (inv l2) 0)))
					  	   else (Neg, trim_zeroes (inv (sub_list (inv l2) (inv l1) 0)))  )
	| (Neg, l1) -> (if(s2=NonNeg) then (Neg, trim_zeroes (inv (add_list (inv l1) (inv l2) 0)))
				   else if(gt (NonNeg,l1) (NonNeg,l2)) then (Neg, trim_zeroes (inv (sub_list (inv l1) (inv l2) 0)))
				   		else (NonNeg, trim_zeroes (inv (sub_list (inv l2) (inv l1) 0))));;  

(*Helper function for mult - mult_a and mult_list *)
(*takes inverted list l and multiply with int a and returns an inverted list as output*)
let rec mult_a a l c = match l with
	[] -> [c]
	| x::xs -> let d= (a*x)+c
			   in (d mod 10)::(mult_a a xs (d/10)) ;;

(* Shifts and adds the the output lists of mult_a to give an inverted int list of product as output *)
let rec mult_list l1 l2 = match l1 with
	[] -> [0]
	| x::xs -> (match l2 with
				[] -> [0] 
				| y::ys -> add_list (mult_a x (y::ys) 0) (0::(mult_list xs (y::ys))) 0 );;

(* Main multiply function *)
let rec mult ((s1,l1): bigint) ((s2,l2): bigint) : bigint = let l1 = (trim_zeroes l1) and l2 = (trim_zeroes l2) in match (s1,l1) with
	(_, []) -> (s2,[0])
	| (NonNeg, l1) -> if (s2=Neg) then (Neg, trim_zeroes (inv (mult_list (inv l1) (inv l2)))) else (NonNeg,trim_zeroes (inv (mult_list (inv l1) (inv l2))))
	| (Neg, l1) -> if (s2=NonNeg) then (Neg, trim_zeroes (inv (mult_list (inv l1) (inv l2)))) else (NonNeg,trim_zeroes (inv (mult_list (inv l1) (inv l2))));;

(* Helper functions for division - add_zeroes, multiple, multiple_addZero, remain, div_list *)
(* func to add 0 at the end of divisor untill divisor is greater than dividend *)
let rec add_zeroes l1 l2 =  match l1 with
	[] -> raise(Divide_by_Zero)
	| x::xs -> if(leq (NonNeg, (x::xs)@[0]) (NonNeg, l2) ) then add_zeroes ((x::xs)@[0]) l2 else (x::xs) ;;

(* Gives l2/l1 where l1 and l2 are of almost same order *)
let rec multiple l1 l2 c = let l1 = (trim_zeroes l1) and l2 = (trim_zeroes l2) in match l1 with
	[] -> raise(Divide_by_Zero)
	| [0] -> raise(Divide_by_Zero)
	| x::xs -> if(leq (NonNeg, l1) (NonNeg, l2)) then (multiple l1 (inv (sub_list (inv l2) (inv l1) 0)) (c+1)) else c ;; 

(* Represent a single step of long division - If final quotient is 231 then it calculates 200, 30 and 1 in 3 steps and the adds all 200+30+1 *)
let rec multiple_addZero l1 l2 ans = match l1 with
	[] -> []
	| [0] -> raise(Divide_by_Zero)
	|  x::xs -> if(leq (NonNeg, (x::xs)@[0]) (NonNeg, l2) ) then (multiple_addZero ((x::xs)@[0]) l2 (0::ans)) else (multiple l1 l2 0)::ans ;; 

(* Gives remainder when l2 is divided by l1 *)
let rec remain l1 l2 = match l1 with
	[] -> l2
	| [0] -> raise(Divide_by_Zero)
	| x::xs -> if(leq (NonNeg, l1) (NonNeg, l2)) then (remain l1 (inv (sub_list (inv l2) (inv l1) 0))) else l2 ;; 

(* Divides int list l2 by l1 *)
let rec div_list l1 l2 = let l1 = (trim_zeroes l1) and l2 = (trim_zeroes l2) in match l1 with
	[] -> raise(Divide_by_Zero)
	| [0] -> raise(Divide_by_Zero)
	| x::xs -> if (leq (NonNeg, l1) (NonNeg, l2)) then inv(add_list (inv (multiple_addZero l1 l2 [])) (inv (div_list l1 (remain (add_zeroes l1 l2) l2 )) ) 0) 
			   else [0] ;;				

(* Main div function - long division implemented *)
let rec div ((s1,l1): bigint) ((s2,l2): bigint) : bigint = let l1 = (trim_zeroes l1) and l2 = (trim_zeroes l2) in match (s1,l1) with
	(_, []) -> (s2,[0])
	| (NonNeg, l1) -> if (s2=Neg) then (Neg, trim_zeroes (div_list l2 l1)) else (NonNeg, trim_zeroes (div_list l2 l1))
	| (Neg, l1) -> if (s2=NonNeg) then (Neg, trim_zeroes (div_list l2 l1)) else (NonNeg, trim_zeroes (div_list l2 l1));;

(* Main rem function *)
let rec rem ((s1,l1): bigint) ((s2,l2): bigint) : bigint = let l1 = (trim_zeroes l1) and l2 = (trim_zeroes l2) in match (s1,l1) with
	(_, []) -> (s2,[0])
	| (NonNeg, l1) -> if (s2=Neg) then (NonNeg, trim_zeroes (inv (sub_list (inv l2) (inv (remain l2 l1)) 0))) else (NonNeg, (trim_zeroes (remain l2 l1)))
	| (Neg, l1) -> if (s2=NonNeg) then (NonNeg, trim_zeroes (inv (sub_list (inv l2) (inv (remain l2 l1)) 0))) else (NonNeg, (trim_zeroes (remain l2 l1)));;

(*converts positive ints to list of int*)
let rec int_to_list a = match a with
	0 -> []
	| _ -> (int_to_list (a/10))@[a mod 10] ;; 

(* Conversion functions from OCaml int to bigint. *)
let mk_big a = 
	if (a>0) then (NonNeg, int_to_list a) else (Neg, int_to_list (-a));;

(* Functions to present the result in the form of a string. *)
let rec print_num bigInt = match bigInt with
	(_, []) -> ""
	| (NonNeg, x::xs) -> (string_of_int x)^(print_num (NonNeg, xs))
	| (Neg, x::xs) -> "-"^(string_of_int x)^(print_num (NonNeg, xs));;
 