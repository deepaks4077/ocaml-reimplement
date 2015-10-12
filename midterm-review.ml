(*
	This is a re-implementation of the core ocaml module Lists
	@Author Deepak Sharma
	@Date October 11, 2015
*)

exception Failure of string ;;

let rec length_aux l carry = match l with
	| [] -> carry
	| a::l -> length_aux l carry+1
;;

let length l = length_aux l 0 ;; 

let hd l = match l with
	| [] -> raise (Failure "hd")
	| a::l -> a
;;

let tl l = match l with
	| [] -> raise (Failure "tl")
	| a::l -> l
;;

let nth_list n l =
	if n < 0 
		then raise (Failure "invalid argument")
	else
		let rec nth_aux n l =  
			match l with 
			| [] -> raise (Failure "nth")
			| a::l -> if n = 0 then l else nth_aux (n-1) l 
		in nth_aux n l
;;

let nth l n = 
	if n < 0 
		then raise (Failure "invalid argument")
	else
		let rec nth_aux l n =  
			match l with 
			| [] -> raise (Failure "nth")
			| a::l -> if n = 0 then a else nth_aux l (n-1)
		in nth_aux l n
;;

let rec rev_append l1 l2 = match l1 with
	| [] -> l2
	| a::l -> rev_append l (a::l2)
;;


let rev l = 
	if l = [] 
		then raise (Failure "Empty list")
	else
		rev_append l []
;;

let rec concat l = match l with
	| [] -> []
	| l1::l2 -> l1@concat l2
;;

let flatten = concat;;

let rec iter f l = match l with 
	| [] -> ()
	| a::l -> f a; iter f l
;;

let iteri n f l = 
	if n < 0
		then raise (Failure "invalid argument")
	else
		iter f (nth_list n l) 
;;

let rec map f l = 
	match l with
		| [] -> []
		| a::l -> let r = f a in r::(map f l)
;;

(*  Can use function keyword to define an anonymous function
	which takes in an argument for pattern-matching. No match
	keyword required.
*)

let rec mapi i f = function
	| [] -> []
	| a::l -> let r = f i a in r :: mapi (i + 1) f l
;;

let rec rev_map f l r = match l with
	| [] -> r
	| a::l -> rev_map f l ((f a)::r)
;;

let rev_map f = rev_map f l [];;

let rec fold_left f l acc = match l with 
	| [] -> acc
	| a::l -> fold_left f (f acc a) l
;;

let rec fold_right f l acc  = match l with 
	| [] -> acc
	| a::l -> f a (fold_right f acc l)
;;

let rec iter2 f l1 l2 = match l1,l2 with
	| ([],[]) -> ()
	| (a1::l1,a2::l2) -> f a1 a2 ; iter2 f l1 l2
	| (_,_) -> raise (Failure "List.Invalid_argument")
;;

let rev_map2 f l1 l2 = 
	let rec rrev_map2 l1 l2 acc = 
		match l1,l2 with
			| ([],[]) -> acc
			| (a1::l1,a2::l2) -> 
				let r = f a1 a2 in rrev_map2 l1 l2 (r::acc)
			| (_,_) -> raise (Failure "List.Invalid_argument")
	in rrev_map2 l1 l2 []
;;	 
	
let rec map2 f l1 l2 = match l1,l2 with 
	| ([],[]) -> []
	| (a1::l1,a2::l2) -> let r = f a1 a2 in r::(map2 f l1 l2)
	| (_,_) -> raise (Failure "List.Invalid_argument")
;;

let rec for_all p = function 
	| [] -> true
	| a::l -> p a && for_all p l
;;

let rec exists p = function 
	| [] -> false
	| a::l -> p a || exists p l
;;

let rec for_all2 p l1 l2 = match l1,l2 with 
	| ([],[]) -> true
	| (a1::l1,a2::l2) -> (p a1 a2) && for_all2 p l1 l2
	| (_,_) -> raise (Failure "List.Invalid_argument")
;;

let rec exists2 p l1 l2 = match l1,l2 with 
	| ([],[]) -> true
	| (a1::l1,a2::l2) -> (p a1 a2) || exists2 p l1 l2
	| (_,_) -> raise (Failure "List.Invalid_argument")
;;

let mem a l = let p = fun a' -> a' = a in exists p l;;

let rec memq a  = function
	  [] -> false
	| x::l -> a = x || memq a l 
;;

let rec find p = function
	  [] -> raise (Failure "List.Not_found")
	| a::l -> if p a then a else find p l 
;;

let rec filter p = function 
	  [] -> []
	| a::l -> if p a then a::(filter p l) else filter p l
;;

let find_all = filter;;  

let partition p l = 
	let rec partition2 (yes,no) = function 
	  [] -> (rev yes, rev no)
	| a::l -> if p a then partition2 (a::yes,no) l 
			  else partition2 (yes,a::no) l
	in partition2 ([],[]) l
;; 

let rec association x = function
	  [] -> raise (Failure "List.Not_found")
	| (a,b)::l -> if compare x a = 0 then b else association x l
;;

let rec assq x = function
	  [] -> raise (Failure "List.Not_found")
	| (a,b)::l -> if x=a then b else assq x l
;;

let mem_assoc x l = exists (fun (a,b) -> compare x a = 0) l;;

let mem_assq x l = exists (fun (a,b) -> compare x = a) l;;

let rec remove_assoc x = function
	  [] -> []
	| (a,b as pair)::l -> if (compare x a = 0) then l 
						  else pair::remove_assoc x l
;;

let rec remove_assq x = function
	  [] -> []
	| (a,b as pair)::l -> if a=x then l 
						  else pair::remove_assq x l
;;

let rec split = function 
	  [] -> ([],[])
	| (a,b)::l -> let (l1,l2) = split l in a::l1,b::l2
;;

let rec combine = function
	  [],[] -> []
	| (a::l1,b::l2) -> (a,b)::(combine (l1,l2))
	| _,_ -> raise (Failure "List.Invalid_argument")
;;

let rec merge cmp l1 l2 = match l1,l2 with
	| [],l2 -> l2
	| l1,[] -> l1
	| a::t1,b::t2 -> if cmp a b <= 0 then  
					 a::merge cmp t1 l2
					 else b::merge cmp l1 t2 
;;
















	



















