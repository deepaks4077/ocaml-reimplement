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
    [] -> []
  | a::l -> let r = f i a in r :: mapi (i + 1) f l
;;
















