(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (needle: string, haystack: string list) =
    let
	fun aux(initial : string list, found: bool, s: string, lst : string list) =
	    case lst of
		[] => if found = true then SOME initial else NONE
	      | l::ls => if same_string (s, l)
			then aux(initial, true, s, ls)
			else aux(l::initial, found, s, ls)
    in
	aux([], false, needle, haystack)
    end
					
	     
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
