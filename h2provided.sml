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
		[] => if found = true then SOME ( rev (initial)) else NONE
	      | l::ls => if same_string (s, l)
			then aux(initial, true, s, ls)
			else aux(l::initial, found, s, ls)
    in
	aux([], false, needle, haystack)
    end
					
fun get_substitutions1 (src: string list list, s : string) =
    case src of
	[] => []
      | str_list::others =>
	let
	    val except_option = all_except_option (s, str_list)
	    val sub_aux = get_substitutions1 (others, s)
	in
	    case except_option of
		NONE => sub_aux
	      | SOME r => r @ sub_aux
	end

fun get_substitutions2 (src: string list list, s : string ) =
    let
	fun aux(acc: string list, haystack: string list list, needle: string) =
	    case haystack of
		[] => acc
	      | h::hs =>
		let
		    val except_option = all_except_option (needle, h)
		in
		    case except_option of
			NONE => aux(acc, hs, needle)
		      | SOME r => aux( acc @ r, hs, needle)
		end
    in
	aux([], src, s)
    end

type fullname = {first: string, middle: string, last: string}

fun similar_names (subs: string list list, {first, middle, last} : fullname) =
    let
	val get_substitutions = get_substitutions2 (subs, first)
	fun helper (items : string list) =
	    case items of
		[] => []
	      | item::remains => {first=item, middle=middle, last=last} :: helper(remains) 
    in
	{first = first, middle = middle, last = last}::helper(get_substitutions)
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
