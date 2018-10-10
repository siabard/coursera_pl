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

fun card_color (s: suit, r: rank) =
    case s of
	Clubs =>  Black
      | Spades => Black	      
      | Hearts => Red
      | Diamonds => Red
			
				     
			    
fun card_value (s:suit, r:rank) =
    case r of
	Ace => 11
      | Num i => i
      | _ => 10

fun remove_card (cs: card list, c: card, exn) =
    case cs of
	c'::cs' =>
	if c' = c
	then cs'
	else c'::(remove_card ( cs', c, exn))
      | _ => raise exn

fun all_same_color (cards : card list ) =
    case cards of
	[] => true
      | c::cs =>
	let
	    val const_color = card_color c
	    fun has_same_color (cards : card list, clr: color) =
		case cards of
		    [] => true
		  | c' :: cs' =>
		    if card_color(c') = clr
		    then has_same_color( cs', clr)
		    else false
	in
	    has_same_color( cs, const_color )
	end
			   
			   
fun sum_cards (cards: card list) =
    let
	fun aux(acc : int, cards: card list) =
	    case cards of
		[] => acc
	     | c::cs => aux( acc + card_value(c), cs)
    in
	aux(0, cards)
    end

fun score (held_cards: card list, goal : int ) =
    let
	val sum = sum_cards (held_cards)
	val preliminary_score =
	    if sum > goal
	    then (sum - goal) * 3
	    else goal - sum
    in
	if all_same_color(held_cards)
	then preliminary_score div 2
	else preliminary_score
    end
	    
	    
				
fun officiate (card_list: card list, does : move list, goal: int ) =
    let
	fun game_helper( held_cards: card list,  cards : card list, dolist : move list ) =
	    case dolist of
		[] => held_cards
	      | d::ds =>
		case d of
		    Discard c => game_helper( remove_card( held_cards, c, IllegalMove), cards, ds)
		  | Draw => case card_list of
				[] => held_cards
			      | c::cs =>
				let
				    val drawed_held_cards = c::held_cards
				in
				    if sum_cards(drawed_held_cards) > goal
				    then drawed_held_cards
				    else game_helper( drawed_held_cards, cs ,ds)
				end
    in
	score(game_helper([], card_list, does), goal)	      
    end
