(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** 

1. only_capitals : string list -> string list 

only start with an uppercase letter.
use List.filter, Char.isUpper, String.sub

 ****)	
fun only_capitals strings =
    List.filter (fn s => Char.isUpper(String.sub(s,0)) )  strings
		
(**** 

2. longest_string1 : string list -> string

returns the longest string in the list
return "" when empty
in the ase of a tile, return the string closest to the beginning
use foldl, String.size

****)

fun longest_string1 strings =
    List.foldl (fn (x,y) => if String.size(x) > String.size(y) then x else y) "" strings;

(****

3.  longest_string2 : string list -> string

returns the longest string in the list
return "" when empty
in the ase of a tile, return the string closest to the end of the list
use foldl, String.size
****)

fun longest_string2 strings =
    List.foldl (fn (x,y) => if String.size(x) >= String.size(y) then x else y) "" strings;


(****

4. longest_string3 same behavior as longest_string1
   longest_string4 same behavior as longest_string2
   longest_string_helper = (int * int -> bool) -> string list -> string
   longest_string_helper is passed a function that behaves like >
   longest_string3, longest_string4 with val-bindings and partial applications of longest_string_helper

****)

fun longest_string_helper f =
    List.foldl (fn (x, y) => if f(x,y) then x else y) "" ;

val longest_string3 = longest_string_helper (fn (x,y) => String.size(x) > String.size(y)) ;
val longest_string4 = longest_string_helper (fn (x,y) => String.size(x) >= String.size(y)) ;

(****

5. longest_capitalized = string list -> string / val binding
****)

val longest_capitalized  =
    longest_string1 o only_capitals;


(****

6. rev_string = string -> string

use o operator
use rev and two String module's functions.

****)

val rev_string  =
    String.implode o rev o String.explode;

(****

7. first_answer = ('a -> 'b option) -> 'a list -> 'b

****)

fun first_answer pred lists =
    case lists of
	[] => raise NoAnswer
      | l::ls => ( case pred l  of
		     SOME v => v
		    | NONE => first_answer pred ls );
		 

(****

8. all_answer = ('a -> 'b option) -> 'a list -> 'b list option

****)



fun all_answers pred lists =
    let
	fun aux acc p ls =
	    case ls of
		[] =>  acc
	     |  l::ls' => ( case p l of
				SOME v => aux (acc @ v) p ls'
			      | NONE => raise NoAnswer )
    in
	SOME (aux [] pred lists)
	handle NoAnswer => NONE
    end;

(****

9. (a) count_wildcards 

****)


val count_wildcards = g (fn _ => 1) (fn _ => 0);

(**** 

9. (b) count_wild_and_variable_lengths

****)

val count_wild_and_variable_lengths = g (fn _ => 1) (fn s => String.size s);

(****

9. (c) count_some_var = ( string * pattern  ) -> int

****)

fun count_some_var (str, ptrn) = g (fn _ => 0) (fn s => if s = str then 1 else 0 ) ptrn;


(****

10. check_pat pattern -> bool

****)

fun check_pat patterns =
    let
	fun variables ptrn =
	    case ptrn of
		Wildcard          => []
	      | Variable x        => [x]
	      | TupleP ps         => List.foldl (fn (q,i) => (variables q) @ i) [] ps
	      | ConstructorP(_,p) => variables p
	      | _                 => []
	fun is_repeats strings =
	    case strings of
		[] => false
	       | s::ss => (List.exists (fn t => s = t) ss) orelse (is_repeats ss)
    in
	not ( is_repeats ( variables patterns) )
    end;
			


(****

11. match (valu * pattern) -> (string * valu) list option

NONE     : if the pattern does not match
SOME lst : lst is the list of bindings if it does
****)

fun match (v , p) =
    case (v,p) of
	(Const(i), ConstP(j)) => if i=j then SOME [] else NONE
      | (Const(i), Variable(s)) => SOME [(s, Const(i))]
      | (Unit, UnitP) => SOME []
      | (Tuple(vs), TupleP(ps)) =>
	(all_answers (fn (v1, p1) => match(v1, p1)) (ListPair.zip(vs, ps) ))
      | (Constructor(s, vs), ConstructorP(t, ps)) => (if s=t then match(vs, ps) else NONE)
      | (_, Wildcard) => SOME []
      | _ => NONE
;


(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

			       (**** you can put all your code here ****)
