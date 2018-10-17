


(* combining function *)


fun compose (f, g) = fn x => f ( g x )



infix />
fun x /> f = f x

fun sqrt_of_abs i = i /> abs /> Real.fromInt /> Math.sqrt
		      
fun backup1 (f, g) = fn x => case f x of
				 NONE => g x 
			       | SOME y => y

fun backup2 (f, g) = fn x => f x handle _ => g x 

(* Currying = Folding *)
				
val sorted3 = fn x => fn y => fn z => z>=y andalso y >= x

val t2 = (((sorted3 7) 9) 11)

(* Nicer currying *)	     
fun sorted3_nicer x y z = z >= y andalso y >= x

val t4 = sorted3_nicer 7 9 11


val x = ref 42
val y = ref 42
val z = x
val _ = x := 43
val w = (!y) + (!x)


(*  Callbacks *)
		   
(* ADT *)

datatype set = S of { insert : int -> set,
		      member: int -> bool,
		      size : unit -> int }
(* val empty_set : set *)

val empty_set =
    let
	fun make_set xs =
	    let
		fun contains i = List.exists (fn j => i=j) xs
	    in
		S {
		    insert = fn i => if contains i
				     then make_set xs
				     else make_set (i::xs),
		    member = contains,
		    size = fn () => length xs
		}
	    end
    in
	make_set []
    end
		  
			
fun use_sets () =
    let val S s1 = empty_set
	val S s2 = (#insert s1) 34
	val S s3 = (#insert s2) 3
	val S s4 = #insert s3 19
    in
	if (#member s4) 42
	then 99
	else if (#member s4) 19
	then 17 + (#size s3) ()
	else 0
    end

