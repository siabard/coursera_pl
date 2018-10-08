(*

Tuples : number of pieces have different types
Lists : number of pieces have same type

Tutples

Syntax: (e1, e2)
Evaluation: Evaluate e1 to v1, and e2 to v2: result is (v1, v2)
Type-Checking: e1 has type ta and e2 has type tb, the pair expression has type ta * tb

access
Syntax: #1 e  
        #2 e


*)

fun swap (pr:int * bool) =
    (#2 pr, #1 pr);

(* (int * int) * (int * int) -> int  *)
fun sum_two_pairs (pr1: int * int, pr2: int * int) =
    (#1 pr1) + (#2 pr2) + (#1 pr2) + (#2 pr2);

	
fun div_mod (x: int, y: int) =
    (x div y, x mod y);

fun sort_pair (pr: int * int ) =
    if (#1 pr) < (#2 pr)
    then pr
    else (#2 pr, #1 pr);

(* Nesting *)
(*

val x1 = (7, (true, 9)) (* int * (bool * int) *)
val x2 = #1 (#2 x1) (* bool *)
val x3 = (#2 x1) (* bool * int *)
val x4 = ((3,5), ((4,8), (0,0))) (* (int * int) * ((int*int) * (int*int)) *)
*)
