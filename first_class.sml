(*
Functional Programming

1. avoid mutation in most/all cases.
2. using functions as values.
*)

(* First class functions *)

fun double x = 2*x
fun incr x = x+1
val a_tuple = (double, incr, double(incr 7))
val eighteen = (#1 a_tuple) 9;

(* Function as Arguments

We can pass one function  as an argument to another function

fun f (g, ...) = ... g (...) ...
fun h1 ... = ...
fun h2 ... = ...

... f(h1, ...) ... f(h2, ...) ...

Elegant strategy for factoring out common code.

*)

fun increment_n_times_lame (n,x) =
    if n=0
    then x
    else 1 + increment_n_times_lame(n-1, x)

fun double_n_times_lame (n, x) =
    if n=0
    then x
    else 2 * double_n_times_lame(n-1, x)

fun nth_tail_lame (n, xs) =
    if n = 0
    then xs
    else tl (nth_tail_lame(n-1, xs))
	    
fun n_times (f, n, x) =
    if n = 0
    then x
    else f(n_times(f, n-1, x))

fun increment x = x+1
fun double x = x+x
val x1 = n_times(double, 4, 7)
val x2 = n_times(increment, 4, 7)
val x3 = n_times(tl, 2, [4,8,12,16])

fun addition (n,x) = n_times(increment, n, x)
fun double_n_times (n,x) = n_times(double, n, x)
fun nth_tail (n, x) = n_times(tl, n, x)
fun triple x = 3 * x

fun triple_n_time (n, x) = n_times(triple, n, x)
				  
(*
Polymorthinc types / Function as argument

Higher order functions are often so 'generic' and 'reusable' that 
they have polymorphic types.


fun n_times (f,n,x) =
  if n = 0
  then x
  else f (n_times(f, n-1, x))

val n_times : ('a -> 'a) * int * 'a -> 'a

* type is inferred based on how arguments are used.
- Describes which types must be exactly something (e.g., int) and
  which can be anything but the same (e.g., 'a)
*)

				  

