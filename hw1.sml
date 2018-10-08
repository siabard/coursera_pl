(*
is_older
takes two dates and evaluates to true or false.  
It evaluates to true if the first argument is a date that comes before the second argument.
If the two dates are the same, the result is false.
*)

fun is_older ( date1: (int*int*int), date2: (int*int*int)) =
    if #1 date1 < #1 date2
    then true
    else if #1 date1 > #1 date2
    then false
    else if #2 date1 < #2 date2
    then true
    else if #2 date1 > #2 date2
    then false
    else if #3 date1 < #3 date2
    then true
    else false;




(*
number_in_month
that takes a list of dates and a month (i.e., an int) and returns
how many dates in the list are in the given month.
*)



fun number_in_month (dates: (int*int*int) list, month: int ) =
    if null dates
    then 0
    else
	let
	    val current = hd dates;
	    val given_month =
		if #2 current = month
		then 1
		else 0;
	in
	    given_month + number_in_month(tl dates, month)
	end;

(*
number_in_months
that takes a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that are in any of the months in the list of months.
*)

fun number_in_months (dates: (int * int * int) list, months: int list) =
    if null months
    then 0
    else
	let
	    val occurence = number_in_month(dates, (hd months))
	in
	    occurence + number_in_months( dates, (tl months))
	end;


(*
dates_in_month
that takes a list of dates and a month (i.e., an int ) and returns a
list holding the dates from the argument list of dates that are in the month.  The returned list should contain dates in the order they were originally given.
*)

fun dates_in_month (dates: (int * int * int) list, month : int ) =
    if null dates
    then []
    else
	let
	    val current = hd dates;
	    val aux = dates_in_month(tl dates, month);
	in
	    if #2 current  = month
	    then
		current :: aux
	    else
		aux
	end;


(*
dates_in_months
that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in the list of months.
Assume the list of months has no number repeated.
*)

fun dates_in_months  (dates: (int * int * int) list, months : int list ) =
    if null months
    then []
    else
	let
	    val occurences = dates_in_month( dates, (hd months))
	in
	    occurences @ dates_in_months( dates, (tl months))
	end;


(*
get_nth
that takes a list of strings and an int n and returns the n th element of the
list where the head of the list is 1st.
*)
fun get_nth (lst : 'a list, pos : int) =
    if pos = 1
    then hd lst
    else get_nth (tl lst, pos-1);


(*
date_to_string
that takes a date and returns a string of the form January 20, 2013 (for example).
Use the operator ^ for concatenating strings and the library function Int.toString
for converting an int to a string.  
For producing the month part, do not use a bunch of conditionals.
Instead, use a list holding 12 strings and your answer to the previous problem.  
For consistency, put a comma following the day and use capitalized English month names:  
January, February, March, April, May, June, July, August, September, October, November, December.
*)


fun date_to_string (date: int * int * int ) =
    let
	val month = get_nth(["January",
			     "February",
			     "March",
			     "April",
			     "May",
			     "June",
			     "July",
			     "August",
			     "September",
			     "October",
			     "November",
			     "December"], #2 date);
	val year = #1 date;
	val day = #3 date;
    in
	month ^ " " ^ Int.toString(day) ^ ", " ^ Int.toString(year)
    end;

(*
number_before_reaching_sum
that takes an int called sum, which you can assume is positive,
and an int list, which you can assume contains all positive numbers, 
and returns an int.
You should return an int n such that the first n elements of the list 
add to less than sum, but the first n + 1 elements of the list add to
sum or more.  Assume the entire list sums to more than the passed in
value; it is okay for an exception to occur if this is not the case
*)

fun number_before_reaching_sum (sum: int, lst: int list)  =
    let
	fun aux(pos: int, part_sum: int, sum: int, lst: int list)  =
	    if part_sum >= sum
	    then pos - 1
	    else if null lst
	    then 0
	    else aux(pos+1, part_sum + (hd lst), sum, (tl lst));
    in
	aux(0, 0, sum, lst)
    end;

(*
what_month
that takes a day of year (i.e., an int between 1 and 365) and returns
what month that day is in (1 for January, 2 for February, etc.).  
Use a list holding 12 integers and your answer to the previous problem
*)

fun what_month (day_of_year: int) =
    let
	val result = number_before_reaching_sum( day_of_year, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]);
    in
	result + 1
    end;

(*
month_range
that takes two days of the year day1 and day2 and returns an int list [m1,m2,...,mn]
where m1 is the month of day1 ,m2 is the month of day1+1 , ..., and mn is the month
of day day2 

*)

fun month_range ( day1: int, day2: int) =
    if day1 <= day2
    then what_month(day1) :: month_range(day1 + 1, day2)
    else [];


(*
oldest
that  takes  a  list  of  dates  and  evaluates  to  an (int*int*int) option.
It evaluates to
NONE if the list has no dates and
SOME d if the date d is the oldest date in the list.
*)

fun oldest ( dates: (int * int * int) list) =
    if null dates
    then NONE
    else
	let
	    val max_date = hd dates;
	    fun aux ( maxi: (int * int * int), remains: (int * int * int) list) =
		if null remains
		then SOME maxi
		else
		    let
			val next = hd remains;
			val tl_remains = tl remains;
		    in
			if is_older(maxi, next)
			then aux(maxi, tl_remains)
			else aux(next, tl_remains)
		    end;
	in
	    aux(max_date, (tl dates))
	end;


(*
Challenge Problem:

number_in_months_challenge
and
dates_in_months_challenge
that are like your solutions to problems 3 and 5 except having a month in the second argument multiple times has no more effect than having it once. 
*)

fun number_in_months_challenge  (dates: (int * int * int) list, months: int list) =
    let
	fun in_list (item: int, lst : int list) =
	    if null lst
	    then false
	    else (item = (hd lst)) orelse in_list(item, tl lst);
	fun remove_dup (src: int list, tgt: int list) =
	    if null src
	    then tgt
	    else
		let
		    val item = hd src;
		    val remain = tl src;
		in
		    if in_list(item, tgt)
		    then remove_dup(remain, tgt)
		    else remove_dup(remain, item::tgt)
		end;
    in
	number_in_months(dates , remove_dup (months, []) )
    end;


fun dates_in_months_challenge  (dates: (int * int * int) list, months: int list) =
    let
	fun in_list (item: int, lst : int list) =
	    if null lst
	    then false
	    else (item = (hd lst)) orelse in_list(item, tl lst);
	fun remove_dup (src: int list, tgt: int list) =
	    if null src
	    then tgt
	    else
		let
		    val item = hd src;
		    val remain = tl src;
		in
		    if in_list(item, tgt)
		    then remove_dup(remain, tgt)
		    else remove_dup(remain, item::tgt)
		end;
    in
	dates_in_months(dates , remove_dup (months, []) )
    end;


(*
reasonable_date
that  takes  a  date  and  determines  if  it describes a real date in the common era.  
A “real date” has a positive year (year 0 did not exist), a month between 1 and 12, 
and a day appropriate for the month.  
Solutions should properly handle leap
years.  
Leap years are years that are either divisible by 400 or divisible by 4 but not divisible by 100. (Do not worry about days possibly lost in the conversion to the Gregorian calendar in the Late 1500s
*)

fun reasonable_date (date: (int * int * int)) =
    let
	val year = #1 date;
	val month = #2 date;
	val day = #3 date;
	val leap_year =
	    not(year mod 4 <> 0 orelse ( year mod 100 = 0 andalso year mod 400 <> 0));
	val days_in_month =
	    if leap_year = true
	    then [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	    else [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
		
    in
	year > 0
	andalso month >= 1
	andalso month <= 12
	andalso day >= 1
	andalso day <= get_nth( days_in_month, month)
    end;
	
