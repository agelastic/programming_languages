fun is_older (date1: int * int * int, date2: int * int * int) = 
    (#1 date1) < (#1 date2) orelse 
    ((#1 date1) =( #1 date2) andalso (#2 date1) < (#2 date2)) orelse
    ((#1 date1) =( #1 date2) andalso (#2 date1) = (#2 date2) 
     andalso (#3 date1 < #3 date2)) 

fun number_in_month (dates: (int * int * int) list, month: int) = 
    if null dates
    then 0
    else 
	if #2 (hd dates) = month
	then number_in_month (tl dates, month) + 1
	else number_in_month (tl dates, month)

fun number_in_months (dates: (int * int * int) list, months: int list) = 
    if null months
    then 0
    else number_in_month (dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month (dates: (int * int * int) list, month: int) = 
    if null dates
    then []
    else
	if #2 (hd dates) = month
	then (hd dates) :: dates_in_month (tl dates, month)
	else  dates_in_month (tl dates, month)

fun dates_in_months (dates: (int * int * int) list, months: int list) =
    if null months
    then []
    else dates_in_month (dates, hd months) @ dates_in_months (dates, tl months)

fun get_nth (strs: string list, n: int) = 
    if n <= 1
    then hd strs
    else get_nth (tl strs, n-1)

fun date_to_string (date: int * int * int ) = 
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"] in
	get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end 

fun number_before_reaching_sum (sum: int, nums: int list) = 
    if hd nums >= sum
    then 0
    else number_before_reaching_sum(sum - hd nums, tl nums) + 1

fun what_month (day: int) = 
    (* assuming 28 days in February, no leap years *)
    let val days_in_months = [31,28,31,30,31,30,31,31,30,31,30,31] in
	number_before_reaching_sum (day, days_in_months) + 1
    end

fun month_range (day1 : int, day2: int) = 
    if day1 > day2
    then []
    else what_month(day1) :: month_range (day1+1, day2)

fun oldest (dates: (int * int * int) list) =
    if null dates
    then NONE
    else
	let val partial_oldest = oldest (tl dates) in
	    if isSome partial_oldest andalso is_older(valOf partial_oldest, hd dates)
	    then partial_oldest
	    else SOME (hd dates)
	end

fun remove_dupes (x : int list) = (* helper function for the challenge question 12 *) 
    if null x
    then []
    else 
	let fun in_list (x: int, l: int list) = 
		not (null l) andalso ((x = hd l) orelse in_list(x, tl l)) in
	    if in_list(hd x, tl x)
	    then remove_dupes (tl x)
	    else hd x :: remove_dupes (tl x)
	end

fun number_in_months_challenge (dates: (int * int * int) list, months: int list) = 
    number_in_months(dates, remove_dupes(months))

fun dates_in_months_challenge (dates: (int * int * int) list, months: int list) = 
    dates_in_months(dates, remove_dupes(months))

fun nth (l: int list, x: int) = (* helper function for the challenge question 13. I could probably move it inside reasonable_date, but I think it reads better this way *)
    if x <=1 
    then hd l
    else nth (tl l, x-1)

fun reasonable_date (date: int * int * int) = 
    let val days_in_months = [31,28,31,30,31,30,31,31,30,31,30,31] in
	let fun leap_year (x : int) = (x mod 400 = 0) orelse 
				      (x mod 100 <> 0 andalso x mod 4 = 0) in
	    (#1 date >0) andalso
	    (#2 date >0 andalso #2 date <13) andalso
	    ((#3 date <= nth (days_in_months, #2 date)) orelse 
	     (#2 date = 2 andalso leap_year(#1 date) andalso #3 date = 29) orelse
	     (#2 date = 2 andalso not (leap_year(#1 date)) andalso #3 date = 28))	     
	end
    end
