(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw1.sml";

val test1a = is_older((1,2,3),(2,3,4)) = true
val test1b = is_older((0,0,0),(3,4,5)) = true
val test1c = is_older((3,4,5),(0,0,0)) = false

val test2a = number_in_month([(2012,2,28),(2013,12,1)],2) = 1

val test3a = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4a = dates_in_month([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5a = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6a = get_nth(["hi", "there", "how", "are", "you"], 2) = "there"
val test6b = get_nth(["hi", "there", "how", "are", "you"], 1) = "hi"
val test6c = get_nth(["hi", "there", "how", "are", "you"], 5) = "you"

val test7a = date_to_string((2013, 6, 1)) = "June 1, 2013"
val test7b = date_to_string((2012, 9, 32)) = "September 32, 2012"

val test8a = number_before_reaching_sum(10, [1,2,3,4,5]) = 3
val test8b = number_before_reaching_sum(10, [1,2,10,7,5]) = 2
val test8c = number_before_reaching_sum(10, [1,2,3,7,5]) = 3

val test9a = what_month(70) = 3
val test9b = what_month(284) = 10 

val test10a = month_range(31, 34) = [1,2,2,2]
val test10b = month_range(58, 62) = [2,2,3,3,3]

val test11a = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31) 
val test11b = oldest([]) = NONE

val test12a = number_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,2,3,3,4]) = 3
val test12b = dates_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,3,3,4,2]) = [(2011,3,31),(2011,4,28),(2012,2,28)]

val test13a = reasonable_date (0,1,1) = false
val test13b = reasonable_date (1,1,30) = true
val test13c = reasonable_date (1,1,32) = false
val test13d = reasonable_date (2000,2,29) = true
val test13e = reasonable_date (2100,2,29) = false
val test13f = reasonable_date (2100,2,28) = true
