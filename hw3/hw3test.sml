(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw3.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test1b = only_capitals ["A","bdssa","CADASD"] = ["A","CADASD"]

val test1c = only_capitals ["A","bdssa","C"] = ["A","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"

val test2b = longest_string1 ["A","bc","de","C"] = "bc"

val test2c = longest_string1 ["A","bc","deassea","de","C"] = "deassea"

val test3 = longest_string2 ["A","bc","C"] = "bc"

val test3b = longest_string2 ["A","bc","de","C"] = "de"

val test4a= longest_string3 ["A","bc","C"] = "bc"

val test4b= longest_string4 ["A","B","C"] = "C"

val test4c = longest_string3 ["A","bc","de","C"] = "bc"

val test4d = longest_string4 ["A","bc","de","C"] = "de"

val test5 = longest_capitalized ["A","bc","C"] = "A";

val test5b = longest_capitalized ["A","bc","Cd"] = "Cd"

val test6 = rev_string "abc" = "cba";

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test8b = all_answers (fn x => if x <10 then SOME [2*x] else NONE) [2,3,4,5,6,7] = SOME [4,6,8,10,12,14]

val test9a = count_wildcards Wildcard = 1

val test9aa = count_wildcards (TupleP [Wildcard, Variable("asd"), Wildcard]) = 2

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9ba = count_wild_and_variable_lengths (TupleP [Wildcard, Variable("asd"), Wildcard]) = 5

val test9ca = count_some_var ("asd", TupleP[Wildcard, Variable("asd"), Wildcard]) = 1;

val test9c = count_some_var ("x", Variable("x")) = 1;

val test10 = check_pat (Variable("x")) = true

val test10a = check_pat (TupleP([Wildcard, Variable("asd"), Variable("dsa")])) = true

val test10b = check_pat (TupleP([Wildcard, Variable("asd"), Variable("asd")])) = false

val test11 = match (Const(1), UnitP) = NONE

val test11a = match (Const(1), ConstP(1)) = SOME []

val test11b = match (Const(123), Variable("asd")) = SOME [("asd",Const 123)]

val test11c = match (Tuple([Unit, Const(321), Constructor("qwe", Unit)]),TupleP([Wildcard, Variable("asd"), Variable("zxc")])) = SOME [("asd",Const 321),("zxc",Constructor ("qwe",Unit))]


val test12 = first_match Unit [UnitP] = SOME []

val test12a = first_match (Tuple([Unit, Const(321), Constructor("qwe", Unit)])) [ConstP 17, TupleP([Wildcard, Variable("asd"), Variable("zxc")])] = SOME [("asd",Const 321),("zxc",Constructor ("qwe",Unit))]

val test12b = first_match (Tuple([Const(321), Constructor("qwe", Unit)])) [ConstP 17, TupleP([Wildcard, Variable("asd"), Variable("zxc")])] = NONE