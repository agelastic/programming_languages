
(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (str,lst) =
    case (str, lst) of
	(str,[]) => NONE
      | (str, h::t) => if same_string (h, str)
		       then SOME t
		       else case all_except_option (str, t) of
				NONE => NONE
			      | SOME s => SOME (h::s)
  

fun get_substitutions1 (lst, str) = 
    case lst of
	[] => []
      | h::t => case all_except_option(str, h) of 
		    NONE => get_substitutions1 (t, str)
		 |  SOME subs => subs @ get_substitutions1 (t, str) 

fun get_substitutions2 (lst, str) = 
    let fun aux(lst, str, acc) = 
	    case lst of
		[] => acc
	      | h::t => case all_except_option (str, h) of 
			    NONE => aux (t, str, acc)
			  | SOME s => aux (t, str, acc @ s)
    in
	aux (lst, str, [])
    end

fun similar_names (subs, full_name) = 
    let fun make_names (first_names, middle, last, acc) = 
	    case first_names of
		[] => acc
	      | h::t => make_names(t, middle, last, {first=h, middle=middle, last=last} :: acc)
    in
	case full_name of
	    {first, middle,last} => make_names(get_substitutions1(subs, first) @ [first], middle, last, [])
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

fun card_color (card) = 
    case card of
	(Spades,_) => Black
      | (Clubs,_) => Black
      | (_,_) => Red

fun card_value (card) = 
    case card of
	(_, Ace) => 11
      | (_, Num i) => i
      | (_, _) => 10

fun remove_card (cs, c, e) = 
    case cs of
	[] => raise e
      | h::t => if h=c then t 
		else h::remove_card (t, c,e)

fun all_same_color (cs) = 
    case cs of
	[] => true
      | x::[] => true
      | x::y::z => card_color x = card_color y andalso all_same_color(y::z)

fun sum_cards (cs) = 
    let fun aux(cs, acc) = 
	case cs of
	    [] => acc
	  | h::t => aux (t, card_value h + acc)
    in
	aux(cs,0)
    end

fun score (cs, goal) = 
    let fun final_score (s) = 
	    if all_same_color(cs)
	    then s div 2
	    else s
    in
	let val sum = sum_cards cs in
	    if sum > goal
	    then final_score (3 * (sum - goal))
	    else final_score (goal - sum)
	end
    end

fun officiate (cs, moves, goal) = 
    let fun round (args) = 
	    case args of
		(_, [], goal, held) => score(held, goal)
	      | (cs, Discard c::moves', goal, held) => round (cs, moves', goal, remove_card(held, c, IllegalMove))
	      | ([], Draw::moves', goal, held) => score (held, goal)
	      | (h::t, Draw::moves', goal, held) => if card_value h + sum_cards held >goal
						    then score(h::held, goal)
						    else round(t, moves', goal, h::held)
    in 
	round(cs, moves, goal, [])
    end
