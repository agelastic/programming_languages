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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals xs = 
    List.filter (fn x=> Char.isUpper (String.sub(x,0)) ) xs

fun longest_string1 xs = 
    List.foldl (fn (x,y) => if String.size x > String.size y then x else y) "" xs

fun longest_string2 xs = 
    List.foldl (fn (x,y) => if String.size x >= String.size y then x else y) "" xs

fun longest_string_helper f xs = 
    List.foldl (fn (x,y) => if f(String.size x,String.size y) then x else y) "" xs

val longest_string3 = longest_string_helper (op >)

val longest_string4 = longest_string_helper (op >=)

val longest_capitalized = longest_string_helper (op >) o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f xs = 
    case xs of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      SOME v => v
		    | NONE => first_answer f xs' 

fun all_answers f xs = 
    let fun aux f xs acc = 
	    case xs of
		[] => SOME acc
	      | x::xs' => case f x of
			      SOME v => aux f xs' (acc @ v)
			    | NONE => NONE
    in aux f xs []
    end

fun count_wildcards p = 
    g (fn () => 1) (fn s => 0) p 

fun count_wild_and_variable_lengths p = 
    g (fn () => 1) String.size p 

fun count_some_var (s, p) = 
    g (fn () => 0) (fn x => if x = s then 1 else 0) p

fun check_pat pat = 
    let fun var_names pat = 
	    case pat of
	        Wildcard          => []
	      | Variable x        => [x]
	      | TupleP ps         => List.concat(List.map var_names ps)
	      | ConstructorP(_,p) => var_names p
	      | _                 => []
    in let fun no_dupes xs = 
	       case xs of
		   [] => true
		 | x::xs' => not (List.exists (fn y:string => y = x) xs') 
			     (* added a type specification to stop "calling polyEqual" warning *)
			     andalso no_dupes xs'
       in
	   no_dupes (var_names pat)
       end
    end

fun match (value,pattern) = 
    case (value,pattern) of
	(_,Wildcard) => SOME []
      | (_,Variable x) => SOME [(x,value)]
      | (Unit, UnitP) => SOME []
      | (Const i, ConstP j) => if i=j then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if length vs = length ps 
				 then all_answers match (ListPair.zip(vs,ps))
				 else NONE 
      | (Constructor(s1,v),ConstructorP(s2,p)) => if s1=s2 
						  then match (v,p) 
						  else NONE
      | (_,_) => NONE   

fun first_match value patterns = 
    SOME (first_answer (fn x => match(value,x)) patterns) handle NoAnswer => NONE

(* challenge problem *)
(*
datatype typ = Anything (* 'a *)
             | UnitT (* () *)
             | IntT (* Int *)
             | TupleT of typ list (* (A, B, ..., ?) *)
             | Datatype of string (* Data *)
                               
fun typecheck_patterns(ctv, ps) =
    let
        fun match(tp) =
            case tp of
                (_, Wildcard) => SOME [Anything]
              | (_, Variable _)  => SOME [Anything]
              | (UnitT, UnitP) => SOME [Anything]
              | (IntT, ConstP i)  => SOME [Anything]
              | (TupleT ts, TupleP ps) => if List.length(ts) = List.length(ps)
                                          then case all_answers match (ListPair.zip(ts, ps)) of
                                                   NONE => NONE
                                                 | SOME ts' => SOME [TupleT(ts')]
                                          else NONE
              | _ => NONE
                         
        fun get_lenient_match(t, acc, ps) =
            case ps of
                [] => acc
              | p::ps' => case match(t, p) of
                              NONE => NONE
                            | SOME (t'::ts) => get_lenient_match(t, SOME t', ps')
                                                                
        fun get_all_matches(ctv, acc, ps) =
            case ctv of
                [] => acc
              | (_, _, t)::ctv' => case get_lenient_match(t, NONE, ps) of
                                       NONE => NONE
                                     | t' => case get_all_matches(ctv', t', ps) of
                                                 NONE => NONE
                                               | t'' =>  t''
    in
        get_all_matches(ctv, NONE, ps)
    end
*)
