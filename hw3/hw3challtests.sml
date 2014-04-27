use "hw3.sml";

val wc = Wildcard
val x = Variable "x"
val y = Variable "y"
val z = Variable "z"

fun tpl vs = Tuple vs
fun pair (a,b) = Tuple [a,b]
fun tp ps = TupleP ps
fun pp (a,b) = TupleP [a,b]

val u = Unit
val up = UnitP

(* Polymorphic lists in the spirit of Java 2 *)
val nla = Constructor ("NilA",u)
fun cnsa h t = Constructor ("ConsA",pair (h, t))
val nlpa = ConstructorP ("NilA",up)
fun cnspa h t = ConstructorP ("ConsA",pp (h, t))

val lista_cstrs = [("NilA","ListA",UnitT),
  	   ("ConsA","ListA",TupleT [Anything,Datatype "ListA"])]

(* lists of ints *)
val nli = Constructor ("NilI",u)
fun cnsi h t = Constructor ("ConsI",pair (h, t))
val nlpi = ConstructorP ("NilI",up)
fun cnspi h t = ConstructorP ("ConsI",pp (h, t))

val listi_cstrs = [("NilI","ListI",UnitT),
		   ("ConsI","ListI",TupleT [IntT,Datatype "ListI"])]

(* lists of lists of ints *)
val nlil = Constructor ("NilIL",u)
fun cnsil h t = Constructor ("ConsIL",pair (h, t))
val nlpil = ConstructorP ("NilIL",up)
fun cnspil h t = ConstructorP ("ConsIL",pp (h, t))

val listil_cstrs = [("NilIL","ListIL",UnitT),
		    ("ConsIL","ListIL",TupleT [Datatype "ListI",
					       Datatype "ListIL"])]

(* lists of t *)
val nlt = Constructor ("NilT",u)
fun cnst h t = Constructor ("ConsT",pair (h, t))
val nlpt = ConstructorP ("NilT",up)
fun cnspt h t = ConstructorP ("ConsT",pp (h, t))

val listt_cstrs = [("NilT","ListT",UnitT),
		   ("ConsT","ListT",TupleT [Datatype "t",Datatype "ListT"])]

(* lists of pairs (t,IntT) *)
val nlti = Constructor ("NilTI",u)
fun cnsti h t = Constructor ("ConsTI",pair (h, t))
val nlpti = ConstructorP ("NilTI",up)
fun cnspti h t = ConstructorP ("ConsTI",pp (h, t))

val listti_cstrs = [("NilTI","ListTI",UnitT),
		    ("ConsTI","ListTI",
		     TupleT [TupleT [Datatype "t",IntT],Datatype "ListTI"])]

(* simple one-of type t *)
val A = Constructor ("A",u)
fun B x = Constructor ("B",x)
fun C (x,y,z) = Constructor ("C",tpl [x,y,z])
val Ap = ConstructorP ("A",up)
fun Bp x = ConstructorP ("B",x)
fun Cp (x,y,z) = ConstructorP ("C",tp [x,y,z])

val t_cstrs = [("A","t",UnitT),
	       ("B","t",IntT),
	       ("C","t",TupleT [IntT,IntT,IntT])]

(* constructors *)
val cstrs = lista_cstrs @ listi_cstrs @ listil_cstrs @ listt_cstrs @
	    listti_cstrs @ t_cstrs
			       
(* empty *)
val test000 = typecheck_patterns (cstrs, []) = SOME Anything
(* wc,var,pair -> pair *)
val test001 = typecheck_patterns (cstrs, [wc,x,pp(y,z)]) =
	      SOME (TupleT[Anything,Anything])
(* wc,triple,pair -> fail *)
val test002 = typecheck_patterns (cstrs, [wc,tp [x,wc,wc],pp(y,z)]) = NONE
(* [], x::xs -> list *)
val test003 = typecheck_patterns (cstrs, [wc,nlpa,cnspa x y]) =
	      SOME (Datatype "ListA")
val test004 = typecheck_patterns (cstrs, [wc,nlpi,cnspi x y]) =
	      SOME (Datatype "ListI")
val test005 = typecheck_patterns (cstrs, [wc,nlpil,cnspil x y]) =
	      SOME (Datatype "ListIL")
val test006 = typecheck_patterns (cstrs, [wc,nlpt,cnspt x y]) =
	      SOME (Datatype "ListT")
val test007 = typecheck_patterns (cstrs, [wc,nlpti,cnspti x y]) =
	      SOME (Datatype "ListTI")
(* no variance: mismatched Nil & Cons -> fail *)
val test008 = typecheck_patterns (cstrs, [wc,nlpi,cnspa x y]) = NONE
(* ListA allows things ML doesn't *)
(* A::(x,y)::z -> ListA *)
val test009 = typecheck_patterns (cstrs, [cnspa Ap (cnspa (pp (x,y)) z)]) =
	      SOME (Datatype "ListA")
(* A::x::z,_::1::[] -> ListA *)
val test010 = typecheck_patterns (cstrs, [cnspa Ap (cnspa x z),
					  cnspa wc (cnspa (ConstP 1) nlpa)]) =
	      SOME (Datatype "ListA")
(* typed lists are more restrictive *)
(* A::(x,y)::z -> fail *)
val test011 = typecheck_patterns (cstrs, [cnspt Ap (cnspt (pp (x,y)) z)]) =
	      NONE
val test012 = typecheck_patterns (cstrs, [cnspt Ap (cnspti (pp (x,y)) z)]) =
	      NONE
val test013 = typecheck_patterns (cstrs, [cnspt Ap (cnspa (pp (x,y)) z)]) =
	      NONE
(* A::x::z,_::1::[] -> fail *)
val test014 = typecheck_patterns (cstrs, [cnspt Ap (cnspt x z),
					  cnspi wc (cnspi (ConstP 1) nlpi)]) =
	      NONE
(* but *)
(* (A,_)::z,_::(_,1)::[] -> ListTI *) 
val test015 = typecheck_patterns (cstrs, [cnspti (pp (Ap,wc)) z,
					  cnspti wc
						 (cnspti (pp (wc,ConstP 1))
							 nlpti)]) =
	      SOME (Datatype "ListTI")
val test016 = typecheck_patterns (cstrs, [cnspti (pp (Ap,wc)) z,
					  cnspti (pp (wc,ConstP 1)) nlpti]) =
	      SOME (Datatype "ListTI")
val test017 = typecheck_patterns (cstrs, [cnspti (pp (wc,ConstP 1)) nlpti]) =
	      SOME (Datatype "ListTI")
val test018 = typecheck_patterns (cstrs, [cnspti wc nlpti]) =
	      SOME (Datatype "ListTI")
val test019 = typecheck_patterns (cstrs, [cnspi wc nlpi]) =
	      SOME (Datatype "ListI")
val test020 = typecheck_patterns (cstrs, [cnspti wc x]) =
	      SOME (Datatype "ListTI")
(* unknown contructor -> fail *)
val test040 = typecheck_patterns (cstrs, [wc,ConstructorP ("Pizza",up)]) = NONE
(* wrong constructor agruments -> fail *)
val test041 = typecheck_patterns (cstrs, [ConstructorP ("C",TupleP [wc,wc])]) =
	      NONE
val test042 = typecheck_patterns (cstrs, [wc,
					  ConstructorP ("C",TupleP [x,wc,nlpa])]) = NONE
(* but can use constructors of the same type *)
val test043 = typecheck_patterns (cstrs, [wc, Cp(x,wc,y),Ap,Bp(x)]) =
	      SOME (Datatype "t")
				 
val test045 = typecheck_patterns (cstrs, [tp [Ap,x,y],tp [wc,wc,nlpi],
					  tp [Bp x,wc,cnspi y z]]) =
	      SOME (TupleT [Datatype "t",Anything,Datatype "ListI"])
(* incorrect pattern -> fail *)
val test050 = typecheck_patterns (cstrs, [pp(x,x)]) = NONE
(* some weird cases *)
(* we allow to match no-argument constructor with variable or _ *)
val test065 = typecheck_patterns (cstrs, [Ap,ConstructorP ("A",x)]) =
	      SOME (Datatype "t")
val test066 = typecheck_patterns (cstrs, [Ap,ConstructorP ("A",wc)]) =
	      SOME (Datatype "t")
(* we don't make TupleT[] the same as UnitT *)
val test067 = typecheck_patterns (cstrs, [up, TupleP []]) = NONE
(* as well as TupleT[x] is not the same as x *)
val test068 = typecheck_patterns (cstrs, [ConstP 1, TupleP [ConstP 1]]) =
	      NONE
