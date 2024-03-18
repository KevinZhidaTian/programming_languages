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

fun only_capitals(inputList: string list) = 
	List.filter(fn str => Char.isUpper(String.sub(str, 0))) inputList

fun longest_string1(inputList: string list) = 
	case inputList of
	[] => ""
	| x::xs' => List.foldl(fn (str, longest) => if String.size str > String.size longest then str else longest) x xs'

fun longest_string2(inputList: string list) = 
	case inputList of
	[] => ""
	| x::xs' => List.foldl(fn (str, longest) => if String.size str >= String.size longest then str else longest) x xs'

fun longest_string_helper cmp stringlist =
	case stringlist of
	[] => ""
	| x::xs' => List.foldl(fn (str, longest) => if cmp(String.size str, String.size longest) then str else longest) x xs'

val longest_string3 = longest_string_helper (fn (x, y)=>x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer function inputList = 
	case inputList of
		[] => raise NoAnswer
		| x::xs' => case function x of
						NONE => first_answer function xs'
						| SOME y => y
