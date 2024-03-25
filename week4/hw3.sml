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

fun all_answers function inputList =
	let
	  fun aux lst accumulator =
			case lst of
			[] => SOME accumulator
			|x::xs' => case function x of
						NONE => NONE
						| SOME y => aux xs' (accumulator @ y)
	in
	  aux inputList []
	end

fun count_wildcards p =
	g (fn () => 1) (fn s => 0) p

fun count_wild_and_variable_lengths p =
	g (fn () => 1) String.size p

fun count_some_var (s, p) =
	g (fn ()=>0) (fn str => if s = str then 1 else 0) p
	
fun check_pat p =
	let
		fun get_name_list pattern = 
			case pattern of
			Wildcard => []
			| Variable x => [x]
			| TupleP ps => List.foldl(fn (p, i)=> (get_name_list p)@ i) [] ps
			| ConstructorP(_,ptn) => get_name_list ptn
			| _ => []

		fun compare (x, y) = case String.compare(x, y) of
						EQUAL => true
						| _ => false

		fun if_unique strList =
			case strList of
				[] => true
				| x::xs' => 
				let
				  val is_existed = List.exists(fn y => compare(x, y)) xs'
				in
				  if is_existed then false else if_unique xs'
				end

		val compose = if_unique o get_name_list
	in
		compose p
	end

fun match (v:valu, ps: pattern) =
	case (v, ps) of
	(_, Wildcard) => SOME []
	|(sv, Variable str) => SOME [(str, sv)]
	|(Unit, UnitP) => SOME []
	|(Const iv, ConstP ip) => if iv = ip then SOME[] else NONE
	|(Tuple vl, TupleP  pl) => if List.length vl = List.length pl
								then all_answers match(ListPair.zip(vl, pl))
								else NONE
	|(Constructor (s1, v1), ConstructorP (s2, p2)) => if s1 = s2
													then match (v1, p2)
													else NONE
	|(_, _) => NONE

fun first_match (value, patternList: pattern list) =
	SOME(first_answer (fn x=> match(value, x)) patternList) handle
	  NoAnswer => NONE