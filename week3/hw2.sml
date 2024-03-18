(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun contains(s1: string, strList: string list) =
    case strList of
        [] => false
        |x::xs' => if same_string(s1, x) then true else contains(s1, xs')

fun all_except_option_helper(s1: string, strList: string list) =
    case strList of 
        [] => []
        |x::xs' => 
            if same_string(s1, x) 
            then all_except_option_helper(s1, xs')
            else x::all_except_option_helper(s1, xs')

fun all_except_option(s1: string, strList: string list) =
    if contains(s1, strList) then SOME (all_except_option_helper(s1, strList)) else NONE

fun get_substitutions1(input: string list list, s: string) = 
    case input of
        [] => []
        | x::xs' => 
            case all_except_option(s, x) of
                NONE => get_substitutions1(xs', s)
                |SOME y =>  y @ get_substitutions1(xs', s)


fun get_substitutions2(input: string list list, s: string) = 
    let fun aux(acc: string list, ls: string list list, str: string)= 
            case ls of
                [] => acc
                |x::xs' => 
                    case all_except_option(s, x) of
                        NONE => aux(acc, xs', s)
                        |SOME y =>  aux(acc @ y, xs', s)
    in
        aux([], input, s)
    end

fun similar_names(strList: string list list, fullName: {first:string,middle:string,last:string}) = 
    let val {first=x,middle=y,last=z} = fullName
        val alternativeNames = get_substitutions2(strList, x)
    in
        let
            fun aux(acc: {first:string,middle:string,last:string} list, names: string list) = 
                case names of
                    [] => acc
                    |c::cs' => aux(acc @ [{first=c,last=z, middle=y}], cs')
        in
            aux([fullName], alternativeNames)
        end
    end
         
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color(card: card) =
    case card of
        (Spades, _) => Black
        |(Clubs, _) => Black
        | (_, _) => Red

fun card_value(card: card) = 
    case card of
        (_, Num x) => x
        |(_, Ace) => 11
        |(_, _) => 10

fun remove_card(cs: card list, c: card, e) =
    case cs of
        [] => raise e
        |y::ys' => if y=c then ys' else y::remove_card(ys', c, e)

fun all_same_color(cs: card list) =
    case cs of
        [] => true
        |c::cs' => 
            let val colour = card_color(c)
            in
                case cs' of
                    [] => true
                    |x::xs' => 
                        if colour = card_color(x)
                        then all_same_color(xs')
                        else false
            end

fun sum_cards(cs: card list) =
    let
        fun aux(acc: int, cards: card list) =
            case cards of
                [] => acc
                |card::restCards => aux(acc + card_value(card), restCards)
    in
        aux(0, cs)
    end

fun score(cs: card list, goal: int) =
    let
        val sum = sum_cards(cs)
    in
        case (sum>goal, all_same_color(cs)) of
            (true, true) => (3 * (sum-goal)) div 2
            |(false, true) => (goal - sum) div 2
            |(true, false) => 3 * (sum-goal)
            |(_, _) => (goal - sum)
    end

fun officiate(cards: card list, moves: move list, goal: int) =
    let
        fun play(cards, moves, hands, goal) =
            if score(hands, goal) > goal
            then score(hands, goal)    
            else
                case moves of
                    [] => score(hands, goal)
                    |move::restMoves => case move of
                                            Discard card=> play(cards, restMoves, remove_card(hands, card, IllegalMove), goal)
                                            |Draw => case cards of
                                                        []=>score(hands, goal)
                                                        |card::restCards => play(restCards, restMoves, card::hands, goal)
    in
        play(cards, moves, [], goal)       
    end
