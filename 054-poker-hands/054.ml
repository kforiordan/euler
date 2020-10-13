
(*
    In the card game poker, a hand consists of five cards and are
    ranked, from lowest to highest, in the following way:

        High Card: Highest value card.
        One Pair: Two cards of the same value.
        Two Pairs: Two different pairs.
        Three of a Kind: Three cards of the same value.
        Straight: All cards are consecutive values.
        Flush: All cards of the same suit.
        Full House: Three of a kind and a pair.
        Four of a Kind: Four cards of the same value.
        Straight Flush: All cards are consecutive values of same suit.
        Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.

    The cards are valued in the order: 2 - 10, Jack, Queen, King, Ace.

    If two players have the same ranked hands then the rank made up of
    the highest value wins; for example, a pair of eights beats a pair
    of fives (see example 1 below). But if two ranks tie, for example,
    both players have a pair of queens, then highest cards in each
    hand are compared (see example 4 below); if the highest cards tie
    then the next highest cards are compared, and so on.

    Consider the following five hands dealt to two players:
    Hand	 	Player 1	 	Player 2	 	Winner
    1	 	5H 5C 6S 7S KD
    Pair of Fives
                    2C 3S 8S 8D TD
    Pair of Eights
                    Player 2
    2	 	5D 8C 9S JS AC
    Highest card Ace
                    2C 5C 7D 8S QH
    Highest card Queen
                    Player 1
    3	 	2D 9C AS AH AC
    Three Aces
                    3D 6D 7D TD QD
    Flush with Diamonds
                    Player 2
    4	 	4D 6S 9H QH QC
    Pair of Queens
    Highest card Nine
                    3D 6D 7H QD QS
    Pair of Queens
    Highest card Seven
                    Player 1
    5	 	2H 2D 4C 4D 4S
    Full House
    With Three Fours
                    3C 3D 3S 9S 9D
    Full House
    with Three Threes
                    Player 1

    The file, poker.txt, contains one-thousand random hands dealt to
    two players. Each line of the file contains ten cards (separated
    by a single space): the first five are Player 1's cards and the
    last five are Player 2's cards. You can assume that all hands are
    valid (no invalid characters or repeated cards), each player's
    hand is in no specific order, and in each hand there is a clear
    winner.

    How many hands does Player 1 win?
*)

open Base;;

type suit = Spades | Hearts | Diamonds | Clubs ;;

type card =
  Ace of suit
| King of suit
| Queen of suit
| Jack of suit
| Minor_card of suit * int;;

(* type rank =
 *   High_card of int
 * | One_pair of int
 * | Two_pairs of int * int
 * | Three_of_a_kind of int
 * | Straight of int
 * | Flush of int
 * | Full_house of int * int
 * | Four_of_a_kind of int
 * | Straight_flush of int
 * | Royal_flush *)

let suit = function Ace s | King s | Queen s | Jack s -> s
                    | Minor_card (s, i) -> s;;

(* let rank_exponent = function
 *   | High_card c -> 
 *   | One_pair c -> 2
 *   | Two_pairs c -> 4
 *   | Three_of_a_kind c -> 8
 *   | Straight c -> 8
 *   | Flush c -> 16
 *   | Full_house c -> 32
 *   | Four_of_a_kind -> 64
 *   |  *)

let value = function
  | Ace s -> 14
  | King s -> 13
  | Queen s -> 12
  | Jack s -> 11
  | Minor_card (s, i) -> i;;

let cmp_cards a b =
  if value a < value b then -1
  else if value a > value b then 1
  else 0;;

let e hand =
  let hand' = List.sort ~compare:cmp_cards hand in
  let magic = 16 in
  let rec aux i cards =
    match cards with
      [] -> 0
    | x :: xs -> (value x) + (magic ** i) + aux (i+1) xs
  in aux 0 hand';;

let _ = e [Minor_card (Hearts, 2);
           Minor_card (Clubs, 4)];;

let is_valid (hand:card list) = List.length hand = 5;;

let is_same_suit a b =
  match suit a, suit b with
    Clubs, Clubs | Diamonds, Diamonds | Hearts, Hearts | Spades, Spades -> true
    | _, _ -> false;;

let is_same_value a b = value a = value b

let high hand =
  let rec aux hand' high' =
    match hand', high' with
      [], _ -> high'
    | card :: cards, None -> aux cards (Some card)
    | card :: cards, (Some high'') -> if cmp_cards card high'' = 1
                                      then aux cards (Some card)
                                      else aux cards (Some high'')
  in aux hand None;;

let is_straight hand =
  if is_valid hand then
    let hand' = List.sort ~compare:cmp_cards hand in
    let rec aux prev_value cards =
      match cards with
        [] -> true
      | x :: xs -> let v = value x in if prev_value = 0 || v = (prev_value+1)
                                      then aux v xs
                                      else false
    in
    aux 0 (List.sort ~compare:cmp_cards hand')
  else
    false;;

let is_flush hand =
  if is_valid hand then
    let hand' = List.sort ~compare:cmp_cards hand in
    let rec aux cards prev =
      match cards, prev with
        [], _ -> true
      | x :: xs, None -> aux xs (Some x)
      | x :: xs, (Some s) when is_same_suit s x -> aux xs prev
      | x :: xs, (Some s) -> false
    in aux hand' None
  else
    false;;

let is_straight_flush hand = is_straight hand && is_flush hand;;

let is_royal_flush hand = is_straight_flush hand &&
                            match hand with
                              hd :: tl when value hd = 10 -> true
                            | _ -> false;;

let is_full_house hand =
  let rec aux cards 

let is_of_a_kind n hand =
  let rec aux cards prev i =
    match cards, prev with
      [], _ -> i >= n
    | x :: xs, None -> aux xs (Some x) (i+1)
    | x :: xs, (Some v) ->
       if i >= n
       then true
       else
         if is_same_value v x
         then aux xs prev (i+1)
         else aux xs (Some x) 1
  in aux (List.sort ~compare:cmp_cards hand) None 0;;

let is_one_pair = is_of_a_kind 2;;
let is_three_of_a_kind = is_of_a_kind 3;;
let is_four_of_a_kind = is_of_a_kind 4;;

let is_two_pairs hand = false;;
let is_full_house hand = false;;

(* This is broken.  In the case of hands of similar rank it will
   compare both hands as if they both contained nothing.  e.g., given
   [8;8;6;5;4] and [6;6;Q;3;2] this function will choose the latter as
   the winner, though a pair of 8s should beat a pair of sixes.

   I'll replace the is_* functions with score_* functions that will
   allow proper comparison.
  *)
let compare_hands a b =
  let rec high_cmp a b =
    match a, b with
      [], [] -> 0
    | x :: xs, y :: ys -> let v = cmp_cards x y in
                          if v = 0
                          then high_cmp xs ys
                          else v
    | _, _ -> 0 (* BUG!  Should raise exception here. *)
  in
  let tests = [is_royal_flush;
               is_straight_flush;
               is_four_of_a_kind;
               is_full_house;
               is_flush;
               is_straight;
               is_three_of_a_kind;
               is_two_pairs;
               is_one_pair]
  in
  let aux_cmp t a b =
    match t a, t b with
      true, true -> high_cmp a b
    | false, true -> -1
    | false, false -> 0
    | true, false -> 1
  in
  let rec compare_hands' tests' a' b' =
    match tests' with
      [] -> high_cmp a' b'
    | t :: ts -> let v = aux_cmp t a' b' in
                 if v = 0
                 then compare_hands' ts a' b'
                 else v
  in compare_hands' tests a b;;


let hand = [(Minor_card (Hearts, 4)); (Ace Hearts); Minor_card (Spades, 9)];;

let worthless_hand = [Queen Diamonds;
                      Jack Clubs;
                      Minor_card (Hearts, 3);
                      Minor_card (Spades, 5);
                      Minor_card (Hearts, 2)];;

let straight_hand = [Minor_card (Hearts, 8);
                     Minor_card (Hearts, 9);
                     Minor_card (Spades, 7);
                     Minor_card (Spades, 10);
                     Jack Clubs];;

let flush_hand = [Minor_card (Clubs, 4);
                  Minor_card (Clubs, 2);
                  Minor_card (Clubs, 8);
                  King Clubs;
                  Ace Clubs];;

let p2 = [Minor_card (Clubs, 3);
          Minor_card (Hearts, 3);
          Minor_card (Clubs, 2);
          Jack Clubs;
          Minor_card (Diamonds, 4)];;

let p3 = [Minor_card (Clubs, 4);
          Minor_card (Hearts, 3);
          Minor_card (Clubs, 3);
          Jack Clubs;
          Minor_card (Diamonds, 4)];;

let p4 = [Minor_card (Clubs, 4);
          Minor_card (Hearts, 3);
          Minor_card (Clubs, 3);
          Minor_card (Diamonds, 3);
          Minor_card (Diamonds, 4)];;


let _ = is_straight straight_hand;;
let _ = is_straight worthless_hand;;

let _ = is_flush straight_hand;;
let _ = is_flush flush_hand;;

let _ = is_royal_flush straight_hand;;
let _ = is_royal_flush flush_hand;;

