(*
The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases
by 3330, is unusual in two ways: (i) each of the three terms are prime, and,
(ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
exhibiting this property, but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this
sequence?
*)

open Base;;

(* I really need to write a module for functions like this. *)
let is_prime n =
  let int_sqrt n = Int.of_float (Float.sqrt(Float.of_int n)) in
  let upper_limit = int_sqrt n in
  let rec aux n' =
    if n' > upper_limit
    then true
    else
      if n % n' = 0
      then false
      else aux (n' + 1)
  in
  if n < 2 then false else aux 2;;

(* Anyway, so we want three terms, each of 4 digits, each prime, and
   each a permutation of the other two. *)

(* Again: module *)
let digits n =
  let rec aux n' digits =
    if n' = 0 then digits
    else
      let n'' = n' / 10 and r = n' % 10
      in aux n'' (r :: digits)
  in aux n [];;

  let cap_lists (head :'a) (tails :'a list list) :'a list list =
    List.map ~f:(fun x -> head :: x) tails;;

(* And again: module *)
let rec permute (l:int list) :int list list =
  let cap_lists (head :int) (tails :int list list) :int list list =
    List.map ~f:(fun x -> head :: x) tails
  in
  let rec adj (prev:int list) (remaining:int list) :int list list =
    match remaining with
      [] -> []
    | x :: remaining' -> let peer = 111 :: x :: 222 :: (prev @ (333 :: remaining')) in
                         let prev' = x :: prev in
                         peer :: (adj prev' remaining')
  in
  adj [] l;;


let _ = permute [1;2;3];;
let _ = permute [1;2;3;4];;

(* We'll start with the easiest thing: identify the primes. *)
let primes = List.filter ~f:is_prime (List.range 1000 9999);;


let combine digits =
  let rec aux digits power =
    match digits with
      [] -> 0
    | x :: xs -> (x * int_of_float (10. ** (float_of_int power))) +
                   aux xs (power+1)
  in aux (List.rev digits) 0;;

let is_palindrome a b = a = combine (List.rev (digits b));;
