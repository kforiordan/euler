(*
The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases
by 3330, is unusual in two ways: (i) each of the three terms are prime, and,
(ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
exhibiting this property, but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this
sequence?
*)


(* So we want three terms, each of 4 digits, each prime, and each a
   permutation of the other two.  First I'll identify the primes, then
   for each of those find its permutations and see if any of them
   occur in the list of primes. *)

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

(* Again: module *)
let digits n =
  let rec aux n' digits =
    if n' = 0 then digits
    else
      let n'' = n' / 10 and r = n' % 10
      in aux n'' (r :: digits)
  in aux n [];;

o(* The inverse of the digits function. *)
let combine digits =
  let rec aux digits power =
    match digits with
      [] -> 0
    | x :: xs -> (x * (10 ** power)) +
                   aux xs (power+1)
  in aux (List.rev digits) 0;;


(* It took me far too long to write this permutation function.  It
   could be optimised - memoization is the first thing I can think of
   - but not tonight. *)
let rec permute l =
  let cap_lists head tails = List.map ~f:(fun x -> head :: x) tails in
  match l with
    [] -> []
  | x :: [] -> [[x]]
  | x :: x' :: [] -> [[x;x'];[x';x]]
  | x :: xs ->
     let rec adj prev remaining =
       match remaining with
         [] -> []
       | x :: remaining' ->
          (cap_lists x (permute (prev @ remaining'))) @ adj (x :: prev) remaining'
     in adj [] l;;


let is_palindrome a b = a = combine (List.rev (digits b));;

(* We can strip out any primes containing zeroes. *)
let contains_zero n =
  let f b i = b || i = 0 in
  let d = digits n in
  List.fold ~init:false ~f:f d;;

(* And we can strip out any primes containing two or more even numbers. *)
let contains_two_evens n =
  let is_even i = i % 2 = 0 in
  let f b i = b + (if is_even i then 1 else 0) in
  let d = digits n in
  if (List.fold ~init:0 ~f:f d) >= 2
  then true
  else false;;

let primes = List.filter ~f:is_prime (List.range ~stride:2 1001 10000);;

let pair n =
  let cmp a b = if a < b then -1 else if a = b then 0 else 1 in
  let sort = List.sort ~compare:cmp in
  combine (sort (digits n));;

let hmm =
  let cmp (a,a') (b,b') = if a < b then -1 else if a = b then 0 else 1 in
  List.sort ~compare:cmp
    (List.filter ~f:(fun (a,b) -> a > 1000)
       (List.map ~f:(fun x -> (pair x), x) primes));;


let rec f l =
  let rec aux prev l' =
    match l' with
      [] -> []
    | (i,n) :: tl ->
       if i = prev
       then n :: aux i tl
       else aux i tl
  in
  aux 0 l;;


let lol = List.map ~f:permute (List.map ~f:digits primes);;

let rofl = List.map ~f:(fun x -> List.map ~f:combine x) lol;;

let foo = List.map ~f:(fun x -> List.filter ~f:(fun y -> y > 1000) x) rofl;;

let bar = List.map ~f:(fun x -> List.filter ~f:is_prime x) foo;;

let quux = List.filter ~f:(fun x -> List.length x >= 3) bar;;
