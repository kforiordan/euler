(* The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

   Find the sum of all the primes below two million.
*)

(* Borrowed from 009. *)
let int_sqrt n = int_of_float (sqrt(float_of_int n));;

(* Borrowed from 003, optimised a little: was O(n), now O(sqrt(n)) *)
let is_prime x =
  let upper_limit = int_sqrt x
  in
  let rec aux x' =
    if x' > upper_limit
    then true
    else
      if x mod x' = 0
      then false
      else aux (x' + 1)
  in
  match x with
    0 | 1 -> false
    | _ -> aux 2;;

(* Borrowed from 006, modified to avoid recursion-induced stack
   overflow. *)
let seq a b =
  let rec aux i acc =
    if i < a then acc else aux (i - 1) (i :: acc)
  in aux b [];;

let upper_limit = 2000000;;
let primes = List.filter (is_prime) (seq 2 upper_limit);;

let sum_of_primes = List.fold_left (fun x y -> x + y) 0 primes;;
