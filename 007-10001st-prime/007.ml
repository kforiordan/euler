(* By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we
   can see that the 6th prime is 13.

   What is the 10,001st prime number? *)

(* Borrowed from 003 *)
let is_prime x =
  let rec aux x' =
    if x' = x
    then true
    else
      if x mod x' = 0
      then false
      else aux (x' + 1)
  in
  match x with
    0 | 1 -> false
    | _ -> aux 2;;

let _ = is_prime 19;;

let nth_prime n =
  let rec aux i n' =
    if is_prime i
    then
      if n' = n
      then i
      else aux (i + 1) (n' + 1)
    else
      aux (i + 1) n'
  in aux 2 0;;

let target = 10000;; (0-indexing; 1st prime is really 0th)

let target_prime = nth_prime target;;
