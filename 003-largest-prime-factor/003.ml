
(*  The prime factors of 13195 are 5, 7, 13 and 29.

    What is the largest prime factor of the number 600851475143 ?  *)

let factors_upto product limit =
 let rec aux x =
    if x > limit
    then []
    else
      if product mod x = 0
      then x :: aux (x + 1)
      else aux (x + 1)
 in match limit with
      0 -> []
    | 1 -> [1]
    | _ -> aux 2;;

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


let prime_factors x =
  let factors =
    let x' = int_of_float (sqrt (float_of_int x))
    in
    factors_upto x x'
  in
  List.filter (is_prime) factors;;


let target = 600851475143;;

(* let target = 13195;; *)

let largest_prime_factor =
  List.fold_left (fun x y -> if x > y then x else y) 1 (prime_factors target);;
