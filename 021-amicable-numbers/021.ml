(* Let d(n) be defined as the sum of proper divisors of n (numbers
   less than n which divide evenly into n).

   If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable
   pair and each of a and b are called amicable numbers.

   For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20,
   22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of
   284 are 1, 2, 4, 71 and 142; so d(284) = 220.

   Evaluate the sum of all the amicable numbers under 10000. *)


(* Proper divisors divide without a remainder but also are less than
   the numerator.  So, for example, 13 has only one divisor: 1.  Not 1
   and 13, just 1. *)

let is_proper_divisor n d = if n > d && n mod d = 0
                            then true else false;;

(* Returns a list of proper divisors for a given number *)
let proper_divisors x =
  let rec aux i divisors =
    if i >= x
    then divisors
    else
      if is_proper_divisor x i
      then aux (i+1) (i :: divisors)
      else aux (i+1) divisors
  in aux 1 [];;

(* Related to amicable numbers: perfect numbers.  This function is not
   used in the solution, I've just included it for comparison. *)
let is_perfect x =
  let sum = List.fold_left (+) 0 (proper_divisors x) in
  if x = sum then true else false;;

(* If the sum of a number's divisors equals the sum of another
   number's divisors, then those two numbers are 'amicable'.  Note: a
   number cannot be amicable with itself.  Has to be another number.
   a ≠ b, as per the finer print of the initial description, y'know,
   the bit you skimmed over without reading.
 *)
let amicable x =
  let sum = List.fold_left (+) 0 (proper_divisors x) in
  if x = sum then None
  else
    let x' = List.fold_left (+) 0 (proper_divisors sum) in
    if x = x' then Some sum
  else None;;

let amicable_pairs_upto n =
  let rec aux i acc =
    if i = n
    then acc
    else
      let i' = amicable i in
      let pair_order (x, y) = if (x < y) then (x, y) else (y, x) in
      match i' with
        None -> aux (i+1) acc
      | Some i'' -> aux (i+1) (pair_order (i, i'') :: acc)
  in
  let pair_cmp (a1, b1) (a2, b2) =
    if a1 = a2
    then if b1 = b2
         then 0
         else
           if b1 < b2
           then -1
           else 1
    else if a1 < a2 then -1 else 1
  in
  List.sort_uniq (pair_cmp) (aux 1 []);;

let solution =
  List.fold_left (+) 0
    (List.map (fun (x,y) -> x + y) (amicable_pairs_upto 10000));;
