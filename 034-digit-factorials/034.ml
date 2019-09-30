(* 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

   Find the sum of all numbers which are equal to the sum of the factorial of
   their digits.

   Note: as 1! = 1 and 2! = 2 are not sums they are not included.
*)

let rec digits n = if n < 10
                   then [n]
                   else (n mod 10) :: (digits (n / 10));;

let rec factorial n =  if n = 0
                       then 1
                       else n * factorial (n - 1);;

(* We're using Jane Street's Core, rather than the Ocaml's own, and I
   could not get fold_left to work without labelling the arguments -
   making it incompatible with the standard library. *)

let factorials_of_digits n = List.map ~f:(factorial) (digits n);;

let sum_of_factorials_of_digits n = List.fold_left
   (factorials_of_digits n) ~init:0 ~f:(+);;

let is_curious n = n = sum_of_factorials_of_digits n;;

let _ = is_curious 145;;

(* Now to find all the other curious numbers.  I'll just brute force
   my way through the naturals. *)

let curious_numbers low high =
  let rec aux i acc =
    if i > high
    then acc
    else
      if is_curious i
      then aux (i+1) (i :: acc)
      else aux (i+1) acc
  in aux low [];;

(* No need to try *all* the naturals: there are no curious numbers
   above 10,000,000 because at this point every additional digit adds
   more than 9! to the number.

   I bet there are a whole bunch of other optimisations, but this is
   fine. *)
let all_curious_numbers = curious_numbers 3 10000000;;

let solution = List.fold_left (all_curious_numbers) ~init:0 ~f:(+);;
