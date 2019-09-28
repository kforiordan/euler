(* 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

   Find the sum of all numbers which are equal to the sum of the factorial of
   their digits.

   Note: as 1! = 1 and 2! = 2 are not sums they are not included.
*)

let rec digits n = if n < 10
                   then [n]
                   else (n mod 10) :: (digits (n / 10));;

let rec factorial n =  if n = 1
                       then 1
                       else n * factorial (n - 1);;

let sum_of_factorials_of_digits n =
  List.fold_left (+) 0 (List.map (factorial) (digits n));;

(* Now to find all the other curious numbers.  I'll just brute force
   my way through the naturals. *)
