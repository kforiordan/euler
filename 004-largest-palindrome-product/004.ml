
(* A palindromic number reads the same both ways. The largest
   palindrome made from the product of two 2-digit numbers is 9009 =
   91 × 99.

   Find the largest palindrome made from the product of two 3-digit
   numbers. *)

let prods lwr upr =
  let rec aux i =
    let rec aux' i' =
      if i' < lwr
      then []
      else (i' * i) :: aux' (i' - 1)
    in
    if i < lwr
    then []
    else aux' i @ aux (i-1)
  in
  aux upr;;

let rec is_palindrome x =
  if x < 10
  then
    true
  else
    let x' = float_of_int x
    in (
      let msf = ( let magnitude = float_of_int (int_of_float (log10 x'))
                  in x / int_of_float (10. ** magnitude) )
        and
          lsf = x mod 10
          in
          (print_int msf; print_string " "; print_int lsf; print_string "\n"; true)
    );;

let _ = is_palindrome 12321;;
let _ = is_palindrome 82321;;

let _ = let sorted_prods =
          List.sort (fun x y -> if x > y then -1 else 1) (prods 100 102)
        in List.filter (is_palindrome) sorted_prods;;

