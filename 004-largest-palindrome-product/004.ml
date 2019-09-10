
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

let magnitude x = int_of_float (log10 (float_of_int x));;

(* Most Significant Figure. *)
let msf x = x / int_of_float (10. ** float_of_int (magnitude x));;

(* Least Significant Figure. *)
let lsf x = x mod 10;;

(* The inner part of a number.  Eg. 98765 -> 876; 1234 -> 23.  Oh no!
   This fails with zeroes.  Belphegor's number becomes
   6660000000000000, not 00000000000006660000000000000. *)
let inner x =
  let m = magnitude x in
  let leftmost = (msf x) * int_of_float((10. ** float_of_int (m)))
  and rightmost = lsf x
  in
  ((x - rightmost) - leftmost) / 10;;

let rec is_palindrome x =
  let m = magnitude x
  in
  let fig n = 0

  let rec aux  i =
    if x' < 10
    then
      true
    else
      let m = (fig ) and l = (lsf x')
      in
      if m = l
      then (* recurse *)
      else false
  in aux 0;;

let belphegor = 1000066600001;; (* Well, sorta. *)

let _ = is_palindrome 12321;;
let _ = is_palindrome 82321;;
let _ = is_palindrome 1234;;
let _ = is_palindrome 12345;;
let _ = is_palindrome belphegor;;

let _ = let sorted_prods =
          List.sort (fun x y -> if x > y then -1 else 1) (prods 100 102)
        in List.filter (is_palindrome) sorted_prods;;

