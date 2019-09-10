
(* A palindromic number reads the same both ways. The largest
   palindrome made from the product of two 2-digit numbers is 9009 =
   91 × 99.

   Find the largest palindrome made from the product of two 3-digit
   numbers. *)

(* Not Belphegor's number!  It just looks similar. *)
let belphegor = 1000066600001;;

(* Returns a list of products of its two arguments. *)
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

(* Returns order of magnitude of a number. *)
let magnitude x = int_of_float (log10 (float_of_int x));;

(* Returns the nth significant figure from a number.  Eg. the 0th sig.
   fig of 12345 is 5, the 2nd is 3 (zero-indexed). *)
let nth_sig_fig x n =
  (x / int_of_float (10. ** (float_of_int n))) mod 10;;

let is_palindrome x =
  let rec aux lwr upr =
    if (nth_sig_fig x lwr) = (nth_sig_fig x upr)
    then
      if upr - lwr < 2
      then true
      else aux (lwr + 1) (upr - 1)
    else
      false
  in
  aux 0 (magnitude x);;

let sorted_prods =
  List.sort_uniq (fun x y -> if x > y then -1 else 1) (prods 100 999);;

let palindromes =
  List.filter (is_palindrome) sorted_prods;;

let largest = match palindromes with hd :: tl -> hd | [] -> -1;;


(* Remnants from a failed approach: *)

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


