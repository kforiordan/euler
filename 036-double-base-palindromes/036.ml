(* The decimal number, 585 = 10010010012 (binary), is palindromic in both
   bases.

   Find the sum of all numbers, less than one million, which are palindromic in
   base 10 and base 2.

   (Please note that the palindromic number, in either base, may not include
   leading zeros.)
*)

open Base;;

(* FIRST, the base 10 functions, borrowed from puzzle 004 and modified
   to work with Base. *)

(* Returns order of magnitude of a number. *)
let magnitude x = Int.of_float (Float.log10 (Float.of_int x));;

(* Returns the nth significant figure from a number.  Eg. the 0th sig.
   fig of 12345 is 5, the 2nd is 3 (zero-indexed). *)
(* Hey, so in whatever stdlib I used in 004, the ** operator expected
   float args, whereas in Base it expects ints. *)
let nth_sig_fig x n = (x / (10 ** n)) % 10;;


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


(* SECOND, the base 2 reimplementations. *)
let logfn base x = Float.log x /. Float.log base;;

let magnfn base x =
  let logbase = logfn base in
  Float.to_int (logbase (Int.to_float x));;

let nthsigfigfn base x n = (x / (base ** n)) % base;;

let log2 = logfn 2.;;
let mag2 = magnfn 2.;;
let nthsigfig2 = nthsigfigfn 2;;

(* had order of arguments to nthsigfig2 mixed up, sigh. *)
let dectobin n =
  let upr = mag2 n in
  let rec aux i =
    nthsigfig2 n i :: if i = upr then [] else aux (i+1)
  in aux 0;;

(* This is a little ugly.  I should have a function that generates
   these is_palindrome functions. *)
let is_palindrome2 x =
  let rec aux lwr upr =
    if (nthsigfig2 x lwr) = (nthsigfig2 x upr)
    then
      if upr - lwr < 2
      then true
      else aux (lwr + 1) (upr -1)
    else
      false
  in
  aux 0 (mag2 x);;


let solution =
  List.fold ~init:0 ~f:(+)
    (List.filter ~f:is_palindrome2
       (List.filter ~f:is_palindrome (List.range 1 1000000)));;
