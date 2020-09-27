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

(* base2 107 = 64 + 32 + 8 + 2 + 1 = 1101011 *)
(* 107 / (2 ** 0) = 107 % 2 = 1 *)
(* 107 / (2 ** 1) =  53 % 2 = 1 *)
(* 107 / (2 ** 2) =  26 % 2 = 0 *)
(* 107 / (2 ** 3) =  13 % 2 = 1 *)
(* 107 / (2 ** 4) =   6 % 2 = 0 *)
(* 107 / (2 ** 5) =   3 % 2 = 1 *)
(* 107 / (2 ** 6) =   1 % 2 = 1 *)

(*  32 / (2 ** 0) =  32 % 2 = 0 *)
(*  32 / (2 ** 1) =  16 % 2 = 0 *)
(*  32 / (2 ** 2) =   8 % 2 = 0 *)
(*  32 / (2 ** 3) =   4 % 2 = 0 *)
(*  32 / (2 ** 4) =   2 % 2 = 0 *)
(*  32 / (2 ** 5) =   1 % 2 = 1 *)
(*  32 / (2 ** 6) =  32 % 2 = 0 *)

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

let palindromes10 =
  List.filter ~f:is_palindrome (List.range 1 1000000);;


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

let is_palindrome2 x =
  let rec aux lwr upr =
    if (nth_sig_fig x lwr) = (nth_sig_fig x upr)
    then
      if upr - lwr < 2
      then true
      else aux (lwr + 1) (upr -1)
    else
      false
  in
  aux 0 (magnitude2 x);;



let _ = is_palindrome2 107;;

let hmm =
  let x = 107 in
  List.map ~f:(fun n -> nth_sig_fig2 x n) (List.range 0 ((magnitude2 x));;

nth_sig_fig2 107 0;;
nth_sig_fig2 107 1;;
nth_sig_fig2 107 2;;
nth_sig_fig2 107 3;;
nth_sig_fig2 107 4;;
nth_sig_fig2 107 5;;
nth_sig_fig2 107 6;;
nth_sig_fig2 107 7;;

let rec print_list l =
  match l with
    [] -> ()
  | hd :: tl -> let _ = print_string ((Int.to_string hd) ^ "\n")
                in print_list tl;;

print_list hmm;;
