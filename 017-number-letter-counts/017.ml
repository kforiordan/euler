(* If the numbers 1 to 5 are written out in words: one, two, three,
   four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in
   total.

   If all the numbers from 1 to 1000 (one thousand) inclusive were
   written out in words, how many letters would be used?

   NOTE: Do not count spaces or hyphens. For example, 342 (three hundred
   and forty-two) contains 23 letters and 115 (one hundred and
   fifteen) contains 20 letters. The use of "and" when writing out
   numbers is in compliance with British usage.
 *)

(* I started doing this with pen and paper, but decided to write a
   program that generates a string for a given number.  I didn't
   initially intend to write a module, but when I saw that Base.Int
   already binds 'zero', I figured a module was the neatest solution,
   and something I need practice with anyway.

   Removing spaces from the generated output is stupid, but it was
   quicker than counting spaces or than understanding the ocaml regexp
   syntax, which is so much more awkward than perl's.
 *)

open Core

module NumStrings = struct
  let zw (low,high,stride) strings =
    let m = List.map2 ~f:(fun i s -> (i,s))
              (List.range ~stride:stride ~stop:`inclusive low high)
              strings
    in match m with
         Unequal_lengths -> assert false
       | Ok m' -> m'

  let zero = "zero"
  let zero_map = zw (0,0,1) [zero]

  let digits = ["one"; "two"; "three"; "four"; "five";
                "six"; "seven"; "eight"; "nine"]
  let digits_map = zw (1,9,1) digits

  let teens = ["ten"; "eleven"; "twelve"; "thirteen";
               "fourteen"; "fifteen"; "sixteen"; "seventeen";
               "eighteen"; "nineteen"]
  let teens_map = zw (10,19,1) teens

  let tens = List.map ~f:(fun s -> s ^ "ty")
               ["twen"; "thir"; "for"; "fif"; "six"; "seven"; "eigh"; "nine"]
  let tens_map = zw (20,90,10) tens

  let hundreds = List.map ~f:(fun s -> s ^ "hundred") digits
  let hundreds_map = zw (100,900,100) hundreds

  let thousands = List.map ~f:(fun s -> s ^ "thousand") digits
  let thousands_map = zw (1000,9000,1000) thousands

  let numbers_map = zero_map @ digits_map @ teens_map
                    @ tens_map @ hundreds_map @ thousands_map

  let rec search m n =
    match m with
      [] -> None
    | (i, s) :: tl when i = n -> Some s
    | hd :: tl -> search tl n

  let search' m n default =
    match search m n with
      None -> default
    | Some s -> s

  let rec to_string n =
    let default = "OOPS" in
    if n > 0 && n < 20
    then
      search' numbers_map n default
    else if n >= 20 && n <= 99
    then
      let r = n % 10 in
      if r = 0
      then search' numbers_map n default
      else (search' numbers_map (n-r) default) ^ (search' numbers_map r default)
    else if n >= 100 && n <= 999
    then
      let r = n % 100 in
      if r = 0
      then search' numbers_map n default
      else (search' numbers_map (n-r) default) ^ "and" ^ to_string r
    else if n = 1000
    then
      "onethousand"
    else
      "UNIMPLEMENTED"
end;;

let add_string_lengths l = List.fold_left ~f:(+) ~init:0
                             (List.map ~f:(fun s -> String.length s) l)

let strings = List.map ~f:(fun i -> NumStrings.to_string i)
                (List.range ~stop:`inclusive 1 1000);;

let solution = add_string_lengths strings;;
