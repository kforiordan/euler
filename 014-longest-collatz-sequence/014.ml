(*

   The following iterative sequence is defined for the set of positive
   integers:

      n → n/2 (n is even)
      n → 3n + 1 (n is odd)

   Using the rule above and starting with 13, we generate the following
   sequence: 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

   It can be seen that this sequence (starting at 13 and finishing
   at1) contains 10 terms. Although it has not been proved yet
   (Collatz Problem), it is thought that all starting numbers finish
   at 1.

   Which starting number, under one million, produces the longest
   chain?

   NOTE: Once the chain starts the terms are allowed to go above one
   million.  *)

let collatz n =
  let rec aux acc =
    match acc with
      [] -> acc
    | hd :: tl -> if hd = 1
                  then acc
                  else
                    if hd mod 2 = 0
                    then aux ((hd / 2) :: acc)
                    else aux (((3 * hd) + 1) :: acc)
  in
  List.rev (aux [n]);;

(* Because we only care about sequence length, here's a cheaper
   version of the collatz function that does not keep track of the
   sequence as it generates it. *)
let cheap_collatz n =
  let rec aux prev len =
    if prev = 1
    then len
    else
      if prev mod 2 = 0
      then aux (prev / 2) (len + 1)
      else aux ((3 * prev) + 1) (len + 1)
  in
  aux n 1;;

let solve n =
  let rec aux i longest_starter longest_length =
    if i > n
    then longest_starter
    else
      let length = cheap_collatz i in
      if length > longest_length
      then aux (i+1) (i) length
      else aux (i+1) longest_starter longest_length
  in aux 1 1 1;;
