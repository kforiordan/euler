(*
If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.

Not all numbers produce palindromes so quickly. For example,

349 + 943 = 1292,
1292 + 2921 = 4213
4213 + 3124 = 7337

That is, 349 took three iterations to arrive at a palindrome.

Although no one has proved it yet, it is thought that some numbers, like 196,
never produce a palindrome. A number that never forms a palindrome through the
reverse and add process is called a Lychrel number. Due to the theoretical
nature of these numbers, and for the purpose of this problem, we shall assume
that a number is Lychrel until proven otherwise. In addition you are given that
for every number below ten-thousand, it will either (i) become a palindrome in
less than fifty iterations, or, (ii) no one, with all the computing power that
exists, has managed so far to map it to a palindrome. In fact, 10677 is the
first number to be shown to require over fifty iterations before producing a
palindrome: 4668731596684224866951378664 (53 iterations, 28-digits).

Surprisingly, there are palindromic numbers that are themselves Lychrel
numbers; the first example is 4994.

How many Lychrel numbers are there below ten-thousand?

NOTE: Wording was modified slightly on 24 April 2007 to emphasise the
theoretical nature of Lychrel numbers.
*)

open Base;;
#require "zarith";;

let digits n =
  let rec aux n' l =
    if Z.equal n' Z.zero then l
    else
      let ten = Z.of_int 10 in
      let n'' = Z.div n' ten and r = Z.(mod) n' ten
      in aux n'' (r :: l)
  in aux n [];;

let combine digits =
  let rec aux digits power =
    match digits with
      [] -> Z.zero
    | x :: xs -> Z.add (Z.mul x (Z.pow (Z.of_int 10) power))
                   (aux xs (power+1))
  in aux (List.rev digits) 0;;

let reverse n = combine (List.rev (digits n));;

let add_own_reverse n = Z.add n (reverse n);;

let is_palindrome a = Z.equal a (reverse a);;

let is_lychrel n =
  let limit = 50 in
  let rec aux i n' =
    if i = limit
    then true
    else
      let n'' = add_own_reverse n' in
      if is_palindrome n''
      then false
      else aux (i+1) n''
  in aux 0 n;;

let lychrel_numbers =
  List.map ~f:Z.to_int
    (List.filter ~f:is_lychrel
       (List.map ~f:Z.of_int
          (List.range 1 10001)));;

let solution = List.length lychrel_numbers;;
