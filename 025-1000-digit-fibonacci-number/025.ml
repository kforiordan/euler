(* The Fibonacci sequence is defined by the recurrence relation:

    Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.

Hence the first 12 terms will be:

    F1 = 1
    F2 = 1
    F3 = 2
    F4 = 3
    F5 = 5
    F6 = 8
    F7 = 13
    F8 = 21
    F9 = 34
    F10 = 55
    F11 = 89
    F12 = 144

The 12th term, F12, is the first term to contain three digits.

What is the index of the first term in the Fibonacci sequence to contain 1000 digits? *)


(* Tail-recursive O(n) fibonacci function borrowed from 002, modified
   to work with arbitrarily large numbers. *)

#require "zarith";;

let fib n =
  let first_fib = (Z.of_int 1) and second_fib = (Z.of_int 2)
  in
  let rec aux n' acc1 acc2 =
    if Z.Compare.(=) n Z.zero
    then first_fib
    else if Z.Compare.(=) n (Z.of_int 1)
    then second_fib
    else if Z.Compare.(=) n' n
    then Z.add acc1 acc2
    else aux (Z.succ n') acc2 (Z.add acc1 acc2)
  in aux (Z.of_int 2) first_fib second_fib;;

(Z.of_int 1) = Z.succ (Z.succ Z.zero);;

