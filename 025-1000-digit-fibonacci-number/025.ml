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
  let first_fib = 1 and second_fib = 2
  in
  let rec aux n' acc1 acc2 =
    match n with
      0 -> first_fib
    | 1 -> second_fib
    | _ -> if n' = n
           then acc1 + acc2
           else aux (n' + 1) acc2 (acc1 + acc2)
  in aux 2 first_fib second_fib;;

let fib n =
  let first_fib = (Z.of_int 1) and second_fib = (Z.of_int 1)
  in
  let rec aux n' acc1 acc2 =
    if Z.equal n Z.zero then first_fib
    else if Z.equal n (Z.of_int 1) then second_fib
    else
      if Z.equal n' n
      then Z.add acc1 acc2
      else aux (Z.succ n') acc2 (Z.add acc1 acc2)
  in aux (Z.of_int 2) first_fib second_fib;;


let solve n =
  let rec aux i =
    let s = Z.to_string (fib (Z.of_int i)) in
    if String.length s >= n
    then (i+1)
    else aux (i+1) (* I've zero-indexed the sequence; we want a 1-indexed solution. *)
  in aux 0;;

let target = 1000;;

let solution = solve target;;
