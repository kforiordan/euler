open Core

(* Haskell has these built in.  Haskell is so great.  I wish I was Haskell. *)
val product 
let product = List.fold_left ~f:( * ) ~init:1
let sum     = List.fold_left ~f:( + ) ~init:0

(* Python thinks this is bad.  Python is great.  I wish I was Python. *)
let cmp a b = if a < b then -1 else if a > b then 1 else 0

(* I'm sure there used to be a function like this, but I can't see one
   in Jane Street's Core. *)
let sort_uniq l =
  let remove_dups l' a =
    match l' with
      [] -> [a]
    | hd :: tl -> if a = hd then a :: tl else a :: l'
  in
  List.fold_left ~f:remove_dups ~init:[] (List.sort ~compare:cmp l)

let int_sqrt n = int_of_float (sqrt(float_of_int n))

let is_prime n =
  let upper_limit = int_sqrt n
  in
  let rec aux n' =
    if n' > upper_limit
    then true
    else
      if n mod n' = 0
      then false
      else aux (n' + 1)
  in
  if n < 2 then false else aux n

(* A reasonable O(n), tail-recursive function that returns the nth
   fibonacci number, where the first two numbers are 0 and 1. *)
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
  in aux 2 first_fib second_fib

(* An O(n^2) function that generates a list of every fibonacci number
   up to a given limit. *)
let fib_upto x =
  let rec aux n =
    let f = fib n in
    if f > x
    then []
    else f :: aux (n + 1)
  in aux 0
