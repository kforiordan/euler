(* Starting in the top left corner of a 2×2 grid, and only being able
   to move to the right and down, there are exactly 6 routes to the
   bottom right corner.

   How many such routes are there through a 20×20 grid? *)

(* Based on drawing out the 4x4 solutions, I think it's (2^n - 2), so
   let's work that out. *)

#require "zarith";;

let solve_for n =
  let two = Z.of_int 2 in Z.sub (Z.pow two n) two;;

let print_z z =
  print_string ((Z.to_string z) ^ "\n");;

let _ = print_z (solve_for 20);;

(* 2^20 - 2 is not the right answer. *)


(* Turning the puzzle through 45 degrees, it looks like a tree, and it
   looks like a filled tree of depth n has 2^(n-1) paths to the leaf
   nodes, and the same number again in reverse .... so 2*(2^(n-1)) or
   2^n ?  So a 3x3 grid should have 2^3 == 8 paths, but it has 6.

   Is it 2^n + 2^(n-1) ?
 *)

let solve_for n =
  let two = Z.of_int 2 in
  Z.add (Z.pow two n) (Z.pow two (n-1));;

let _ = print_z (solve_for 20);;


(* Ok, so Lattice Paths - and here I'm looking at a South East lattice
   - are a well known part of combinatorics, and the solution is the
   from (0,0) to (a, b) is the binomial coefficient (a+b ¦ a), which
   is:

     (a+b)! / b!((a+b)-b)!  == 40! / 20!(20!)

   While I could do this by hand, I'm going to borrow the factorial
   function I wrote for 034.
 *)

let rec factorial n =  if n = 0
                       then 1
                       else n * factorial (n - 1);;

(* Hahaha, ok so doing this by naively expanding factorials isn't
   going to work without arbitrary sized numbers, and that's awkward. *)

(* I stole this from
   https://github.com/dhammikamare/Learn-OCaml/blob/master/lab02%20Recursive%20Functions/binomial_coeff.ml
   and it works, but I cannot see why. *)
let rec binomial_coeff (n:int) (k:int) :int =
  if k = 0 || k = n
  then 1
  else binomial_coeff (n-1) (k-1) + binomial_coeff (n-1) (k);;

let solve x y = binomial_coeff (x+y) x;;

(* AHAHAHAHAHA FUCK ME that dude's function isn't going to work
   because it's O(2^n) or something. *)

(* This will help dividing n! by k! where k<n *)
let rec bounded_factorial n bound =
  if n = bound
  then 1
  else n * bounded_factorial (n - 1) bound;;

(* It is not enough.  40!/20! is still too big. *)



let fdivide (n:int list) (k:int list) : (int list) * (int list) =
  let cmp = fun a b -> if a < b then -1 else if a = b then 0 else 1 in
  let nsort = List.sort ~compare:cmp in
  let rec aux n' k' =
    match n', k' with
      _, [] -> (n', [])
    | hd :: tl -> 
  in aux (nsort n) (nsort k);;


let mult = List.fold_left ~init:1 ~f:(fun a b -> a * b);;

(* Oh fuck it, I'll just do the math by hand, and sorta: *)
let _ = mult [23;29;31;33;35;37;39;2];;

(* No, that's wrong too. *)
