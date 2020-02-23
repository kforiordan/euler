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
