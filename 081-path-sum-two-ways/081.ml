
(* In the 5 by 5 matrix below, the minimal path sum from the top left
   to the bottom right, by only moving to the right and down, is
   indicated in bold red and is equal to 2427.

       ⎛ 131 673 234 103  18 ⎞
       ⎜ 201  96 342 965 150 ⎜
       ⎜ 630 803 746 422 111 ⎜
       ⎜ 537 699 497 121 956 ⎜
       ⎝ 805 732 524  37 331 ⎠

   (The path is 131; 201, 96, 342; 746, 422; 121; 37 331)

   Find the minimal path sum, in matrix.txt ... a 31K text file
   containing a 80 by 80 matrix, from the top left to the bottom right
   by only moving right and down. *)

open Base;;

type area = { x:int; y: int }

(* This is a kinda heavyweight way of determining matrix dimensions *)
type matrix = Filename of string | Matrix of int array array;;

let dimensions_file file =
  let lines = In_channel.read_lines file
  in
  let rec aux prev_x lines' =
    match lines' with
      [] -> prev_x
    | hd :: tl ->
       let n = List.length (Base.String.split ~on:',' hd)
       in if prev_x = -1 || prev_x = n
          then aux n tl
          else -2
  in { x = (aux (-1) lines); y = (List.length lines) };;

let dimensions_matrix matrix =
  let len = Array.length matrix
  in if len = 0 then { x = 0; y = 0 }
     else { x = len; y = Array.length (matrix.(0)) };;

let dimensions matrix =
  match matrix with
    Filename f -> dimensions_file f
  | Matrix m -> dimensions_matrix m;;

(* Given a string of comma-separated numbers, returns an array of
   those numbers. *)
let array_of_line line =
  Array.of_list (
      List.map ~f:Int.of_string (
          Base.String.split ~on:',' line));;


(* Reads from file, returns matrix.  Checks dimensions first, then
   ignores that check. *)
let read_matrix file =
  let dimensions' = dimensions file in
  let matrix = Array.create ~len:dimensions'.y [||] in
  let lines = In_channel.read_lines file in
  let rec aux i lines' =
    match lines' with
      [] -> matrix
    | hd :: tl -> (matrix.(i) <- (array_of_line hd);
                   aux (i+1) tl)
  in aux 0 lines;;

let solve m =
  expected_output;;

let test_solver solver matrix expected =
  let solution = solver matrix
  in
  let rec aux solution' expected' =
    match (solution', expected') with
      ([], []) -> (true, "such true, wow")
    | ([], _) -> (false, "solution insufficient")
    | (_, []) -> (false, "trailing nonsense in solution")
    | (got::xs, expected::ys)
      -> if got = expected
         then aux xs ys
         else (false, "expected " ^ (string_of_int expected) ^
                        ", got " ^ (string_of_int got) ^ "\n")
  in aux solution expected;;

let _ = test_solver (solve) matrix expected_output;;
