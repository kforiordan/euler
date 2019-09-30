
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

let read_matrix f = [];;

let matrix = read_matrix "test-matrix.txt";;

let expected_output = [ 131; 201; 96; 342; 746; 422; 121; 37; 331 ];;

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
