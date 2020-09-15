(* By starting at the top of the triangle below and moving to adjacent
   numbers on the row below, the maximum total from top to bottom is 23.

	    3
	   7 4
	  2 4 6
	 8 5 9 3

   That is, 3 + 7 + 4 + 9 = 23.

   Find the maximum total from top to bottom of the triangle below:

     [see triangle.txt; also triangle-small.txt]

   NOTE: As there are only 16384 routes, it is possible to solve this problem
   by trying every route. However, Problem 67, is the same challenge with a
   triangle containing one-hundred rows; it cannot be solved by brute force,
   and requires a clever method! ;o)
*)

#require "core.top";;
open Core;;
open Base;;

let triangle file =
  let raw_triangle = In_channel.read_lines file in
  let n_lines = List.length raw_triangle in
  let triangle_matrix = Array.create ~len:n_lines [||] in
  let add_to_matrix i line =
    let array_of_line line' =
      Array.of_list
        (List.map ~f:Int.of_string
           (Base.String.split ~on:' ' line'))
    in
    triangle_matrix.(i) <- array_of_line line in
  let _ = List.mapi ~f:add_to_matrix raw_triangle
  in
  triangle_matrix

let read_triangle file =
  let raw_triangle = In_channel.read_lines file in
  let line_to_nums l = List.map ~f:Int.of_string (Base.String.split ~on:' ' l) in
  List.map ~f:line_to_nums raw_triangle;;

let rec shrink_row row =
  match row with
    [] -> []
  | x :: [] -> [x]
  | x :: x' :: [] -> max x x' :: []
  | x :: x' :: xs -> max x x' :: shrink_row (x' :: xs);;

let rec add_rows row row' =
  match row, row' with
    [], [] -> []
  | (_, []) | ([], _) -> []  (* This is bad *)
  | (x :: xs), (y :: ys) -> (x + y) :: add_rows xs ys;;

let a = [8; 5; 9; 3];;
let _ = shrink_row a;;
let _ = add_rows (shrink_row a) (shrink_row a);;

let solve triangle =
  match triangle with
    [] -> []
  | hd :: tl ->


let _ = solve triangle;;
