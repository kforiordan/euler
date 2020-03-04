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

let list_from_line line =
  Base.String.split ~on:' ' line;;

let list_from_file filename =
  let lines = In_channel.read_lines filename
  in
  List.fold_left ~f:(@) ~init:[]
    (List.map ~f:list_from_line lines);;

let triangle = tree_from_file "triangle-small.txt"

type 'a bt =
  | Empty
  | Node of 'a bt * 'a * 'a bt

(* bfs is actually harder than it sounds.

let rec add_to_tree tree n =
  match tree with
    Empty -> Node (Empty, n, Empty)
  | Node (Empty, n', right) -> Node ((add_to_tree Empty n), n', Empty)
  | Node (left, n', Empty) -> Node (left, n', (add_to_tree Empty n))
  | Node (left, n', right) -> Node (add_to_tree

let tree_from_list tree list =
  match list with
    [] -> tree
  | hd :: tl -> add_to_tree hd 
*)
