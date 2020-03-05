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

type 'a bt =
  | Empty
  | Node of 'a bt * 'a * 'a bt

let rec insert_df (tree:'a bt) n =
  match tree with
    Empty -> Node (Empty, n, Empty)
  | Node (Empty, n', Empty) ->
     Node ((insert_df Empty n), n', Empty)
  | Node (Empty, n', right) ->
     Node ((insert_df Empty n), n', right)
  | Node (left, n', Empty) ->
     Node (left, n', insert_df Empty n)
  | Node (left, n', right) ->
     Node ((insert_df left n), n', right);;

let rec insert_bf (tree:'a bt) n =
  Empty;;

let list_to_tree f l =
  let rec aux tree l' =
    match l' with
      [] -> tree
    | hd :: tl ->
       let tree' = f tree hd in
       aux tree' tl
  in aux Empty l;;

let list_to_tree_df = list_to_tree (insert_df);;
let list_to_tree_bf = list_to_tree (insert_bf);;
