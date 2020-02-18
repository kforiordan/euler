(* In the 20×20 grid [grid.csv] ... what is the greatest product of
   four adjacent numbers in the same direction (up, down, left, right,
   or diagonally) in the 20×20 grid?
*)

(* Large parts of this have been borrowed from 081. *)

open Core;;
open Base;;

type dimensions = { rows:int; cols:int }




(* Given the name of a matrix file, returns matrix dimensions. *)
let dimensions_file file =
  (* I'm sure there used to be a function like this in the old stdlib.  Oh well. *)
  let sort_uniq l =
    let remove_dups l' a = a :: match l' with
                                  [] -> []
                                | hd :: tl -> if a = hd then tl else l'
    in
    let cmp a b = if a < b then -1 else if a > b then 1 else 0 in
    List.fold_left ~f:remove_dups ~init:[] (List.sort ~compare:cmp l)
  in
  let lines = In_channel.read_lines file in
  let cols =
    let line_lengths =
      sort_uniq
        (List.map ~f:List.length
           (List.map ~f:(fun s -> Base.String.split ~on:',' s) lines))
    in
    match line_lengths with
      [] -> -1 (* Raise exception here *)
    | hd :: [] -> hd
    | hd :: _ -> -(List.length line_lengths) (* Raise exception here *)
  in
  { rows = (List.length lines); cols = cols };;


(* Returns dimensions of a given matrix *)
let dimensions_matrix matrix =
  let len = Array.length matrix
  in if len = 0 then { rows = 0; cols = 0 }
     else { rows = len; cols = Array.length (matrix.(0)) };;


(* Given a string of comma-separated numbers, returns an array of
   those numbers.  This is a vector of points with the same y value,
   indexed by x value. *)
let array_of_line line = Array.of_list
                           (List.map ~f:Int.of_string
                              (Base.String.split ~on:',' line));;


(* Reads from file, returns matrix.  Checks dimensions first, then
   ignores that check.  Passes over the input file twice. *)
let read_matrix file =
  let dimensions' = dimensions_file file in
  let matrix = Array.create ~len:dimensions'.rows [||] in
  let lines = In_channel.read_lines file in
  let add_to_matrix i line = matrix.(i) <- array_of_line line in
  let _ = List.mapi ~f:add_to_matrix lines
  in
  matrix;;


let solve file n =
  let m = read_matrix file
  in (m.(0).(0), n);;

let _ = solve "grid.csv" 4;;
