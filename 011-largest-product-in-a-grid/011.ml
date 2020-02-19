(* In the 20×20 grid [grid.csv] ... what is the greatest product of
   four adjacent numbers in the same direction (up, down, left, right,
   or diagonally) in the 20×20 grid?
*)

(* Large parts of this have been borrowed from 081.  My solution is
   verbose and ugly, and at the end of the day it's just a brute force
   approach - and I don't think there's any better way.

   What I do have is I read the data from a file, and the number of
   adjacent numbers is a parameter.  So if this comes up again, I'm
   sorted. *)

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


let n_right matrix (row, col) n =
  let dims = dimensions_matrix matrix in
  if (col + n) > dims.cols
  then
    []
  else
    let rec aux i =
      if i < n
      then
        let col' = col+i in
        ((row, col'), matrix.(row).(col')) :: aux (i+1)
      else
        []
    in aux 0;;

let n_down matrix (row, col) n =
  let dims = dimensions_matrix matrix in
  if (row + n) > dims.rows
  then
    []
  else
    let rec aux i =
      if i < n
      then
        let row' = row+i in
        ((row', col), matrix.(row').(col)) :: aux (i+1)
      else
        []
    in aux 0;;

let n_diag_down_right matrix (row, col) n =
  let dims = dimensions_matrix matrix in
  if (row + n) > dims.rows || (col + n) > dims.cols
  then
    []
  else
    let rec aux i =
      if i < n
      then
        let row' = row+i in
        let col' = col+i in
        ((row', col'), matrix.(row').(col')) :: aux (i+1)
      else
        []
    in aux 0;;


let n_diag_down_left matrix (row, col) n =
  let dims = dimensions_matrix matrix in
  if (row + n) < dims.rows && (col - n) >= 0
  then
    let rec aux i =
      if i < n
      then
        let row' = row+i in
        let col' = col-i in
        ((row', col'), matrix.(row').(col')) :: aux (i+1)
      else
        []
    in aux 0
  else
    [];;

let _ = n_right (read_matrix "grid.csv") (1,1) 4;;
let _ = n_diag_down_left (read_matrix "grid.csv") (1,1) 4;;
let _ = n_diag_down_left (read_matrix "grid.csv") (1,2) 4;;
let _ = n_diag_down_left (read_matrix "grid.csv") (1,3) 4;;
let _ = n_diag_down_left (read_matrix "grid.csv") (1,4) 4;;
let _ = n_diag_down_left (read_matrix "grid.csv") (1,5) 4;;

let n_diag = n_diag_down_right;;
let n_diag' = n_diag_down_left;;

(* We should handle the case where the cell list is junk ... *)
let prod_cells cells =
  List.fold_left ~init:1 ~f:(fun b ((r,c),v) -> b * v) cells;;


let _ = n_right (read_matrix "grid.csv") (0,0) 4;;
let _ = prod_cells (n_right (read_matrix "grid.csv") (0,0) 4);;
let _ = n_right (read_matrix "grid.csv") (0,16) 4;;
let _ = n_right (read_matrix "grid.csv") (0,17) 4;;

let solve file n =
  let matrix = read_matrix file in
  let dims = dimensions_matrix matrix in
  let prods =
    let rec vary_rows row =
      let rec vary_cols col =
        if dims.cols > col
        then
          let adjacent_right = n_right matrix (row, col) n in
          let adjacent_down = n_down matrix (row, col) n in
          let adjacent_diag = n_diag matrix (row, col) n in
          let adjacent_diag' = n_diag' matrix (row, col) n in
          let prod_right = prod_cells adjacent_right in
          let prod_down = prod_cells adjacent_down in
          let prod_diag = prod_cells adjacent_diag in
          let prod_diag' = prod_cells adjacent_diag' in
          prod_right :: prod_down :: prod_diag :: prod_diag' :: vary_cols (col+1)
        else
          []
      in
      if dims.rows > row
      then
        vary_cols 0 @ vary_rows (row+1)
      else
        []
    in vary_rows 0
  in List.sort ~compare:(fun a b -> if a < b then 1 else -1) prods;;

let _ = List.hd (solve "small.csv" 4);;
let _ = List.hd (solve "grid.csv" 4);;
