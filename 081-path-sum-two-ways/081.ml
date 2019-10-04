
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

open Core;;
open Base;;

type dimensions = { rows:int; cols:int }


(* I'm sure there used to be a function like this in the old stdlib.  Oh well. *)
let sort_uniq l =
  let remove_dups l' a = a :: match l' with
      [] -> []
    | hd :: tl -> if a = hd then tl else l'
  in
  let cmp a b = if a < b then -1 else if a > b then 1 else 0
  in
  List.fold_left ~f:remove_dups ~init:[] (List.sort ~compare:cmp l);;


(* Given the name of a matrix file, returns matrix dimensions. *)
let dimensions_file file =
  let lines = In_channel.read_lines file
  in
  let cols =
    let line_lengths = sort_uniq
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


let cheapest_path_from_count = ref 0;;

let solve m =
  let _ = cheapest_path_from_count := 0 in
  let (min_x, min_y) = (0, 0)
  and (max_x, max_y) = let d = dimensions_matrix m in ((d.rows - 1), (d.cols - 1))
  in
  if max_y < min_y (* || max_x < min_x *)
  then
    (* An empty matrix.  No problem, so no solution. *)
    []
  else
    let right (x, y) = (x+1, y)
    and down (x, y) = (x, y+1)
    and in_bounds (x, y) = x <= max_x && y <= max_y
    (* and cost path = List.fold_left ~f:(+) ~init:0
     *                   (List.map ~f:(fun (x,y) -> m.(x).(y)) path) *)
    in
    let next_hops (x, y) = List.filter ~f:in_bounds [right(x,y); down(x,y)]
    in
    let rec cheapest_path_from (x, y) =
      let _ = cheapest_path_from_count := (!cheapest_path_from_count + 1) in
      if x = max_x && y = max_y
      then (m.(x).(y), [(x,y)])
      else
        match next_hops (x, y) with
          [] -> (m.(x).(y), [(x,y)])  (* If objective not reached this
                                        is an error condition.  Raise
                                        exception? *)
        | hd :: hd' :: _ -> let (cost1, path1) = cheapest_path_from hd
                            and (cost2, path2) = cheapest_path_from hd'
                            in
                            let (cost, path) =
                              if cost1 < cost2
                              then (cost1, path1)
                              else (cost2, path2)
                            in (m.(x).(y) + cost, (x,y) :: path)
        | hd :: _ -> let (cost, path) = cheapest_path_from hd
                     in (m.(x).(y) + cost, (x,y) :: path)
    in
    let (cost, path) = cheapest_path_from (0, 0)
    in
    path;;


let test_solver solver matrix expected =
  let solution = solver matrix
  in
  let rec aux solution' expected' =
    match (solution', expected') with
      ([], []) -> (true, "such true, wow")
    | ([], _) -> (false, "solution insufficient")
    | (_, []) -> (false, "trailing nonsense in solution")
    | (got::xs, expected::ys) ->
       if got = expected
       then aux xs ys
       else (false, "expected " ^ (Int.to_string expected) ^
                      ", got " ^ (Int.to_string got) ^ "\n")
  in aux solution expected;;


(* Given a path of points, and a matrix, returns a list of the values
   found in the matrix at each point *)
let path_vals matrix path =
    List.map ~f:(fun (a',b') -> matrix.(a').(b')) path;;

let matrix = read_matrix "test-matrix.txt"
and expected_output = [ 131; 201; 96; 342; 746; 422; 121; 37; 331; ]
    in
    test_solver (fun m -> path_vals m (solve m)) matrix expected_output;;

let _ = !cheapest_path_from_count;;
