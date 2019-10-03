
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

type area = { x:int; y: int }

(* This is a kinda heavyweight way of determining matrix dimensions *)
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
          else -2  (* Should probably raise an exception here *)
  in { x = (aux (-1) lines); y = (List.length lines) };;

let dimensions_matrix matrix =
  let len = Array.length matrix
  in if len = 0 then { x = 0; y = 0 }
     else { y = len; x = Array.length (matrix.(0)) };;

(* type matrix = Matrix_filename of string | Matrix of int array array;;
 *
 * let dimensions matrix =
 *   match matrix with
 *     Matrix_filename f -> dimensions_file f
 *   | Matrix m -> dimensions_matrix m;; *)

(* Given a string of comma-separated numbers, returns an array of
   those numbers.  This is a vector of points with the same y value,
   indexed by x value. *)
let array_of_line line =
  Array.of_list (
      List.map ~f:Int.of_string (
          Base.String.split ~on:',' line));;

(* Reads from file, returns matrix.  Checks dimensions first, then
   ignores that check. *)
let read_matrix file =
  let dimensions' = dimensions_file file in
  let matrix = Array.create ~len:dimensions'.x [||] in
  let lines = In_channel.read_lines file in
  let rec aux i lines' =
    match lines' with
      [] -> matrix
    | hd :: tl -> (matrix.(i) <- (array_of_line hd);
                   aux (i+1) tl)
  in aux 0 lines;;


(* let solve m =
 *   let (min_x, min_y) = (0, 0) and
 *       (max_x, max_y) =
 *         let d = dimensions_matrix m in ((d.x - 1), (d.y - 1))
 *   in
 *   if (max_x < 0 || max y < 0)
 *   then []
 *   else
 *     let x = min_x and y = min_y
 *     in
 *     let right (x, y) = if x + 1 <= max_x then ((x+1),y) else (x,y)
 *     and down (x, y) = if y + 1 <= max_y then (x,(y+1)) else (x,y)
 *     in
 *     let cheaper (x,y) (x',y') =
 *       
 *     let cost (from_x, from_y) (to_x, to_y) =
 *       if (from_x, from_y) = (to_x, to_y)
 *       then (0, [])
 *       else
 *         m.(to_x).(to_y) +
 *           cheaper (cost (to_x
 *         m.(to_x).(to_y) + cheaper (right (x, y)) (down (x, y))
 *     in
 *     if right (x, y) = (x, y)
 *     then
 *       if down (x, y) = (x, y)
 *       then m.(x).(y)
 *       else 
 *     else
 *       m.(x)
 *     let aux x y =
 * in    in
 *     aux min_x min_y;; *)



  (*   if x = (max_x - 1)
   *   then
   *     if y = (max_y - 1)
   *     then m.(y).(x) :: path
   *     else aux x (y+1) (m.(y).(x) :: path)
   *   else
   *     aux (x+1) y (m.(y).(x) :: path)
   * in List.rev (aux min_x min_y []);; *)


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
         else (false, "expected " ^ (Int.to_string expected) ^
                        ", got " ^ (Int.to_string got) ^ "\n")
  in aux solution expected;;


let path m =
 let (min_x, min_y) = (0, 0) and
     (max_x, max_y) =
       let d = dimensions_matrix m in ((d.x - 1), (d.y - 1))
 in
 if max_y < min_y (* || max_x < min_x *)
 then
   (* An empty matrix.  No problem, so no solution. *)
   (0, [])
 else
   let right (x, y) = (x+1, y)
   and down (x, y) = (x, y+1)
   and in_bounds (x, y) = x <= max_x && y <= max_y
   and cost path =
     List.fold_left ~f:(+) ~init:0
       (List.map ~f:(fun (x,y) -> m.(x).(y)) path)
   in
   let next_hops (x, y) =
     List.filter ~f:in_bounds [right(x,y); down(x,y)]
   in
   let rec cheapest_path_from (x, y) =
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
   cheapest_path_from (0, 0);;
   (* let (lol,rofl) = cheapest_path_from (0, 0) in (9,[]);; *)
   (* let (_, path) = cheapest_path_from (0,0)
    * in path;; *)
   (* let (_, path) = cheapest_path_from (0,0)
    * in List.map ~f:(fun (x,y) -> m.(x).(y)) path;; *)

let matrix = read_matrix "test-matrix.txt";;

let expected_output = [ 131; 201; 96; 342; 746; 422; 121; 37; 331; ];;

(* 131,673,234,103,18
 * 201,96,342,965,150
 * 630,803,746,422,111
 * 537,699,497,121,956
 * 805,732,524,37,331 *)

let hmm = path matrix;;

let path_vals matrix path =
    List.map ~f:(fun (a',b') -> matrix.(a').(b')) path;;

let path_vals_only matrix (cost, path) = path_vals matrix path;;

let _ = test_solver (path) matrix expected_output;;

let _ = List.fold_left ~f:(+) ~init:0 [131;673;234;103;18;150;111;956;331];;

let _ = List.fold_left ~f:(+) ~init:0 [131;201;630;537;805;732;524;37;331];;

let _ = List.fold_left ~f:(+) ~init:0 [131;201;630;537;805;732;524;37];;

let _ = List.fold_left ~f:(+) ~init:0 expected_output;;
