
(* 2520 is the smallest number that can be divided by each of the
   numbers from 1 to 10 without any remainder.

   What is the smallest positive number that is evenly divisible by
   all of the numbers from 1 to 20?  *)

(* This could be solved in reasonable time with pen and paper. *)


let s1 = [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20];;
let s2 = [2;3;5;7;11;13;17;19];; (* [1;4;6;8;9;10;12;14;15;16;18;20] *)
let s3 = [5;7;9;11;13;16;17;19];; (* [1;2;3;4;6;8;10;12;14;15;18;20] *)

let solution = List.fold_left (fun x y -> x * y) 1 s3;;
