(* 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

   What is the sum of the digits of the number 2^1000? *)

#require "zarith";;

let f x =
  let rec aux x' digits =
    if Z.Compare.(=) x' Z.zero
    then digits
    else
      let (d, r) = Z.div_rem x' (Z.of_int 10)
      in
      aux d ((Z.to_int r) :: digits)
  in
  aux x [];;

let target = Z.pow (Z.of_int 2) 1000;;

let sum = List.fold_left (fun x y -> x + y) 0 (f target)
