(* A Pythagorean triplet is a set of three natural numbers, a < b < c,
   for which,

     a^2 + b^2 = c^2

   For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

   There exists exactly one Pythagorean triplet for which a + b + c =
   1000.  Find the product abc. *)

let int_sqrt n = int_of_float (sqrt(float_of_int n));;

let is_square n = let r = int_sqrt n in
                  if r * r = n
                  then true
                  else false;;

(* Brute force, with little optimisation.  Builds (a,b) tuples, checks
   them against the given conditions. *)
let special x =
  let rec aux b =
    if b = 0
    then []
    else
      let rec aux' a =
        if a = 0
        then []
        else
          let a2 = a * a and b2 = b * b in
          let c2 = a2 + b2 in
          let c = int_sqrt c2
          in
          if is_square c2 && a + b + c = x
          then (a, b, c) :: aux' (a-1)
          else aux' (a-1)
      in
      aux' b @ aux (b - 1)
  in aux x;;

let solution = let (a,b,c) = List.hd (special 1000) in a * b * c;;
