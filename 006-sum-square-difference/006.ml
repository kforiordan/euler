(* The sum of the squares of the first ten natural numbers is,

     1^2 + 2^2 + ... + 10^2 = 385

   The square of the sum of the first ten natural numbers is,

     (1 + 2 + ... + 10)^2 = 55^2 = 3025

   Hence the difference between the sum of the squares of the first
   ten natural numbers and the square of the sum is 3025 − 385 = 2640.

   Find the difference between the sum of the squares of the first one
   hundred natural numbers and the square of the sum. *)

let gauss n = (n * (n+1)) / 2;;

let rec seq a b = if a > b then [] else a :: seq (a + 1) b;;

let x = 100

let sum_of_squares = List.fold_left (fun x y -> x + y) 0
                       (List.map (fun x -> x * x) (seq 1 x));;

let square_of_sums = let g = gauss x in g * g;;

let diff = square_of_sums - sum_of_squares;;
