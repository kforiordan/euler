
(* Courtesy of dhart -- wait a minute, that's Gauss's formula! *)
let triangle n = ((n * n) + n) / 2;;

let int_sqrt n = (int_of_float (sqrt (float_of_int)));;

let int_sqrt n = int_of_float;;

let factors x =
  let int_sqrt n = (int_of_float (sqrt (float_of_int n))) in
  let rec aux m n =
    if n < m
    then
      []
    else
      if x mod n = 0
      then
        let n' = x / n in
        if n' = n
        then n :: aux m (n - 1)
        else n :: (n' :: aux m (n - 1))
      else
        aux m (n - 1)
  in aux 1 (int_sqrt x);;

let n_factors x = List.length (factors (triangle x));;

(* This will not work.  brute 100 takes seconds, brute 200 takes about
   a minute, so brute 500 is likely to take aeons. *)
let brute x =
  let rec aux i =
    if n_factors i > x
    then i
    else aux (i+1)
  in aux 1;;

let cache : (int * int) = Hashtbl.create 5000;;

let f x:int = x;;

let g (x:int) : (int) = x;;

let h (x:int * int) = x;;

let brute' x =
  try Hashtbl.find cache x with
    Not_found -> let solution = brute x in
                 let _ = Hashtbl.add cache x solution in
                 solution;;

let _ = brute' 200;;
