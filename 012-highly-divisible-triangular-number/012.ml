
(* Courtesy of dhart -- wait a minute, that's Gauss's formula! *)
let triangle n = ((n * n) + n) / 2;;

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

let n_factors x = List.length (factors x);;

let solve n =
  let rec aux i =
    let t = triangle i in
    if n_factors t > n
    then (i,t)
    else aux (i+1)
  in aux 1;;
