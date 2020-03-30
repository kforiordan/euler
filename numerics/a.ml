
(* wtf is this *)
let first i =
  let n = 10 in
  let x = 0.5 in
  let h = 1.0 in
  let rec aux i' h' x' =
    if i' = n
    then []
    else
      let h'' = 0.25 *. h' in
      let y = (sin (x +. h') -. sin x) /. h' in
      let error = abs ((cos x) -. y)
      in
      (i', h', y, error) :: aux
  in aux' i h;;
