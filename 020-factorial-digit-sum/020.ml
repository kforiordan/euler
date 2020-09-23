
let factorial n =
  let rec aux n' acc =
    if n' <= 1
    then acc
    else aux (n' - 1) (n' * acc)
  in
  aux n 1;;

let digits n =
  let ten = 10 in
  let rec aux n' =
    if n' < ten
    then [n']
    else
      let r = n' % ten in
      r :: aux (n' / ten)
  in List.rev (aux n);;

let solve n =
  List.fold ~init:0 ~f:(+) (digits (factorial n));;


#require "zarith";;

let zfactorial n =
  let rec aux n' acc =
    if Z.leq n' Z.one
    then acc
    else aux (Z.sub n' Z.one) (Z.mul n' acc)
  in
  aux (Z.of_int n) Z.one;;

let zdigits n =
  let ten = Z.of_int 10 in
  let rec aux n' =
    if Z.lt n' ten
    then
      [n']
    else
      let r = Z.rem n' ten in
      r :: aux (Z.div n' ten)
  in List.rev (List.map (aux n) ~f:Z.to_int);;

let zsolve n =
  List.fold ~init:0 ~f:(+) (zdigits (zfactorial n));;
