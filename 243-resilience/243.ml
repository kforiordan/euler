(* A positive fraction whose numerator is less than its denominator is
   called a proper fraction.  For any denominator, d, there will be
   d−1 proper fractions; for example, with d = 12: 1/12 , 2/12 , 3/12
   , 4/12 , 5/12 , 6/12 , 7/12 , 8/12 , 9/12 , 10/12 , 11/12 .

   We shall call a fraction that cannot be cancelled down a resilient
   fraction.  Furthermore we shall define the resilience of a
   denominator, R(d), to be the ratio of its proper fractions that are
   resilient; for example, R(12) = 4/11 .  In fact, d = 12 is the
   smallest denominator having a resilience R(d) < 4/10 .

   Find the smallest denominator d, having a resilience R(d) < 15499/94744 . *)

let rec euclid_gcd n m = if n < m
                         then euclid_gcd m n
                         else
                           let rec aux n' m' =
                             let r = n' mod m'
                             in if r = 0 then m' else aux m' r
                           in aux n m;;

let is_coprime n m = if euclid_gcd n m = 1 then true else false;;

let resilience denominator =
  if denominator < 1
  then
    (0, 0)
  else
    let min_numerator = 1
    and max_numerator = denominator - 1
    in
    let resilient_numerators =
      let rec aux i acc =
        if i > max_numerator
        then acc
        else
          if is_coprime i denominator
          then aux (i+1) (i :: acc)
          else aux (i+1) acc
      in aux min_numerator []
    in
    ( List.length(resilient_numerators), max_numerator);;

let resilience_quotient denominator =
  let (n, d) = resilience denominator in
  if d = 0 then 0. else
    (float_of_int n) /. (float_of_int d);;
