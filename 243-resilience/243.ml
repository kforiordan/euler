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

(* Borrowed from 027, which borrowed it from 010 - I really need to
   write a module at this stage. *)
let is_prime x =
  if x < 2 then false
  else
    let int_sqrt n = int_of_float (sqrt(float_of_int n)) in
    let upper_limit = int_sqrt x in
    let rec aux x' =
      if x' > upper_limit
      then true
      else
        if x mod x' = 0
        then false
        else aux (x' + 1)
    in
    aux 2;;

let next_prime n =
  if n < 2 then 2
  else if n = 2 then 3
  else
    let rec aux i = if is_prime i then i else aux (i + 2)
    in aux (n + 2);;

let solve_computationally target =
  let rec aux factors =
    let candidate = List.fold_left ~f:( * ) ~init:1 factors in
    let _ = print_string ((string_of_int candidate) ^ "\n") in
    let r = resilience_quotient candidate
    in
    if r < target
    then
      (candidate, r)
    else
      match factors with
        [] -> aux [next_prime candidate]
      | hd :: _ -> aux ((next_prime hd) :: factors)
  in aux [2];;

let target_resilience = 1.0 *. (15499. /. 94744.);;

(* let (solution, resilience) = solve target_resilience in
 *     (target_resilience, solution, resilience);; *)


(* My first idea was to try each number that has only prime factors,
   so 2, 6, 30, 210, 2310, ... It turns out that by the time we're
   using 23 as a factor the product is 223,092,870, which has a
   resilience quotient of 0.168, where we're looking for a bit under
   0.164 .  And that's where I'm stuck, at 1.025 times the threshold.
   Close!  The resilience function, which calls a gcd function for
   each input, isn't going to work here - or, rather, it could be made
   to work, but wouldn't finish within a minute.  I need a different
   approach. *)

(* I'm sure I'm right that the solution is a product of prime factors,
   but the trick is to reason about how much each additional prime
   factor affects resilience, and to just keep adding them until we've
   reached the threshold.

   So: for any number divisible by the first prime, 2, almost 50% of
   its proper fractions are non-resilient.  Any number divisible by
   both 2 and the second prime, 3, will have almost 50% + (50% of
   33.3%) of its proper fractions non-resilient.  Add 5 as a factor
   and we're at 50% + (50% of 33.3%) + (50% of 33.3% of 20%) ... and
   so on.  I'm guessing the product of the first 9 or 10 primes is the
   solution. *)

let resilience_quotient_factors factors =
  let things =
    let rec aux porosity' factors' =
      match factors' with
        [] -> []
      | hd :: tl -> (List.fold_left ~f:( * ) ~init:1 factors') :: (aux tl)
    in aux factors
  in
  things;;
  List.map ~f:(fun x -> 1. /. (float_of_int x)) things;;

let _ = resilience_quotient_factors [5;3;2];;

  let porosity =
    let aux factors' =
      match factors' with
        [] -> 0.
      | hd :: tl ->
         1. /. (float_of_int (List.fold_left ~f:( * ) ~init:1 factors')


let solve_mathematically target =
  let rec aux factors =
    if resilience_quotient_factors factors < target
    then List.fold_left ~f:( * ) ~init:1 factors
    else
      match factors with
        [] -> aux [2]
      | hd :: _ -> aux (next_prime hd :: factors)
  in aux [];;

let solve = solve_mathematically;;


(* Some exploratory stuff I should've done at the outset: *)

(* Borrowed from 021 *)
let proper_divisors x =
  let is_proper_divisor n d = n > d && n mod d = 0 in
  let rec aux i divisors =
    if i >= x
    then divisors
    else
      if is_proper_divisor x i
      then aux (i+1) (i :: divisors)
      else aux (i+1) divisors
  in aux 1 [];;

let _ = proper_divisors 15499;;
let _ = proper_divisors 94744;;
let _ = euclid_gcd 15499 94744;;

(* Result: 15499 and 94744 are coprime.  I can't see a reason why
   these particular numbers were chosen. *)
