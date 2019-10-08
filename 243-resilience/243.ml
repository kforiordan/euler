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

(* Haskell has these built in. *)
let product = List.fold_left ~f:( * ) ~init:1;;
let sum = List.fold_left ~f:( + ) ~init:0;;

let rec euclid_gcd n m = if n < m
                         then euclid_gcd m n
                         else
                           let rec aux n' m' =
                             let r = n' mod m'
                             in if r = 0 then m' else aux m' r
                           in aux n m;;

let is_coprime n m = if euclid_gcd n m = 1 then true else false;;

let resilience_computational denominator =
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

let resilience_quotient f denominator =
  let (n, d) = f denominator in
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
    let candidate = product factors in
    let _ = print_string ((string_of_int candidate) ^ "\n") in
    let r = resilience_quotient (resilience_computational) candidate
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
   solution.
*)

(* It's not the product of the first n primes anyway.  The product of
   2;3;5;7;11;13;17;19;23 has a resilience quotient of
   16358819608886738, which is 1.0000000032417349 times the threshold.
   So I'm close, much closer than yesterday's 1.025, but not quite
   there.  Adding another factor, 29, brings the resilience to
   0.965517 times the target, so I guess there's some number between
   those two prime products that is just slightly under the target,
   but I can't see how to find it mathematically, and I don't think it
   can be brute forced, at least not the way I'm doing it.  *)

let resilience_factors factors =
  let max_numerator = (product factors) - 1 in
  let rec aux factors' remaining multiples =
    match factors' with
      [] -> 0
    | hd :: tl -> let multiples' = remaining / hd
                  in
                  let remainder = remaining - multiples'
                  in
                  multiples' + aux tl remainder (multiples + multiples')
  in
  (max_numerator - (aux factors max_numerator 0), max_numerator);;

let _ =
  let factors = [2;3;5;7;11;13;17;19;23;29] in
  resilience_quotient (resilience_factors) factors;;

let _ = product [2;3;5;7;11;13;17;19;23;2;3];;

let hmm =
  let o = [[2;3;5;7;11;13;17;19;23];
           [2;3;5;7;11;13;17;19;23;2];
           [2;3;5;7;11;13;17;19;23;2;2];
           [2;3;5;7;11;13;17;19;23;2;3]]
  in
  let f p = resilience_quotient (resilience_computational) (product p) in
  List.map ~f:f o;;

let f = resilience_quotient (resilience_computational);;

f 81;;

let topn =
  List.take
    (let cmp = fun (d,q) (d',q') -> if q < q' then -1
                                    else if q > q' then 1
                                    else 0
     in
     (List.sort ~compare:cmp
        (List.map ~f:(fun x -> (x, f x))
           (List.range 1 10000)))) 30;;

let solve_mathematically target =
  let rec aux factors =
    if (resilience_quotient (resilience_factors) factors) < target
    then product factors
    else
      match factors with
        [] -> aux [2]
      | hd :: _ -> ( print_string ((Int.to_string hd) ^ "\n");
                     aux (next_prime hd :: factors) )
  in aux [2];;

let solve = solve_mathematically;;

let _ = target_resilience;;
let _ = solve target_resilience;;


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


(* The solution is the product of [2;2;2;3;5;7;11;13;17;19;23].  I
   found this by trial and error.  Looking in the forum, I see the
   real math answers involve totients, whatever they are, so I think I
   should do a few totient-related problems. *)

