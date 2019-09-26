(* Euler discovered the remarkable quadratic formula:

     n^2 + n + 41

   It turns out that the formula will produce 40 primes for the consecutive
   integer values 0 ≤ n ≤ 39.

   However, when n=40, 40^2 + 40 + 41 = 40(40+1) + 41 is divisible by 41, and
   certainly when n=41, 41^2 + 41 + 41 is clearly divisible by 41.

   The incredible formula n^2 − 79n + 1601 was discovered, which produces 80
   primes for the consecutive values 0 ≤ n ≤ 79 .  The product of the
   coefficients, −79 and 1601, is −126479.

   Considering quadratics of the form:

     n^2 + an + b  , where |a|<1000 and |b|≤1000

     where |n| is the modulus/absolute value of n e.g. |11|=11 and |−4|=4

   Find the product of the coefficients, a and b, for the quadratic expression
   that produces the maximum number of primes for consecutive values of n,
   starting with n=0.
 *)

(* Why does the question state the product of the terms a and b?
   Pretty sure this is an implementation clue that I don't see the
   relevance of. *)


(* Borrowed from 010, with int_sqrt function, also from 010, incorporated. *)
let is_prime x =
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
  match x with
    0 | 1 -> false
    | _ -> aux 2;;

let hmm =
let max_a = 9
and max_b = 10
and first_n = 0
    in
    let min_a = first_n - max_a
    and min_b = first_n - max_b
    in
    let gen_euler a b = fun n -> (n * n) + (a * n) + b
    in
    let rec f n a b i results =
      if b > max_b
      then if a > max_a
           then results
           else f first_n (a+1) min_b 0 results
      else
        let euler = gen_euler a b in
        let r = euler n in
        if is_prime r
        then f (n+1) a b (i+1) results
        else
          if i = 0
          then f first_n a (b+1) 0 ((a,b,i) :: results)
          else f (n+1) a b (i+1) results
    in f first_n min_a min_b 0 [];;

