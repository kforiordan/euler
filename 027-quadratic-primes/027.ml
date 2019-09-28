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

(* Testing is_prime.  I really need some sort of test framework.  And
   a module for functions I reuse. *)
let test_cases = [(3, true); (4, false); (97, true); (99, false);
                  (243, false); (257, true)];;
let rec test f l =
  match l with
  | [] -> "yay"
  | (arg, result) :: tl -> if f arg = result
                           then test f tl
                           else "boo: " ^ string_of_int arg;;
let _ = test (is_prime) test_cases;;


(* Given a and b, returns a function like euler's n^2 + an + b *)
let gen_euler a b = fun n -> (n * n) + (a * n) + b;;

(* Given an euler function and an n to start with, returns the number
   of primes that function generates. *)
let rec count_primes f n = if is_prime (f n)
                           then count_primes (f) (n+1)
                           else n;;


(* Testing the gen_euler and count_primes functions *)
let f = gen_euler 1 41;;
let _ = if count_primes f 0 = 40 then "yay" else "boo";;

let f = gen_euler (-79) 1601;;
let _ = if count_primes f 0 = 80 then "yay" else "boo";;


let trial_euler_functions (min_a, max_a) (min_b, max_b) =
  let rec vary_a a acc =
    let rec vary_b b acc' =
      if b > max_b
      then acc'
      else
        let euler = gen_euler a b in
        let count = count_primes (euler) 0 in
        vary_b (b+1) ((a, b, count) :: acc')
    in
    if a > max_a
    then acc
    else
      let killer_bees = vary_b min_b [] in
      vary_a (a+1) (killer_bees @ acc)
  in vary_a min_a [];;

let largest_euler_function f a b =
  let largest_n (a, b, n) (a', b', n') =
    if n > n' then (a, b, n) else (a', b', n')
  in List.fold_left (largest_n) (0,0,-1) (f a b);;

let first_n = 0;;
let a = (-999, 999) and b = (-1000, 1000);;

let solution =
  let (a', b', n) =
    largest_euler_function (trial_euler_functions) a b
  in
  a' * b';;
