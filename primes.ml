(* Comparing this guy's clojure code to my own Ocaml efforts.

   http://blog.cleancoder.com/uncle-bob/2020/04/09/ALittleMoreClojure.html
*)

(* This is the earliest is_prime *)
let is_prime x =
  let rec aux x' =
    if x' = x
    then true
    else
      if x mod x' = 0
      then false
      else aux (x' + 1)
  in
  match x with
    0 | 1 -> false
    | _ -> aux 2;;

(* This adds the sqrt limit optimisation. *)
let is_prime x =
  let upper_limit = int_sqrt x
  in
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

(* And this handles zero, negative ints, etc. *)
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

(* Ok, now on to that Clean Coder guy's clojure functions. *)

(* But first, Ocaml has no builtin range function, so here's one. *)
let rec range a b = if a >= b then []
                    else a :: range (a + 1) b;;

(*  (defn prime? [n]
      (let [sqrt (Math/sqrt n)
            divisors (range 2 (inc sqrt))]
       divisors))
*)
let is_prime n =
  let root = int_of_float (sqrt (float_of_int n)) in
  let divisors = range 2 (root + 1) in
  divisors;;


(*  (defn prime? [n]
      (let [sqrt (Math/sqrt n)
            divisors (range 2 (inc sqrt))
            remainders (map (fn [x] (rem n x)) divisors)]
       remainders))
*)
let is_prime n =
  let root = int_of_float (sqrt (float_of_int n)) in
  let divisors = range 2 (root + 1) in
  let remainders = List.map (fun x -> n mod x) divisors in
  remainders;;


(*  (defn prime? [n]
      (let [sqrt (Math/sqrt n)
            divisors (range 2 (inc sqrt))
            remainders (map (fn [x] (rem n x)) divisors)
            zeroes (filter zero? remainders)]
       zeroes))
*)
let is_prime n =
  let root = int_of_float (sqrt (float_of_int n)) in
  let divisors = range 2 (root + 1) in
  let remainders = List.map (fun x -> n mod x) divisors in
  let zeroes = List.filter (fun x -> x = 0) remainders in
  zeroes;;


(*  (defn prime? [n]
      (let [sqrt (Math/sqrt n)
            divisors (range 2 (inc sqrt))
            remainders (map (fn [x] (rem n x)) divisors)
            zeroes (filter zero? remainders)]
        (empty? zeroes)))
*)
let is_prime n =
  let root = int_of_float (sqrt (float_of_int n)) in
  let divisors = range 2 (root + 1) in
  let remainders = List.map (fun x -> n mod x) divisors in
  let zeroes = List.filter (fun x -> x = 0) remainders in
  match zeroes with
    [] -> true
  | _ -> false;;


(*  (defn primes [n]
      (let [candidates (range 1 (inc n))]
        (filter prime? candidates)))
*)
let primes n =
  let candidates = range 2 (n + 1) in
  List.filter (is_prime) candidates;;
