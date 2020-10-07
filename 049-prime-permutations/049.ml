(*
The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases
by 3330, is unusual in two ways: (i) each of the three terms are prime, and,
(ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
exhibiting this property, but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this
sequence?
*)


(* So we want three terms, each of 4 digits, each prime, and each a
   permutation of the other two.  First I'll identify the primes, then
   for each of those find its permutations and see if any of them
   occur in the list of primes. *)

open Base;;

(* I really need to write a module for functions like this. *)
let is_prime n =
  let int_sqrt n = Int.of_float (Float.sqrt(Float.of_int n)) in
  let upper_limit = int_sqrt n in
  let rec aux n' =
    if n' > upper_limit
    then true
    else
      if n % n' = 0
      then false
      else aux (n' + 1)
  in
  if n < 2 then false else aux 2;;

(* Again: module *)
let digits n =
  let rec aux n' digits =
    if n' = 0 then digits
    else
      let n'' = n' / 10 and r = n' % 10
      in aux n'' (r :: digits)
  in aux n [];;

(* The inverse of the digits function. *)
let combine digits =
  let rec aux digits power =
    match digits with
      [] -> 0
    | x :: xs -> (x * (10 ** power)) +
                   aux xs (power+1)
  in aux (List.rev digits) 0;;





(* It took me far too long to write this permutation function.  It
   could be optimised - memoization is the first thing I can think of
   - but not tonight. *)
let permute l =
  let rec listcmp metacmp lx ly =
    match lx, ly with
      [], [] -> 0
    | _, [] -> 1
    | [], _ -> -1
    | x :: xs, y :: ys ->
       let r = metacmp x y in
       if r = 0
       then listcmp metacmp xs ys
       else r
  in
  let intlistcmp =
    let cmp a b = if a < b then -1 else if a > b then 1 else 0 in
    listcmp cmp
  in
  let uniq cmp l =
    let rec aux prev l' =
      match l' with
        [] -> []
      | x :: xs ->
         match prev with
           None -> x :: aux (Some x) xs
         | Some prev' ->
            if cmp x prev' = 0
            then aux (Some prev') xs
            else x :: aux (Some x) xs
    in
    aux None (List.sort ~compare:cmp l)
  in
  let rec aux l' =
    let cap_lists head tails = List.map ~f:(fun x -> head :: x) tails in
    match l' with
      [] -> []
    | x :: [] -> [[x]]
    | x :: x' :: [] -> [[x;x'];[x';x]]
    | x :: xs ->
       let rec adj prev remaining =
         match remaining with
           [] -> []
         | x :: remaining' ->
            (cap_lists x (aux (prev @ remaining'))) @ adj (x :: prev) remaining'
       in adj [] l'
  in
  uniq intlistcmp (aux l);;

let rec permute_naively l =
  let cap_lists head tails = List.map ~f:(fun x -> head :: x) tails in
  match l with
    [] -> []
  | x :: [] -> [[x]]
  | x :: x' :: [] -> [[x;x'];[x';x]]
  | x :: xs ->
     let rec adj prev remaining =
       match remaining with
         [] -> []
       | x :: remaining' ->
          (cap_lists x (permute_naively (prev @ remaining'))) @ adj (x :: prev) remaining'
     in adj [] l;;

let prime_permutations n =
  let cmp a b = if a < b then -1 else if a > b then 1 else 0 in
  List.sort ~compare:cmp
    (List.filter ~f:is_prime
       (List.map ~f:combine (permute (digits n))));;

let solve i limit =
  let i' = i + ((i+1) % 2) in
  let range = List.range ~stride:2 i' limit in
  let primes = List.filter ~f:is_prime range in
  let related_primes = List.map ~f:prime_permutations primes in
  let size_filter = List.filter ~f:(fun x -> x >= i && x < limit) in
  let related_primes' = List.map ~f:(fun l -> size_filter l) related_primes in
  uniq intlistcmp related_primes';;

let hmm = solve 1000 10000;;

let rec the3300 l =
  match l with
    [] -> []
  | hd :: [] -> []
  | hd :: hd' :: [] -> []
  | hd :: hd' :: hd'' :: tl -> if hd' - hd = hd'' - hd'
                       then (hd, hd', hd'') :: the3300 (hd' :: hd'' :: tl)
                       else the3300 (hd' :: hd'' :: tl);;

let rec the3300' l =
  match l with
    [] -> false
  | hd :: [] -> false
  | hd :: hd' :: [] -> false
  | hd :: hd' :: hd'' :: tl ->
       if hd' - hd = hd'' - hd'
       then true (* (hd, hd', hd'') :: the3300 (hd' :: hd'' :: tl) *)
       else the3300' (hd' :: hd'' :: tl);;

let solution = solve 1000 10000;;
let solns = List.filter ~f:the3300' solution;;

(* At this point I noticed that the difference is 3330, not 3300.  The answer is

     2969 ^ 6299 ^ 9629
*)

(* Detritus from here on down, kept for posterity.

 * let _ = prime_permutations 1006;;
 * 
 * let _ = is_prime 1006;;
 * 
 * let urgh = List.map ~f:digits (List.range ~stride:2 1001 1008);;
 * 
 * let hmm = List.map ~f:permute_naively urgh;;
 * let hmm = List.map ~f:permute urgh;;
 * 
 * 
 * let is_palindrome a b = a = combine (List.rev (digits b));;
 * 
 * (\* We can strip out any primes containing zeroes. *\)
 * let contains_zero n =
 *   let f b i = b || i = 0 in
 *   let d = digits n in
 *   List.fold ~init:false ~f:f d;;
 * 
 * (\* And we can strip out any primes containing two or more even numbers. *\)
 * let contains_two_evens n =
 *   let is_even i = i % 2 = 0 in
 *   let f b i = b + (if is_even i then 1 else 0) in
 *   let d = digits n in
 *   if (List.fold ~init:0 ~f:f d) >= 2
 *   then true
 *   else false;;
 * 
 * let primes = List.filter ~f:is_prime (List.range ~stride:2 1001 10000);;
 * 
 * let pair n =
 *   let cmp a b = if a < b then -1 else if a = b then 0 else 1 in
 *   let sort = List.sort ~compare:cmp in
 *   combine (sort (digits n));;
 * 
 * let hmm =
 *   let cmp (a,a') (b,b') = if a < b then -1 else if a = b then 0 else 1 in
 *   List.sort ~compare:cmp
 *     (List.filter ~f:(fun (a,b) -> a > 1000)
 *        (List.map ~f:(fun x -> (pair x), x) primes));;
 * 
 * 
 * let rec f l =
 *   let rec aux prev l' =
 *     match l' with
 *       [] -> []
 *     | (i,n) :: tl ->
 *        if i = prev
 *        then n :: aux i tl
 *        else aux i tl
 *   in
 *   aux 0 l;;
 * 
 * 
 * let lol = List.map ~f:permute (List.map ~f:digits primes);;
 * 
 * let rofl = List.map ~f:(fun x -> List.map ~f:combine x) lol;;
 * 
 * let foo = List.map ~f:(fun x -> List.filter ~f:(fun y -> y > 1000) x) rofl;;
 * 
 * let bar = List.map ~f:(fun x -> List.filter ~f:is_prime x) foo;;
 * 
 * let quux = List.filter ~f:(fun x -> List.length x >= 3) bar;;
 *)
