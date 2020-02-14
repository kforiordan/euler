
(* Courtesy of dhart -- wait a minute, that's Gauss's formula! *)
let triangle n = ((n * n) + n) / 2;;

(* Trial division from 1..√x .  O(√n) run time, as far as I can see. *)
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

type result = { nth:int; triangle:int; n_factors:int }

let solve n =
  let rec aux i =
    let t = triangle i in
    let n' = List.length (factors t) in
    if n' > n
    then
      { nth=i; triangle=t; n_factors=n' }
    else
      aux (i+1)
  in aux 1;;


(* There's some pattern here:

    # solve 16;;
    - : result = {i = 24; triangle = 300; n_factors = 18}
    # solve 32;;
    - : result = {i = 63; triangle = 2016; n_factors = 36}
    # solve 63;;
    - : result = {i = 224; triangle = 25200; n_factors = 90}
    # solve 125;;
    - : result = {i = 560; triangle = 157080; n_factors = 128}
    # solve 250;;
    - : result = {i = 2079; triangle = 2162160; n_factors = 320}
    # solve 500;;
    - : result = {i = 12375; triangle = 76576500; n_factors = 576}
    # solve 1000;;
    - : result = {i = 41040; triangle = 842161320; n_factors = 1024}
    # solve 2000;;
    - : result = {i = 313599; triangle = 49172323200; n_factors = 2304}

      16 ->   18 == 2^4 (+ 2^1)  == (2^1)(2^3 + 1)
      32 ->   36 == 2^5 (+ 2^2)  == (2^2)(2^3 + 1)
      63 ->   90 == 2^6 (+ ugh)  == ugh
     125 ->  128 == 2^7          == (2^5)(2^2)
     250 ->  320 == 2^8 (+ 2^6)  == (2^6)(2^2 + 1)
     500 ->  512 == 2^9 (+ 2^6)  == (2^6)(2^3 + 1)
    1000 -> 1024 == 2^10         == (2^7)(2^3)
    2000 -> 2304 == 2^11 (+ 2^8) == (2^8)(2^3 + 1)

   I'd be willing to bet the first triangular number with over 4,000
   factors has 4,096 factors.  Not willing to stick around and wait for
   the calculation to complete though.

   I've no idea what this means.  Probably not much.  The most divisible
   numbers are always going to be highly divisible by powers of 2.

   Ah, I'm thinking about this wrong.  There is a mathematical way to
   guess at this, and I've seen it in [checks notes] problem 243, on
   resilient numbers.
*)
