(* This module implements algorithms from Cheney & Kincaid, 4th ed. *)

(* Programming tips from section 1.1:

    1. Be careful and correct.
    2. USe pseudocode.
    3. Check and double check.
    4. Use test cases - check your pseudocode with pencil & paper.
    5. Modularize code.
    6. Generalize slightly - not too much, don't overcomplicate.
    7. Show intermediate results - including the input data.
    8. Include warning messages.
    9. Use meaningful variable names.
   10. Declare all variables.
   11. Include comments.
   12. Use clean loops - minimal, anything that can be outside should be.
   13. Declare nonchanging constants.
   14. Use appropriate data structures.
   15. Use arrays of all types - here they mean multi-dimensional arrays
   16. Use built-in functions - sin, log, exp, arcsin, etc.
   17. Use program libraries.
   18. Do not over-optimize.

   Case studies:

    1. Computing sums - add floats in order of magnitude (inc) to reduce rounding error.
    2. Mathematical constants - not necessarily provided by your programming environment.
    3. Exponents - x^y may be implemented as exp(y ln x), oh ok.
    4. Avoid mixed mode - avoid mixing int & real expressions.
    5. Precision - single & double precision is a thing.
    6. Memory fetches - favour fetches from adjacent words in memory (think >1d arrays)
    7. Avoid arrays - by overwriting
    8. Limit iterations - see comment within module
    9. Floating point equality - just don't.
   10. Equal floating point steps - fp speed/accuracy tradeoff: many additions vs fewer multiplications
   11. Function evaluations - should you use functions or repeated code?
*)

(* To me almost every case study looks like a case of
   over-optimization, or at least premature optimization.
*)

module Numath = struct
  (* "In a repetitive algorithm, one should always limit the number of
     permissible steps by the use of ... a control variable.  This
     will prevent endless cycling due to unforeseen problems
     (e.g. programming errors and rounding errors)"

     What's a good general limit though?  Is there one?
   *)
  let n_max = 10

  (* "The following is a pseudocode ot compute f'(x) at x=0.5, where
     f(x)=sin(x). ... We have neither shown the output ... nor
     explained the purpose ... We invite the reader to discover this ..."

     Well fuck.
  *)
  let first n =
    let x = 0.5 in
    let h = 1.0 in
    let i = 1 in
    let rec aux i' h' =
      if i' > n
      then []
      else
        let h'' = 0.25 *. h' in
        let y = (sin (x +. h'') -. sin x) /. h'' in
        let error = abs_float ((cos x) -. y) in
        (i', h'', y, error) :: (aux (i' + 1) h'')
    in aux i h
end

let _ = Numath.first 10;;
