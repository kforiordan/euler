
(* This is so awful.  Every python solution is way, way better. *)

let tm_only (_, (t:Unix.tm)) = t;;


let make_tm y mon mday = let y' = if y >= 1900 then y - 1900
                                  else y
                         in
                         tm_only (Unix.mktime { tm_sec = 0;
                                                tm_min = 0;
                                                tm_hour = 0;
                                                tm_mday = mday;
                                                tm_mon = mon;
                                                tm_year = y';
                                                tm_wday = -1;
                                                tm_yday = -1;
                                                tm_isdst = false; }) ;;

let example = make_tm 1900 0 1;;
let start = make_tm 1901 0 1;;

let month_starts (d:Unix.tm) =
  let rec aux (d':Unix.tm) i acc =
    if i <= 11
    then
      let next_month = make_tm d'.tm_year (d'.tm_mon + 1) d'.tm_mday in
      aux next_month (i+1) (d'::acc)
    else acc
  in aux d d.tm_mon [];;

let years (d:Unix.tm) =
  let rec aux (d':Unix.tm) acc =
    if d'.tm_year <= 100
    then
      let m = month_starts d' in
      let next_year_start = make_tm (d'.tm_year + 1) d'.tm_mon d'.tm_mday in
      aux next_year_start (m @ acc)
    else
      acc
  in aux d [];;

let first_days = years start;;

let sundays = List.filter (fun (d:Unix.tm) -> d.tm_wday = 0) first_days;;

let solution = List.length sundays;;
