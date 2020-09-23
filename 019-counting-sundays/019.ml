


(*
type date = { year:int; month:int; day:int }

let is_leap (d:date) =
  if d.year mod 4 = 0 then
    if d.year mod 100 = 0
    then d.year mod 400 = 0
    else true
  else false

let days_in_month (d:date) =
  match d.month with
    9 | 4 | 6 | 11 -> 30
    | 2 -> if is_leap d then 29 else 28
    | _ -> 31

let days_in_year (d:date) = if is_leap d then 366 else 365

let is_well_formed (d:date) =
  d.year >= 1900 &&
    d.month >= 1 && d.month <= 12 &&
      d.day >= 1 && d.day <= days_in_month d

let is_same_year d d' = d.year = d'.year

let is_same_month d d' = d.month = d'.month

let is_same_day d d' = d.day = d'.day

let is_same_date d d' = is_same_year d d' && is_same_month d d' && is_same_day d d'

let is_before earlier later =
  if earlier.year < later.year
  then true
  else
    if is_same_year earlier later
    then
      if earlier.month < later.month then true
      else
        if is_same_month earlier later
        then
          if earlier.day < later.day
          then true
          else false
        else
          false
    else
      false

let bang = { year = 1900; month = 1; day = 1 }

let today = { year = 2020; month = 5; day = 6 }

let rec days_between earlier later
  if is_same_date earlier later
  then 0
  else
    if is_before earlier later
    then
      - days_between earlier later
    else 999;;

*)
