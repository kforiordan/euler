
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

let is_same_date d d' = d.year = d'.year && d.month = d'.month && d.day = d'.day

let is_same_year d d' = d.year = d'.year

let is_same_month d d' = d.month = d'.month

let is_same_day d d' = d.day = d'.day

let is_before d d' =
  if d.year < d'.year
  then true
  else
    if is_same_year d d'
    then
      if d.month < d'.month then true
      else
        if is_same_month d d'
        then
          if d.day < d'.day
          then true
          else false
        else
          false
    else
      false

let bang = { year = 1900; month = 1; day = 1 }

let today = { year = 2020; month = 5; day = 6 }

let rec days_between d d' =
  if is_same_date d d'
  then 0
  else
    if is_before d' d
    then
      - days_between d' d
    else 999;;

