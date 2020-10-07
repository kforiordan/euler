
#require "core.top";;
open Core;;
open Base;;

let rec flatten lol =
  match lol with
    [] -> []
  | hd :: tl -> hd @ flatten tl;;

(* Read names from a file; split on comma; strip surrounding quotes. *)
let read_names file =
  let raw = In_channel.read_lines file in
  let split = String.split ~on:',' in
  let flattened = flatten (List.map ~f:split raw) in
  let drop_quotes s =
    String.strip ~drop:(fun c -> Char.equal c '"') s in
  let trimmed = List.map ~f:drop_quotes flattened in
  List.sort ~compare:String.compare trimmed;;

(* Surely there is a standard library function that does this? *)
let string_to_list s =
  let l = String.length s in
  let rec aux i =
    if i = l
    then []
    else s.[i] :: aux (i+1)
  in
  aux 0;;

(* A=1, B=2, ... this sums the chars in a string. *)
let string_sum s =
  let string_values =
    List.map ~f:(fun c -> (Char.to_int c) - 64) (string_to_list s)
  in
  List.fold ~init:0 ~f:(+) string_values;;

let weird_string_sum i s =
  (i+1) * string_sum s;;

let names = read_names "p022_names.txt";;

let weird_string_sums = List.mapi ~f:weird_string_sum names;;

let solution = List.fold ~init:0 ~f:(+) weird_string_sums;;
