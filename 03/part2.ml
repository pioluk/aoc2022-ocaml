open Base
open Stdio

let char_set_of_list = Set.of_list (module Char)

let get_priority c =
  let code = Char.to_int c in
  if code >= 65 && code <= 90 then Some (code - 38)
  else if code >= 97 && code <= 122 then Some (code - 96)
  else None

let group l =
  let rec loop l1 l2 l3 =
    match (l1, l2, l3) with
    | l1, l2, l3 when List.length l1 = 3 -> loop [] (List.cons l1 l2) l3
    | l1, l2, x :: rest -> loop (List.cons x l1) l2 rest
    | l1, l2, [] when not @@ List.is_empty l1 -> loop [] (List.cons l1 l2) []
    | _, l2, [] -> l2
  in
  loop [] [] l

let sum_ints = List.fold ~init:0 ~f:( + )

let () =
  let lines = In_channel.read_lines "input.txt" in
  lines |> group
  |> List.map ~f:(fun group ->
         group
         |> List.map ~f:(fun x -> char_set_of_list @@ String.to_list x)
         |> List.reduce ~f:Set.inter)
  |> List.filter_opt |> List.map ~f:Set.to_list |> List.join
  |> List.map ~f:get_priority |> List.filter_opt |> sum_ints |> printf "%d\n"
