open Base
open Stdio

let group_lines input =
  let rec f temp inner all =
    match all with
    | [] -> inner
    | x :: rest ->
      if equal_string x "" then f [] (temp :: inner) rest
      else f (x :: temp) inner rest
  in
  f [] [] input

let () =
  let lines = In_channel.read_lines "input.txt" in
  let grouped = group_lines lines in
  let grouped_ints = List.map ~f:(List.map ~f:Int.of_string) grouped in
  let summed = List.map ~f:(List.fold ~init:0 ~f:( + )) grouped_ints in
  let max_calories = List.fold ~init:0 ~f:max summed in
  printf "%d\n" max_calories
