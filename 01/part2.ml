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
  let top3_summed =
    List.fold ~init:0 ~f:( + )
    @@ List.take (List.sort ~compare:(fun a b -> -1 * compare_int a b) summed) 3
  in
  printf "%d\n" top3_summed
