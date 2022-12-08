open Base
open Stdio

let char_set_of_list = Set.of_list (module Char)

let get_priority c =
  let code = Char.to_int c in
  if code >= 65 && code <= 90 then Some (code - 38)
  else if code >= 97 && code <= 122 then Some (code - 96)
  else None

let () =
  let lines = In_channel.read_lines "input.txt" in
  let result =
    List.fold ~init:0 ~f:( + )
    @@ List.map
         ~f:(fun line ->
           let c_length = String.length line / 2 in
           let c1 = Caml.String.sub line 0 c_length in
           let c2 = Caml.String.sub line c_length c_length in
           let items1 = char_set_of_list (String.to_list c1) in
           let items2 = char_set_of_list (String.to_list c2) in
           let intersection = Set.inter items1 items2 in
           intersection |> Set.to_list |> List.map ~f:get_priority
           |> List.filter_opt |> List.fold ~init:0 ~f:( + ))
         lines
  in
  printf "%d\n" result |> ignore
