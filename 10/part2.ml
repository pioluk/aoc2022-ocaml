open Base
open Stdio

let () =
  let lines = In_channel.read_lines "input.txt" in
  let _, values =
    List.fold ~init:(1, [])
      ~f:(fun (v, l) line ->
        match String.split ~on:' ' line with
        | ["noop"] -> (v, List.append l [v])
        | ["addx"; x] -> (v + Int.of_string x, List.append l [v; v])
        | _ -> (v, l))
      lines
  in
  let row_values = List.chunks_of ~length:40 values in
  let rows =
    List.map
      ~f:(fun xs ->
        xs
        |> List.mapi ~f:(fun i c ->
               match Array.exists ~f:(( = ) i) [|c - 1; c; c + 1|] with
               | true -> "█"
               | false -> " ")
        |> String.concat ~sep:"")
      row_values
  in
  String.concat ~sep:"\n" rows |> printf "\n%s\n"
