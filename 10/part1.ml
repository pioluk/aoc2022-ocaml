open Base
open Stdio

let indices = [20; 60; 100; 140; 180; 220]

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
  let sampled_values =
    List.map ~f:(fun i -> List.nth_exn values (i - 1)) indices
  in
  List.zip_exn indices sampled_values
  |> List.map ~f:(fun (i, x) -> i * x)
  |> List.fold ~init:0 ~f:( + ) |> printf "%d\n"
