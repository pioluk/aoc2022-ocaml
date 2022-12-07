open Base
open Stdio

type rps = Rock | Paper | Scissors

type result = Win | Draw | Defeat

let get_result rps1 rps2 =
  match (rps1, rps2) with
  | Rock, Paper -> Defeat
  | Rock, Scissors -> Win
  | Paper, Rock -> Win
  | Paper, Scissors -> Defeat
  | Scissors, Rock -> Defeat
  | Scissors, Paper -> Win
  | _ -> Draw

let get_score a b =
  let result = get_result a b in
  let result_score =
    match result with
    | Win -> 6
    | Draw -> 3
    | Defeat -> 0
  in
  let figure_score =
    match a with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3
  in
  result_score + figure_score

let rps_of_abc = function
  | "A" -> Some Rock
  | "B" -> Some Paper
  | "C" -> Some Scissors
  | _ -> None

let result_of_xyz = function
  | "X" -> Some Defeat
  | "Y" -> Some Draw
  | "Z" -> Some Win
  | _ -> None

let get_play rps result =
  match (rps, result) with
  | Rock, Win -> Paper
  | Rock, Defeat -> Scissors
  | Paper, Win -> Scissors
  | Paper, Defeat -> Rock
  | Scissors, Win -> Rock
  | Scissors, Defeat -> Paper
  | x, Draw -> x

let () =
  let lines = In_channel.read_lines "input.txt" in
  let instructions =
    List.map
      ~f:(fun line ->
        match String.split ~on:' ' line with
        | [abc; xyz] -> (rps_of_abc abc, result_of_xyz xyz)
        | _ -> (None, None))
      lines
  in
  let plays =
    List.map
      ~f:(function
        | Some rps, Some result -> (Some rps, Some (get_play rps result))
        | _ -> (None, None))
      instructions
  in
  let scores =
    List.map
      ~f:(function
        | Some a, Some b -> get_score b a
        | _ -> 0)
      plays
  in
  let final_score = List.fold ~init:0 ~f:( + ) scores in
  printf "%d\n" final_score |> ignore
