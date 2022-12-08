open Base
open Stdio

type command = int * int * int

let make_stacks desc =
  let stack_count =
    match List.last desc with
    | Some x -> (String.length x / 4) + 1
    | _ -> 0
  in
  Array.init stack_count ~f:(fun i ->
      List.drop_last_exn desc
      |> List.map ~f:(fun line -> Caml.String.sub line ((i * 4) + 1) 1)
      |> List.filter ~f:(fun x -> not @@ String.equal " " x))

let move stacks ((count, from, target) : command) =
  let from_index, target_index = (from - 1, target - 1) in
  let from_crates = Array.get stacks from_index in
  let moved = List.take from_crates count in
  let new_from_crates = List.drop from_crates count in
  let target_crates = Array.get stacks target_index in
  let new_target_crates = List.append (List.rev moved) target_crates in
  let copy = Array.copy stacks in
  Array.set copy from_index new_from_crates;
  Array.set copy target_index new_target_crates;
  copy

let parse_command (str : string) =
  match String.split ~on:' ' str with
  | _ :: count :: _ :: from :: _ :: target :: _ ->
    Some (Int.of_string count, Int.of_string from, Int.of_string target)
  | _ -> None

let () =
  let lines = In_channel.read_lines "input.txt" in
  let empty_line_index =
    match List.findi ~f:(fun _ x -> String.equal x "") lines with
    | Some (i, _) -> i
    | _ -> 0
  in
  let desc, moves = List.split_n lines empty_line_index in
  let moves =
    match moves with
    | _ :: rest -> rest
    | _ -> []
  in
  let stacks = make_stacks desc in
  let commands = moves |> List.map ~f:parse_command |> List.filter_opt in
  let final_stacks = List.fold ~init:stacks ~f:move commands in
  let top_crates =
    Array.to_list @@ Array.filter_opt
    @@ Array.map
         ~f:(function
           | hd :: _ -> Some hd
           | _ -> None)
         final_stacks
  in
  String.concat ~sep:"" top_crates |> printf "%s\n"
