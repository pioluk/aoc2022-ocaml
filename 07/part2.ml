open Base
open Stdio

type command = Cd of string | Ls

let is_command str =
  match String.split ~on:' ' str with
  | "$" :: _ -> true
  | _ -> false

let parse_command str =
  match String.split ~on:' ' str with
  | ["$"; "cd"; dir] -> Some (Cd dir)
  | ["$"; "ls"] -> Some Ls
  | _ -> None

let parse_file_entry str =
  match String.split ~on:' ' str with
  | "dir" :: _ -> None
  | [size; file] -> Some (file, Int.of_string size)
  | _ -> None

let dir_stack_to_path s = String.concat ~sep:"/" s

let foo l =
  List.range ~stop:`inclusive 1 (List.length l)
  |> List.map ~f:(fun i -> List.sub ~pos:0 ~len:i l)

let total_space = 70_000_000
let target_free_space = 30_000_000
let target_used = total_space - target_free_space

let () =
  let lines = In_channel.read_lines "input.txt" in
  let _, dir_map =
    List.fold
      ~init:([], Map.empty (module String))
      ~f:(fun (dir_stack, dir_map) line ->
        if is_command line then
          match parse_command line with
          | Some (Cd "/") -> (["/"], dir_map)
          | Some (Cd "..") -> (List.drop_last_exn dir_stack, dir_map)
          | Some (Cd dir) -> (dir_stack @ [dir], dir_map)
          | _ -> (dir_stack, dir_map)
        else
          match parse_file_entry line with
          | Some (_, size) ->
            let all_paths = foo dir_stack in
            let new_dir_map =
              List.fold ~init:dir_map
                ~f:(fun m path ->
                  Map.update
                    ~f:(function
                      | Some v -> v + size
                      | _ -> size)
                    m (dir_stack_to_path path))
                all_paths
            in
            (dir_stack, new_dir_map)
          | _ -> (dir_stack, dir_map))
      lines
  in
  let used_space =
    match Map.find dir_map "/" with
    | Some v -> v
    | _ -> -1
  in
  dir_map |> Map.to_alist
  |> List.map ~f:(fun (_, v) -> v)
  |> List.filter ~f:(fun v -> used_space - v < target_used)
  |> List.reduce_exn ~f:Int.min |> printf "%d\n"
