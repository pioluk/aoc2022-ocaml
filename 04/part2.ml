open Base
open Stdio

let get_range desc =
  match String.split ~on:'-' desc with
  | x :: y :: _ -> Some (Int.of_string x, Int.of_string y)
  | _ -> None

let () =
  let lines = In_channel.read_lines "input.txt" in
  lines
  |> List.map ~f:(String.split ~on:',')
  |> List.map ~f:(function
       | s1 :: s2 :: _ -> (
         match (get_range s1, get_range s2) with
         | Some x, Some y -> Some (x, y)
         | _ -> None)
       | _ -> None)
  |> List.filter_opt
  |> List.filter ~f:(fun ((low1, high1), (low2, high2)) ->
         (low1 <= high2 && low2 <= high1) || (low2 <= high1 && low1 <= high2))
  |> List.length |> printf "%d\n"
