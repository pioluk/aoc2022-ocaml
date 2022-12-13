open Base
open Stdio

let count_visible own_height heights =
  match List.findi ~f:(fun _ x -> x >= own_height) heights with
  | Some (i, _) -> i + 1
  | None -> List.length heights

let get_score grid (x, y) =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  let get_height i j = grid.(i).(j) in
  let current_height = get_height x y in
  let top_heights =
    List.rev_map ~f:(fun my -> get_height x my) @@ List.range 0 y
  in
  let top_score = count_visible current_height top_heights in
  let right_heights =
    List.map ~f:(fun mx -> get_height mx y) @@ List.range (x + 1) width
  in
  let right_score = count_visible current_height right_heights in
  let bottom_heigths =
    List.map ~f:(fun my -> get_height x my) @@ List.range (y + 1) height
  in
  let bottom_score = count_visible current_height bottom_heigths in
  let left_heights =
    List.rev_map ~f:(fun mx -> get_height mx y) @@ List.range 0 x
  in
  let left_score = count_visible current_height left_heights in
  top_score * right_score * bottom_score * left_score

let explode str =
  let rec loop = function
    | a, b when a < 0 -> b
    | a, b -> loop (a - 1, Char.to_string str.[a] :: b)
  in
  loop (String.length str - 1, [])

let () =
  let lines = In_channel.read_lines "input.txt" in
  let grid =
    List.to_array
    @@ List.map
         ~f:(fun line ->
           List.to_array @@ List.map ~f:Int.of_string @@ explode line)
         lines
  in
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  let x =
    List.cartesian_product
      (List.range 1 (width - 1))
      (List.range 1 (height - 1))
    |> List.map ~f:(get_score grid)
    |> List.reduce ~f:Int.max |> Option.value_exn
  in
  printf "%d\n" x
