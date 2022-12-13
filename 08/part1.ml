open Base
open Stdio

let is_visible grid (x, y) =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  let get_height i j = grid.(i).(j) in
  let current_height = get_height x y in
  let pred n = n < current_height in
  let from_top =
    List.for_all ~f:(fun my -> pred @@ get_height x my) @@ List.range 0 y
  in
  let from_right =
    List.for_all ~f:(fun mx -> pred @@ get_height mx y)
    @@ List.range (x + 1) width
  in
  let from_bottom =
    List.for_all ~f:(fun my -> pred @@ get_height x my)
    @@ List.range (y + 1) height
  in
  let from_left =
    List.for_all ~f:(fun mx -> pred @@ get_height mx y) @@ List.range 0 x
  in
  from_top || from_right || from_bottom || from_left

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
    |> List.filter ~f:(is_visible grid)
    |> List.length
  in
  printf "%d\n" (x + (width * 2) + (height * 2) - 4)
