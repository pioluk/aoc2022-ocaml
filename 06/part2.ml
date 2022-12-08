open Base
open Stdio

let char_set_of_list = Set.of_list (module Char)

let n = 14

let () =
  let content = In_channel.read_all "input.txt" in
  let content_length = String.length content in
  let res =
    List.range 0 (content_length - n)
    |> List.find ~f:(fun i ->
           let str = Caml.String.sub content i n in
           let s = char_set_of_list (String.to_list str) in
           let len = Set.length s in
           len = n)
  in
  match res with
  | Some i -> printf "%d\n" (i + n)
  | _ -> ()
