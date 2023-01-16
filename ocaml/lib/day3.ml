open Base
open Stdex

type input = char list list

let parse_input lines = List.map ~f:String.to_list lines

let char_priority c =
  match c with
  | 'a'..'z' -> Char.to_int c - Char.to_int 'a' + 1
  | 'A'..'Z' -> Char.to_int c - Char.to_int 'A' + 27
  | _ -> 0              

let intersect lists =
  List.map ~f:(Set.of_list (module Char)) lists
  |> List.reduce_exn ~f:Set.inter
  |> Set.to_list

let backpack_score b =
  List.chunks_of b ~length:(List.length b / 2)
  |> intersect
  |> List.map ~f:char_priority
  |> List.sum

let group_score group = 
  intersect group |> List.hd_exn |> char_priority

let part1 backpacks = 
  List.map ~f:backpack_score backpacks |> List.sum

let part2 backpacks = 
  List.chunks_of backpacks ~length:3 |> List.map ~f:group_score |> List.sum

