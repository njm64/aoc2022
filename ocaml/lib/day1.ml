open Base
open Stdex

type input = int list list

let parse_input lines =
  let parse_group = List.map ~f:Int.of_string in
  List.group String.is_empty lines 
  |> List.map ~f:parse_group

let part1 calories = 
  List.map ~f:List.sum calories 
  |> List.max

let part2 calories =
  List.map ~f:List.sum calories
  |> List.sort ~compare:(Fn.flip Int.compare)
  |> List.sub ~pos:0 ~len:3 
  |> List.sum
