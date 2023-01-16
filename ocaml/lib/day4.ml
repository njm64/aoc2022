open Base
open Stdex

type range = (int * int)
type input = (range * range) list

let make_pair lst = (List.nth_exn lst 0, List.nth_exn lst 1)

let parse_range s = 
  String.split s ~on:'-' 
  |> List.map ~f:Int.of_string 
  |> make_pair

let parse_line s = 
  String.split s ~on:',' 
  |> List.map ~f:parse_range 
  |> make_pair

let parse_input lines = List.map ~f:parse_line lines

let contains ((min_a, max_a), (min_b, max_b)) =
  (min_a <= min_b && max_b <= max_a) ||
  (min_b <= min_a && max_a <= max_b)

let overlaps ((min_a, max_a), (min_b, max_b)) =
  (min_b <= min_a && min_a <= max_b) ||
  (min_b <= max_a && max_a <= max_b) ||
  (min_a <= min_b && max_b <= max_a) ||
  (min_b <= min_a && max_a <= max_b)

let part1 = List.count ~f:contains 
let part2 = List.count ~f:overlaps

