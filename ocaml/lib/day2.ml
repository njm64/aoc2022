open Base
open Stdex

type input = (char * char) list 

type shape = Rock | Paper | Scissors [@@deriving equal, show]
type result = Win | Lose | Draw [@@deriving equal, show]

type rule = { 
  them : shape; 
  us : shape; 
  result : result 
} [@@deriving show]

let rules = [ 
  { them = Rock; us = Rock; result = Draw };
  { them = Rock; us = Paper; result = Win };
  { them = Rock; us = Scissors; result = Lose };
  { them = Paper; us = Rock; result = Lose };
  { them = Paper; us = Paper; result = Draw };
  { them = Paper; us = Scissors; result = Win };
  { them = Scissors; us = Rock; result = Win };
  { them = Scissors; us = Paper; result = Lose };
  { them = Scissors; us = Scissors; result = Draw };
]

let char_to_shape = function
  | 'A' | 'X' -> Rock
  | 'B' | 'Y' -> Paper
  | 'C' | 'Z' -> Scissors
  | _ -> failwith "Invalid shape"

let char_to_result = function
  | 'X' -> Lose
  | 'Y' -> Draw
  | 'Z' -> Win
  | _ -> failwith "Invalid result"

let result_to_score = function
  | Lose -> 0
  | Draw -> 3
  | Win -> 6

let shape_to_score = function
  | Rock -> 1
  | Paper -> 2
  | Scissors -> 3

let parse_line line = 
  let (a, b) = String.lsplit2_exn ~on:' ' line in (a.[0], b.[0])

let parse_input lines = List.map ~f:parse_line lines

let find_result (a, b) =
  let them = char_to_shape a in
  let us = char_to_shape b in
  let r = List.find_exn rules ~f:(fun r -> equal_shape r.us us && 
                                           equal_shape r.them them)
  in r.result

let find_shape (a, b) =
  let them = char_to_shape a in
  let result = char_to_result b in
  let r = List.find_exn rules ~f:(fun r -> equal_shape r.them them && 
                                           equal_result r.result result)
  in r.us

let score1 turn =
  let r = find_result turn in
  let s = char_to_shape (snd turn) in
  (result_to_score r) + (shape_to_score s)

let score2 turn =
  let s = find_shape turn in
  let r = char_to_result (snd turn) in
  (result_to_score r) + (shape_to_score s)

let part1 turns = List.map ~f:score1 turns |> List.sum
let part2 turns = List.map ~f:score2 turns |> List.sum

