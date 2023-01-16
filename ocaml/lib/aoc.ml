open Base
open Stdio

module type Day = sig
  type input

  val parse_input : string list -> input
  val part1 : input -> int
  val part2 : input -> int
end

let days : (module Day) list = [
  (module Day1); 
  (module Day2);
  (module Day3);
  (module Day4)
]

let module_for_day d =
  match List.nth days (d - 1) with
  | Some m -> m
  | None -> failwith "Day not found"

let read_raw_input d =
  try
    let filename = Printf.sprintf "../input/day%d.txt" d in
    In_channel.read_lines filename
  with _ -> failwith "Failed to read input"

let run d =
  let module M = (val module_for_day d) in
  let input = read_raw_input d |> M.parse_input in
  for i = 1 to 2 do
    let start = Caml.Sys.time () in
    let f = if i = 1 then M.part1 else M.part2 in
    let result = f input in
    let elapsed = Caml.Sys.time () -. start in
    printf "Day %02d Part %d: %-20d %fs\n%!" d i result elapsed
  done

let run_all () =
  for d = 1 to List.length days do
    run d
  done

