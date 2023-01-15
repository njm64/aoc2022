open Base
open Stdio

let usage () = printf "Usage: aoc [day]\n"

let () =
  try
    match Caml.Sys.argv with
    | [| _ |] -> Aoc.run_all ()
    | [| _; day |] -> Aoc.run (Int.of_string day)
    | _ -> usage ()
  with Failure s -> printf "%s\n" s
