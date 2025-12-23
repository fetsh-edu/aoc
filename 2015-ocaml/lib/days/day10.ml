(** Day 10: TODO - Add puzzle title *)

open Helpers

type t = string

let parse_input = String.trim

let step_string (s : string) : string =
  let len = String.length s in
  if len = 0 then s
  else
    let buf = Buffer.create (len * 2) in
    let rec loop i current run_len =
      if i = len then (
        Buffer.add_string buf (string_of_int run_len);
        Buffer.add_char buf current)
      else
        let c = s.[i] in
        if c = current then loop (i + 1) current (run_len + 1)
        else (
          Buffer.add_string buf (string_of_int run_len);
          Buffer.add_char buf current;
          loop (i + 1) c 1)
    in
    loop 1 s.[0] 1;
    Buffer.contents buf

let run (iterations : int) (input : t) : string =
  let rec loop n s = if n = 0 then s else loop (n - 1) (step_string s) in
  loop iterations input

(** Solve part 1 of the puzzle. *)
let solve_part1 (input : t) : string =
  input |> run 40 |> String.length |> string_of_int

(** Solve part 2 of the puzzle. *)
let solve_part2 (input : t) : string =
  input |> run 50 |> String.length |> string_of_int
