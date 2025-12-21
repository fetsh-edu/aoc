(** Day 8: Matchsticks *)

open Helpers

type t = string list

(** Parse the input string into your input type. The input parameter contains
    the entire puzzle input as a string. *)
let parse_input (input : string) : t =
  input |> String.trim |> String.split_on_char '\n'

(** Solve part 1 of the puzzle. *)

type memchar = Regular | Slash | HexFirst | HexSecond

let asses_string (line : string) : int =
  let mem_length, _ =
    line
    |> String.fold_left
         (fun (length, memchar) char ->
           match (char, memchar) with
           | '"', Regular -> (length + 1, Regular)
           | '\\', Regular -> (length, Slash)
           | '\\', Slash -> (length + 1, Regular)
           | '"', Slash -> (length + 1, Regular)
           | 'x', Slash -> (length, HexFirst)
           | _, HexFirst -> (length, HexSecond)
           | _, HexSecond -> (length + 1, Regular)
           | _, _ -> (length + 1, Regular))
         (-2, Regular)
  in
  String.length line - mem_length

let sum f = List.fold_left (fun acc x -> acc + f x) 0

(** Solve part 1 of the puzzle. *)
let solve_part1 = sum asses_string >> string_of_int

let handle_line (line : string) : int =
  String.fold_left
    (fun length -> function '"' | '\\' -> length + 2 | _ -> length + 1)
    2 line
  - String.length line

(** Solve part 2 of the puzzle. *)
let solve_part2 = sum handle_line >> string_of_int
