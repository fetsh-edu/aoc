(** Day 1: Not Quite Lisp *)

type t = string
(** TODO: Define your input type. For example:
    - type t = int list (list of numbers)
    - type t = string list (list of lines)
    - type t = char array array (2D grid)
    - type t = (int * int) list (list of pairs) *)

(** Parse the input string into your input type. The input parameter contains
    the entire puzzle input as a string. *)
let parse_input = String.trim

(** Solve part 1 of the puzzle. *)
let solve_part1 input =
  input
  |> String.fold_left (fun acc c -> acc + if c = '(' then 1 else -1) 0
  |> string_of_int

(** Solve part 2 of the puzzle. *)
let solve_part2 input =
  let rec find_floor floor pos =
    if floor = -1 then Some pos
    else if pos = String.length input then None
    else
      match input.[pos] with
      | '(' -> find_floor (floor + 1) (pos + 1)
      | ')' -> find_floor (floor - 1) (pos + 1)
      | _ -> None
  in
  match find_floor 0 0 with Some pos -> string_of_int pos | None -> "No way"
