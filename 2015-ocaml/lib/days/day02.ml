(** Day 2: I Was Told There Would Be No Math *)

open Helpers

type present = { length : int; width : int; height : int }
type t = present list

(** Parse the input string into your input type. The input parameter contains
    the entire puzzle input as a string. *)
let parse_input (_input : string) : t =
  _input |> String.split_on_char '\n'
  |> List.filter (fun line -> String.trim line <> "")
  |> List.map (fun line ->
      Scanf.sscanf line "%dx%dx%d" (fun length width height ->
          { length; width; height }))

(** Solve part 1 of the puzzle. *)
let solve_part1 (input : t) : string =
  let total_area =
    List.fold_left
      (fun acc { length; width; height } ->
        let lw = length * width in
        let wh = width * height in
        let hl = height * length in
        let smallest_side = min lw (min wh hl) in
        acc + ((2 * (lw + wh + hl)) + smallest_side))
      0 input
  in
  string_of_int total_area

(** Solve part 2 of the puzzle. *)
let solve_part2 (input : t) : string =
  let total_ribbon =
    List.fold_left
      (fun acc { length; width; height } ->
        let max_side = max length (max width height) in
        let smallest_perimeter = length + width + height - max_side in
        let volume = length * width * height in
        acc + (2 * smallest_perimeter) + volume)
      0 input
  in
  string_of_int total_ribbon
