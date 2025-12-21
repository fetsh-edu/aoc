(** Day 4: TODO - Add puzzle title *)

open Helpers

type t = string

(** Parse the input string into your input type. The input parameter contains
    the entire puzzle input as a string. *)
let parse_input = String.trim

let find_number_with_zero_bits zero_bits secret : int =
  let rec loop n =
    let s = secret ^ string_of_int n in
    let d = Digest.MD5.string s |> Digest.to_hex in
    if d |> String.starts_with ~prefix:(String.make zero_bits '0') then n
    else loop (n + 1)
  in
  loop 1

(** Solve part 1 of the puzzle. *)
let solve_part1 = find_number_with_zero_bits 5 >> string_of_int

(** Solve part 2 of the puzzle. *)
let solve_part2 = find_number_with_zero_bits 6 >> string_of_int
