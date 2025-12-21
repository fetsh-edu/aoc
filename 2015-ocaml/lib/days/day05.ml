(** Day 5: Doesn't He Have Intern-Elves For This? *)

open Helpers

type t = string list

(** Parse the input string into your input type. The input parameter contains
    the entire puzzle input as a string. *)
let parse_input = String.split_on_char '\n'

let is_nice s =
  let is_illegal c1 c2 =
    match (c1, c2) with
    | 'a', 'b' | 'c', 'd' | 'p', 'q' | 'x', 'y' -> true
    | _ -> false
  in
  let length = String.length s in
  let rec loop vowels has_double i =
    if i >= length then vowels >= 3 && has_double
    else
      let c = s.[i] in
      let vowels = if String.is_vowel c then vowels + 1 else vowels in
      if i = 0 then loop vowels has_double (i + 1)
      else
        let prev_c = s.[i - 1] in
        if is_illegal prev_c c then false
        else
          let has_double = has_double || c = prev_c in
          loop vowels has_double (i + 1)
  in
  loop 0 false 0

(** Solve part 1 of the puzzle. *)
let solve_part1 = List.count is_nice >> string_of_int

let is_nice2 s =
  let length = String.length s in
  let table = Hashtbl.create (length - 1) in
  let rec loop has_triple has_repeat i =
    if i >= length then has_triple && has_repeat
    else
      let has_triple = has_triple || (i >= 2 && s.[i] = s.[i - 2]) in
      let has_repeat =
        if has_repeat then true
        else if i = 0 then false
        else
          let p = (s.[i - 1], s.[i]) in
          match Hashtbl.find_opt table p with
          | None ->
              Hashtbl.add table p (i - 1);
              false
          | Some j when j + 3 <= i -> true
          | Some _ -> false
      in
      loop has_triple has_repeat (i + 1)
  in
  loop false false 0

(** Solve part 2 of the puzzle. *)
let solve_part2 = List.count is_nice2 >> string_of_int
