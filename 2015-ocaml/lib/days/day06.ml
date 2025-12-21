(** Day 6: Probably a Fire Hazard *)

open Helpers
open Re

type op = On | Off | Toggle
type instruction = { op : op; x1 : int; y1 : int; x2 : int; y2 : int }
type t = instruction list

let re =
  let op = alt [ str "turn on"; str "turn off"; str "toggle" ] in
  seq
    [
      group op;
      char ' ';
      group (rep1 digit);
      char ',';
      group (rep1 digit);
      str " through ";
      group (rep1 digit);
      char ',';
      group (rep1 digit);
    ]
  |> compile

let parse_instruction (line : string) : instruction =
  match exec_opt re line with
  | None -> failwith ("Invalid instruction: " ^ line)
  | Some groups ->
      let op =
        match Group.get groups 1 with
        | "turn on" -> On
        | "turn off" -> Off
        | "toggle" -> Toggle
        | _ -> failwith "Invalid operation"
      in
      let x1 = int_of_string (Group.get groups 2) in
      let y1 = int_of_string (Group.get groups 3) in
      let x2 = int_of_string (Group.get groups 4) in
      let y2 = int_of_string (Group.get groups 5) in
      { op; x1; y1; x2; y2 }

(** Parse the input string into your input type. The input parameter contains
    the entire puzzle input as a string. *)
let parse_input (_input : string) : t =
  _input |> String.trim |> String.split_on_char '\n'
  |> List.map parse_instruction

(** Solve part 1 of the puzzle. *)
let solve_part1 (instructions : t) : string =
  let grid = Array.make_matrix 1000 1000 false in
  List.iter
    (fun { op; x1; y1; x2; y2 } ->
      for i = x1 to x2 do
        for j = y1 to y2 do
          match op with
          | On -> grid.(i).(j) <- true
          | Off -> grid.(i).(j) <- false
          | Toggle -> grid.(i).(j) <- not grid.(i).(j)
        done
      done)
    instructions;

  grid
  |> Array.fold_left (fun acc row -> acc + Array.count Fun.id row) 0
  |> string_of_int

(** Solve part 2 of the puzzle. *)
let solve_part2 (instructions : t) : string =
  let grid = Array.make_matrix 1000 1000 0 in
  List.iter
    (fun { op; x1; y1; x2; y2 } ->
      for i = x1 to x2 do
        for j = y1 to y2 do
          match op with
          | On -> grid.(i).(j) <- grid.(i).(j) + 1
          | Off -> grid.(i).(j) <- max 0 (grid.(i).(j) - 1)
          | Toggle -> grid.(i).(j) <- grid.(i).(j) + 2
        done
      done)
    instructions;

  grid
  |> Array.fold_left (fun acc row -> acc + Array.fold_left ( + ) 0 row) 0
  |> string_of_int
