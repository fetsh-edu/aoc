(** Day 7: Some Assembly Required *)

open Helpers

type op = Const of int | Wire of string

type instr =
  | Assign of op
  | Not of op
  | And of op * op
  | Or of op * op
  | LShift of op * int
  | RShift of op * int

module StringMap = Map.Make (String)
module StringTable = Hashtbl.Make (String)

type circuit = instr StringMap.t
type t = circuit

let mask16 x = x land 0xFFFF

(** Parse the input string into your input type. The input parameter contains
    the entire puzzle input as a string. *)
let parse_input (input : string) : t =
  let parse_op s =
    match int_of_string_opt s with Some n -> Const n | None -> Wire s
  in
  let parse_line line =
    match String.split_on_char ' ' line with
    | [ op; "->"; dst ] -> (dst, Assign (parse_op op))
    | [ "NOT"; op; "->"; dst ] -> (dst, Not (parse_op op))
    | [ op1; "AND"; op2; "->"; dst ] -> (dst, And (parse_op op1, parse_op op2))
    | [ op1; "OR"; op2; "->"; dst ] -> (dst, Or (parse_op op1, parse_op op2))
    | [ op; "LSHIFT"; n; "->"; dst ] ->
        (dst, LShift (parse_op op, int_of_string n))
    | [ op; "RSHIFT"; n; "->"; dst ] ->
        (dst, RShift (parse_op op, int_of_string n))
    | _ -> failwith ("Invalid line: " ^ line)
  in
  input |> String.split_on_char '\n'
  |> List.fold_left
       (fun acc line ->
         if String.trim line = "" then acc
         else
           let dest, ins = parse_line line in
           StringMap.add dest ins acc)
       StringMap.empty

let rec eval_wire circuit memo wire =
  match StringTable.find_opt memo wire with
  | Some value -> value
  | None -> (
      match StringMap.find_opt wire circuit with
      | None -> failwith "Wire not found"
      | Some instruction ->
          let value = eval_instruction circuit memo instruction in
          StringTable.add memo wire value;
          value)

and eval_instruction circuit memo = function
  | Assign op -> eval_op circuit memo op |> mask16
  | Not op -> lnot (eval_op circuit memo op) |> mask16
  | And (op1, op2) ->
      eval_op circuit memo op1 land eval_op circuit memo op2 |> mask16
  | Or (op1, op2) ->
      eval_op circuit memo op1 lor eval_op circuit memo op2 |> mask16
  | LShift (op1, n) -> eval_op circuit memo op1 lsl n |> mask16
  | RShift (op1, n) -> eval_op circuit memo op1 lsr n |> mask16

and eval_op circuit memo = function
  | Const n -> n
  | Wire w -> eval_wire circuit memo w

(** Solve part 1 of the puzzle. *)
let solve_part1 (circuit : t) : string =
  eval_wire circuit (StringTable.create 128) "a" |> string_of_int

(** Solve part 2 of the puzzle. *)
let solve_part2 (circuit : t) : string =
  let a = eval_wire circuit (StringTable.create 128) "a" in
  eval_wire
    (circuit |> StringMap.add "b" (Assign (Const a)))
    (StringTable.create 128) "a"
  |> string_of_int
