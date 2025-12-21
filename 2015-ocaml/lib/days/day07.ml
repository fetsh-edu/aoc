(** Day 7: TODO - Add puzzle title *)

open Helpers

type operand = Const of int | Wire of string

type instr =
  | Assign of operand
  | Not of operand
  | And of operand * operand
  | Or of operand * operand
  | LShift of operand * int
  | RShift of operand * int

module StringMap = Map.Make (String)
module StringTable = Hashtbl.Make (String)

type circuit = instr StringMap.t
type t = circuit

let mask16 x = x land 0xFFFF

(** Parse the input string into your input type. The input parameter contains
    the entire puzzle input as a string. *)
let parse_input (input : string) : t =
  let parse_operand s =
    match int_of_string_opt s with Some n -> Const n | None -> Wire s
  in
  let parse_line line =
    match String.split_on_char ' ' line with
    | [ op; "->"; dst ] -> (dst, Assign (parse_operand op))
    | [ "NOT"; op; "->"; dst ] -> (dst, Not (parse_operand op))
    | [ op1; "AND"; op2; "->"; dst ] ->
        (dst, And (parse_operand op1, parse_operand op2))
    | [ op1; "OR"; op2; "->"; dst ] ->
        (dst, Or (parse_operand op1, parse_operand op2))
    | [ op; "LSHIFT"; n; "->"; dst ] ->
        (dst, LShift (parse_operand op, int_of_string n))
    | [ op; "RSHIFT"; n; "->"; dst ] ->
        (dst, RShift (parse_operand op, int_of_string n))
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

let rec eval_wire (circuit : circuit) (memo : int StringTable.t) wire =
  match StringTable.find_opt memo wire with
  | Some value -> value
  | None -> (
      match StringMap.find_opt wire circuit with
      | None -> failwith "Wire not found"
      | Some instruction ->
          let value = eval_instruction circuit memo instruction in
          StringTable.add memo wire value;
          value)

and eval_instruction (circuit : circuit) (memo : int StringTable.t) = function
  | Assign op -> eval_operand circuit memo op |> mask16
  | Not op ->
      let x = eval_operand circuit memo op in
      mask16 (lnot x)
  | And (op1, op2) ->
      let x = eval_operand circuit memo op1 in
      let y = eval_operand circuit memo op2 in
      mask16 (x land y)
  | Or (op1, op2) ->
      let x = eval_operand circuit memo op1 in
      let y = eval_operand circuit memo op2 in
      mask16 (x lor y)
  | LShift (op1, n) ->
      let x = eval_operand circuit memo op1 in
      mask16 (x lsl n)
  | RShift (op1, n) ->
      let x = eval_operand circuit memo op1 in
      mask16 (x lsr n)

and eval_operand (circuit : circuit) (memo : int StringTable.t) = function
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
