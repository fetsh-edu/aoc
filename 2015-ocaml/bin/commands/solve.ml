(** Solve command - runs solutions for Advent of Code days *)

open Camlukah

(** Get the solver module for a specific day *)
let get_solver day =
  match day with
  | 1 ->
      let module Solver = Day01 in
      Ok (module Solver : Day.Solver)
  | 2 ->
      let module Solver = Day02 in
      Ok (module Solver : Day.Solver)
  | 3 ->
      let module Solver = Day03 in
      Ok (module Solver : Day.Solver)
  | 4 ->
      let module Solver = Day04 in
      Ok (module Solver : Day.Solver)
  | 5 ->
      let module Solver = Day05 in
      Ok (module Solver : Day.Solver)
    | 6 ->
      let module Solver = Day06 in
      Ok (module Solver : Day.Solver)
    | _ -> Error (Printf.sprintf "Day %d is not implemented yet" day)

(** Run a specific part of a day with a given solver module *)
let run_part (module Solver : Day.Solver) part input_content =
  let parsed_input = Solver.parse_input input_content in
  let result =
    match part with
    | 1 -> Solver.solve_part1 parsed_input
    | 2 -> Solver.solve_part2 parsed_input
    | _ -> failwith "Invalid part number"
  in
  Printf.printf "Part %d: %s\n" part result

(** Run both parts of a day with a given solver module *)
let run_both_parts (module Solver : Day.Solver) input_content =
  (* Parse input once and use for both parts *)
  let parsed_input = Solver.parse_input input_content in
  let part1_result = Solver.solve_part1 parsed_input in
  let part2_result = Solver.solve_part2 parsed_input in
  Printf.printf "Part 1: %s\n" part1_result;
  Printf.printf "Part 2: %s\n" part2_result

(** Main entry point for solve command *)
let run args =
  match args with
  | [ day_str ] -> (
      (* Only day provided, run both parts *)
      let day = int_of_string day_str in
      (* Check if day is implemented first *)
      match get_solver day with
      | Error msg ->
          Printf.eprintf "Error: %s\n" msg;
          exit 1
      | Ok solver_module -> (
          Printf.printf "Running Day %d\n" day;
          Printf.printf "==============\n";
          match Input.read_day_input day with
          | Error msg ->
              Printf.eprintf "Error: %s\n" msg;
              exit 1
          | Ok content -> run_both_parts solver_module content))
  | [ day_str; part_str ] -> (
      (* Both day and part provided *)
      let day = int_of_string day_str in
      let part = int_of_string part_str in
      (* Check if day is implemented first *)
      match get_solver day with
      | Error msg ->
          Printf.eprintf "Error: %s\n" msg;
          exit 1
      | Ok solver_module -> (
          Printf.printf "Running Day %d, Part %d\n" day part;
          Printf.printf "========================\n";
          match Input.read_day_input day with
          | Error msg ->
              Printf.eprintf "Error: %s\n" msg;
              exit 1
          | Ok content -> run_part solver_module part content))
  | _ ->
      Printf.eprintf "Usage: camlukah solve DAY [PART]\n";
      Printf.eprintf "  DAY  - Day number (1-25)\n";
      Printf.eprintf "  PART - Part number (1 or 2, optional)\n";
      exit 1
