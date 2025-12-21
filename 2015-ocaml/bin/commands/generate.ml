(** Generate command - scaffolds a new day's solution *)

(** Template for a day's solution file *)
let day_template day =
  Printf.sprintf
    {|(** Day %d: TODO - Add puzzle title *)

open Helpers

(** TODO: Define your input type.
    For example:
    - type t = int list            (list of numbers)
    - type t = string list         (list of lines)
    - type t = char array array    (2D grid)
    - type t = (int * int) list    (list of pairs)
*)
type t = unit

(** Parse the input string into your input type.
    The input parameter contains the entire puzzle input as a string. *)
let parse_input (_input : string) : t =
  (* TODO: Implement parsing logic *)
  (* Example for parsing lines:
     String.split_on_char '\n' input
     |> List.filter (fun line -> String.trim line <> "")
  *)
  todo ()

(** Solve part 1 of the puzzle. *)
let solve_part1 (_input : t) : string =
  (* TODO: Implement part 1 solution *)
  todo ()

(** Solve part 2 of the puzzle. *)
let solve_part2 (_input : t) : string =
  (* TODO: Implement part 2 solution *)
  todo ()
|}
    day

(** Template for a day's test file *)
let test_template day =
  Printf.sprintf
    {xxx|open OUnit2
open Test_helpers

(** Test input from the puzzle description *)
let test_input = {|TODO: Add example input from puzzle description|}

let test_part1 _ =
  let parsed = Camlukah.Day%02d.parse_input test_input in
  let result = Camlukah.Day%02d.solve_part1 parsed in
  (* TODO: Update expected result from puzzle description *)
  assert_string_equal "TODO: expected result" result

let test_part2 _ =
  let parsed = Camlukah.Day%02d.parse_input test_input in
  let result = Camlukah.Day%02d.solve_part2 parsed in
  (* TODO: Update expected result from puzzle description *)
  assert_string_equal "TODO: expected result" result

let suite =
  "Day %d Tests" >::: [
    "test_part1" >:: test_part1;
    "test_part2" >:: test_part2;
  ]
|xxx}
    day day day day day

(** Write content to a file *)
let write_file filename content =
  try
    let oc = open_out filename in
    output_string oc content;
    close_out oc;
    Ok ()
  with Sys_error msg -> Error msg

(** Create a file with given content, checking if it already exists *)
let create_file filename content =
  if Sys.file_exists filename then
    Error (Printf.sprintf "File already exists: %s" filename)
  else write_file filename content

(** Helper to check if string contains substring *)
let contains_substring haystack needle =
  try
    let len = String.length needle in
    let rec check i =
      if i + len > String.length haystack then false
      else if String.sub haystack i len = needle then true
      else check (i + 1)
    in
    check 0
  with _ -> false

(** Add a new day to solve.ml's get_solver function *)
let add_day_to_solver day =
  let solve_file = "bin/commands/solve.ml" in

  try
    (* Read the file *)
    let content = Camlukah.Input.read_file solve_file in

    (* Check if day is already registered *)
    let day_module = Printf.sprintf "Day%02d" day in
    if contains_substring content day_module then
      Error (Printf.sprintf "Day %d is already registered in %s" day solve_file)
    else
      (* Pattern to find and replace - formatted version is single line *)
      let wildcard_pattern =
        "  | _ -> Error (Printf.sprintf \"Day %d is not implemented yet\" day)"
      in

      (* Build the new case - keep single-line format *)
      let new_case =
        Printf.sprintf
          "  | %d ->\n\
          \      let module Solver = Day%02d in\n\
          \      Ok (module Solver : Day.Solver)\n\
          \  %s"
          day day wildcard_pattern
      in

      (* Use Str module for replacement *)
      let regexp = Str.regexp_string wildcard_pattern in
      let updated_content = Str.replace_first regexp new_case content in

      write_file solve_file updated_content
  with Sys_error msg ->
    Error (Printf.sprintf "Failed to update %s: %s" solve_file msg)

(** Add a new day to test_runner.ml *)
let add_day_to_test_runner day =
  let test_runner_file = "test/test_runner.ml" in

  try
    let content = Camlukah.Input.read_file test_runner_file in

    (* Check if day is already registered *)
    let test_module = Printf.sprintf "Test_day%02d" day in
    if contains_substring content test_module then
      Error
        (Printf.sprintf "Day %d is already registered in %s" day
           test_runner_file)
    else
      (* Update get_day_suite function *)
      (* Use the function comment as an anchor to find the right wildcard *)
      let suite_marker = "(** Get test suite for a specific day *)" in

      (* Split content at the marker *)
      let marker_regexp = Str.regexp_string suite_marker in
      let marker_pos =
        try Str.search_forward marker_regexp content 0
        with Not_found ->
          failwith "Could not find get_day_suite function marker"
      in

      let before_marker = String.sub content 0 marker_pos in
      let after_marker = String.sub content marker_pos (String.length content - marker_pos) in

      (* Try to find empty pattern first (no pipe) *)
      let empty_wildcard = "match day with _ -> None" in
      let empty_regexp = Str.regexp_string empty_wildcard in
      let has_empty_pattern =
        try
          ignore (Str.search_forward empty_regexp after_marker 0);
          true
        with Not_found -> false
      in

      let updated_after =
        if has_empty_pattern then
          (* Empty case - replace "match day with _ -> None" *)
          let new_match =
            Printf.sprintf "match day with\n  | %d -> Some Test_day%02d.suite\n  | _ -> None" day day
          in
          Str.replace_first empty_regexp new_match after_marker
        else
          (* Non-empty case - replace "| _ -> None" *)
          let wildcard_pattern = "| _ -> None" in
          let wildcard_regexp = Str.regexp_string wildcard_pattern in
          let new_case =
            Printf.sprintf "| %d -> Some Test_day%02d.suite\n  | _ -> None" day day
          in
          Str.replace_first wildcard_regexp new_case after_marker
      in

      let content = before_marker ^ updated_after in

      (* Update all_suites function - formatted version uses [] instead of [\n  ] *)
      let updated_content =
        (* First try to match empty list pattern "[]" *)
        let empty_list_pattern = "let all_suites () = []" in
        let regexp_empty = Str.regexp_string empty_list_pattern in
        let has_empty_list =
          try
            ignore (Str.search_forward regexp_empty content 0);
            true
          with Not_found -> false
        in
        if has_empty_list then
          (* Empty list - replace with single item *)
          let new_list =
            Printf.sprintf "let all_suites () = [ Test_day%02d.suite ]" day
          in
          Str.replace_first regexp_empty new_list content
        else
          (* Non-empty list - add to beginning *)
          let non_empty_pattern = "let all_suites () = [" in
          let regexp_non_empty = Str.regexp_string non_empty_pattern in
          let new_beginning =
            Printf.sprintf "let all_suites () = [ Test_day%02d.suite;" day
          in
          Str.replace_first regexp_non_empty new_beginning content
      in

      write_file test_runner_file updated_content
  with Sys_error msg ->
    Error (Printf.sprintf "Failed to update %s: %s" test_runner_file msg)

(** Main entry point for generate command *)
let run args =
  match args with
  | [ day_str ] ->
      let day = int_of_string day_str in
      Printf.printf "Generating scaffold for Day %d\n\n" day;

      (* Generate file paths *)
      let day_file = Printf.sprintf "lib/days/day%02d.ml" day in
      let test_file = Printf.sprintf "test/test_day%02d.ml" day in
      (* Create day solution file *)
      (match create_file day_file (day_template day) with
      | Ok () -> Printf.printf "✓ Created %s\n" day_file
      | Error msg -> Printf.eprintf "✗ Error creating %s: %s\n" day_file msg);

      (* Create test file *)
      (match create_file test_file (test_template day) with
      | Ok () -> Printf.printf "✓ Created %s\n" test_file
      | Error msg -> Printf.eprintf "✗ Error creating %s: %s\n" test_file msg);

      (* Automatically add day to solve.ml *)
      Printf.printf "\n";
      (match add_day_to_solver day with
      | Ok () -> Printf.printf "✓ Added Day%02d to bin/commands/solve.ml\n" day
      | Error msg -> Printf.eprintf "✗ Error updating solve.ml: %s\n" msg);

      (* Automatically add day to test_runner.ml *)
      (match add_day_to_test_runner day with
      | Ok () -> Printf.printf "✓ Added Day%02d to test/test_runner.ml\n" day
      | Error msg -> Printf.eprintf "✗ Error updating test_runner.ml: %s\n" msg);

      Printf.printf "\n✅ Done! Run 'dune build' to check for errors\n"
  | _ ->
      Printf.eprintf "Usage: camlukah generate DAY\n";
      Printf.eprintf "  DAY - Day number (1-25)\n";
      exit 1
