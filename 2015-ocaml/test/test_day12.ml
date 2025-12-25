open OUnit2
open Test_helpers

(** Test input from the puzzle description *)
let test_input = {|TODO: Add example input from puzzle description|}

let test_part1 _ =
  let parsed = Camlukah.Day12.parse_input test_input in
  let result = Camlukah.Day12.solve_part1 parsed in
  (* TODO: Update expected result from puzzle description *)
  assert_string_equal "TODO: expected result" result

let test_part2 _ =
  let parsed = Camlukah.Day12.parse_input test_input in
  let result = Camlukah.Day12.solve_part2 parsed in
  (* TODO: Update expected result from puzzle description *)
  assert_string_equal "TODO: expected result" result

let suite = "Day 12 Tests" >::: []
