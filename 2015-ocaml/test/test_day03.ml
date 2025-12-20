open OUnit2
open Test_helpers

(** Test input from the puzzle description *)
let test_input = {|TODO: Add example input from puzzle description|}

let test_part1 _ =
  "^>v<" |> Camlukah.Day03.parse_input |> Camlukah.Day03.solve_part1
  |> assert_string_equal "4";

  "^v^v^v^v^v" |> Camlukah.Day03.parse_input |> Camlukah.Day03.solve_part1
  |> assert_string_equal "2"

let test_part2 _ =
  let parsed = Camlukah.Day03.parse_input test_input in
  let result = Camlukah.Day03.solve_part2 parsed in
  (* TODO: Update expected result from puzzle description *)
  assert_string_equal "TODO: expected result" result

let suite =
  "Day 3 Tests"
  >::: [ "test_part1" >:: test_part1; "test_part2" >:: test_part2 ]
