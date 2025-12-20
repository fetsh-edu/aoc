open OUnit2
open Test_helpers

(** Test input from the puzzle description *)
let test_input = {|2x3x4
1x1x10
|}

let test_part1 _ =
  let parsed = Camlukah.Day02.parse_input test_input in
  let result = Camlukah.Day02.solve_part1 parsed in
  assert_string_equal "101" result

let test_part2 _ =
  let parsed = Camlukah.Day02.parse_input test_input in
  let result = Camlukah.Day02.solve_part2 parsed in
  assert_string_equal "48" result

let suite =
  "Day 2 Tests"
  >::: [ "test_part1" >:: test_part1; "test_part2" >:: test_part2 ]
