open OUnit2
open Test_helpers

(** Test input from the puzzle description *)
let test_input = {|""
"abc"
"aaa\"aaa"
"\x27"|}

let test_part1 _ =
  let parsed = Camlukah.Day08.parse_input test_input in
  let result = Camlukah.Day08.solve_part1 parsed in
  assert_string_equal "12" result

let test_part2 _ =
  let parsed = Camlukah.Day08.parse_input test_input in
  let result = Camlukah.Day08.solve_part2 parsed in
  assert_string_equal "19" result

let suite =
  "Day 8 Tests"
  >::: [ "test_part1" >:: test_part1; "test_part2" >:: test_part2 ]
