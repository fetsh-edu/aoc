open OUnit2
open Test_helpers

(** Test input from the puzzle description *)
let test_input =
  {|123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i|}

let test_part1 _ =
  let parsed = Camlukah.Day07.parse_input test_input in
  let result = Camlukah.Day07.solve_part1 parsed in
  (* TODO: Update expected result from puzzle description *)
  assert_string_equal "TODO: expected result" result

let test_part2 _ =
  let parsed = Camlukah.Day07.parse_input test_input in
  let result = Camlukah.Day07.solve_part2 parsed in
  (* TODO: Update expected result from puzzle description *)
  assert_string_equal "TODO: expected result" result

let suite = "Day 7 Tests" >::: []
(* >::: [ "test_part1" >:: test_part1; "test_part2" >:: test_part2 ] *)
