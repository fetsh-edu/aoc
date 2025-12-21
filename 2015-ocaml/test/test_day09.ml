open OUnit2
open Test_helpers

(** Test input from the puzzle description *)
let test_input =
  {|London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141|}

let test_part1 _ =
  let parsed = Camlukah.Day09.parse_input test_input in
  let result = Camlukah.Day09.solve_part1 parsed in
  assert_string_equal "605" result

let test_part2 _ =
  let parsed = Camlukah.Day09.parse_input test_input in
  let result = Camlukah.Day09.solve_part2 parsed in
  assert_string_equal "982" result

let suite =
  "Day 9 Tests"
  >::: [ "test_part1" >:: test_part1; "test_part2" >:: test_part2 ]
