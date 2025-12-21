open OUnit2
open Test_helpers

(** Test input from the puzzle description *)
let test_input =
  {|turn on 0,0 through 999,999
toggle 0,0 through 999,0
turn off 499,499 through 500,500|}

let test_part1 _ =
  let parsed = Camlukah.Day06.parse_input test_input in
  let result = Camlukah.Day06.solve_part1 parsed in
  assert_string_equal "998996" result

let suite = "Day 6 Tests" >::: [ "test_part1" >:: test_part1 ]
