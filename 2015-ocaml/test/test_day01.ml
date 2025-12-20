open OUnit2
open Test_helpers

let test_part1 _ =
  let input = Camlukah.Day01.parse_input "(())" in
  assert_string_equal "0" (Camlukah.Day01.solve_part1 input);

  let input = Camlukah.Day01.parse_input "(((" in
  assert_string_equal "3" (Camlukah.Day01.solve_part1 input)

let test_part2 _ =
  let input = Camlukah.Day01.parse_input ")" in
  assert_string_equal "1" (Camlukah.Day01.solve_part2 input);

  let input = Camlukah.Day01.parse_input "()())" in
  assert_string_equal "5" (Camlukah.Day01.solve_part2 input)

let suite =
  "Day01Tests" >::: [ "test_part1" >:: test_part1; "test_part2" >:: test_part2 ]
