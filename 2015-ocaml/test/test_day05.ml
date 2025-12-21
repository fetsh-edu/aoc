open OUnit2
open Test_helpers

(** Test input from the puzzle description *)
let test_input =
  {|ugknbfddgicrmopn
aaa
jchzalrnumimnmhp
haegwjzuvuyypxyu
dvszwmarrgswjxmb
|}

let test_input2 = {|qjhvhtzxzqqjkmpb
xxyxx
uurcxstgmygtbstg
ieodomkazucvgmuy
|}

let test_part1 _ =
  let parsed = Camlukah.Day05.parse_input test_input in
  let result = Camlukah.Day05.solve_part1 parsed in
  assert_string_equal "2" result

let test_part2 _ =
  let parsed = Camlukah.Day05.parse_input test_input2 in
  let result = Camlukah.Day05.solve_part2 parsed in
  assert_string_equal "2" result

let suite =
  "Day 5 Tests"
  >::: [ "test_part1" >:: test_part1; "test_part2" >:: test_part2 ]
