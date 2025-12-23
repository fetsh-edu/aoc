open OUnit2
open Test_helpers

let test_part1 _ =
  "1" |> Camlukah.Day10.parse_input |> Camlukah.Day10.run 5
  |> assert_string_equal "312211"

let suite = "Day 10 Tests" >::: [ "test_part1" >:: test_part1 ]
