open OUnit2
open Test_helpers

let test_part1 _ =
  "abcdefgh" |> Camlukah.Day11.parse_input |> Camlukah.Day11.solve_part1
  |> assert_string_equal "abcdffaa";

  "ghijklmn" |> Camlukah.Day11.parse_input |> Camlukah.Day11.solve_part1
  |> assert_string_equal "ghjaabcc"

let suite = "Day 11 Tests" >::: [ "test_part1" >:: test_part1 ]
