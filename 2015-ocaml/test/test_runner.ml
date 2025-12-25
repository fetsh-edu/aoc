(** Test runner - dispatcher for day tests *)

open OUnit2

(** Get test suite for a specific day *)
let get_day_suite day =
  match day with
  | 1 -> Some Test_day01.suite
  | 2 -> Some Test_day02.suite
  | 3 -> Some Test_day03.suite
  | 5 -> Some Test_day05.suite
  | 6 -> Some Test_day06.suite
  | 7 -> Some Test_day07.suite
  | 8 -> Some Test_day08.suite
  | 9 -> Some Test_day09.suite
  | 10 -> Some Test_day10.suite
  | 11 -> Some Test_day11.suite
  | 12 -> Some Test_day12.suite
  | _ -> None

(** Get all test suites *)
let all_suites () = [ Test_day12.suite; Test_day11.suite; Test_day10.suite; Test_day09.suite; Test_day08.suite; Test_day07.suite; Test_day06.suite; Test_day05.suite; Test_day03.suite; Test_day01.suite; Test_day02.suite ]

(** Main entry point *)
let () =
  (* Check environment variable for day filter *)
  let day_filter =
    try Some (int_of_string (Sys.getenv "CAMLUKAH_TEST_DAY")) with _ -> None
  in

  match day_filter with
  | None ->
      (* No filter - run all tests *)
      let combined_suite = "All Days" >::: all_suites () in
      run_test_tt_main combined_suite
  | Some day -> (
      (* Specific day from environment *)
      match get_day_suite day with
      | None ->
          Printf.eprintf "Error: No tests found for day %d\n" day;
          exit 1
      | Some suite -> run_test_tt_main suite)
