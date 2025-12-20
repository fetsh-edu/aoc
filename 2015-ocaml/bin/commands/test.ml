(** Test command - runs unit tests for days *)

(** Main entry point for test command *)
let run args =
  match args with
  | [] ->
      (* No day specified - run all tests *)
      Printf.printf "Running all tests...\n";
      Printf.printf "==================\n\n";
      let exit_code = Sys.command "dune exec test/test_runner.exe" in
      exit exit_code
  | [ day_str ] ->
      (* Specific day *)
      let day = int_of_string day_str in
      Printf.printf "Running tests for Day %d\n" day;
      Printf.printf "=========================\n\n";
      let cmd =
        Printf.sprintf "CAMLUKAH_TEST_DAY=%d dune exec test/test_runner.exe" day
      in
      let exit_code = Sys.command cmd in
      exit exit_code
  | _ ->
      Printf.eprintf "Usage: camlukah test [DAY]\n";
      Printf.eprintf "  DAY - Day number (optional, runs all if omitted)\n";
      exit 1
