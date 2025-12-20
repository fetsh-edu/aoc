(** Advent of Code CLI - main entry point *)

(** Print usage information *)
let print_usage () =
  Printf.printf "Usage: camlukah COMMAND [OPTIONS]\n\n";
  Printf.printf "Commands:\n";
  Printf.printf
    "  solve DAY [PART]   Solve a specific day (and optionally part)\n";
  Printf.printf "  test DAY           Run solution on test input\n";
  Printf.printf "  generate DAY       Generate scaffold for a new day\n";
  Printf.printf "  download DAY       Download puzzle input\n";
  Printf.printf "\nExamples:\n";
  Printf.printf "  camlukah solve 1        # Run both parts of day 1\n";
  Printf.printf "  camlukah solve 1 1      # Run only part 1 of day 1\n";
  Printf.printf "  camlukah test 1         # Test day 1\n";
  Printf.printf "  camlukah generate 2     # Create scaffold for day 2\n";
  Printf.printf "  camlukah download 1     # Download day 1 input\n"

(** Main entry point - dispatch to subcommands *)
let () =
  let args = Sys.argv in
  match Array.to_list args with
  | [ _ ] ->
      (* No arguments *)
      print_usage ()
  | _ :: "solve" :: rest -> Solve.run rest
  | _ :: "test" :: rest -> Test.run rest
  | _ :: "generate" :: rest -> Generate.run rest
  | _ :: "download" :: rest -> Download.run rest
  | _ :: cmd :: _ ->
      Printf.eprintf "Error: Unknown command '%s'\n\n" cmd;
      print_usage ();
      exit 1
  | _ ->
      print_usage ();
      exit 1
