(** Download command - downloads puzzle input from Advent of Code *)

open Camlukah

(** Get current year *)
let current_year () =
  let tm = Unix.localtime (Unix.time ()) in
  1900 + tm.Unix.tm_year

(** Parse command line arguments *)
let parse_args args =
  let rec parse day year = function
    | [] -> (day, year)
    | "--year" :: y :: rest -> parse day (Some (int_of_string y)) rest
    | "-y" :: y :: rest -> parse day (Some (int_of_string y)) rest
    | d :: rest when day = None -> parse (Some (int_of_string d)) year rest
    | _ :: rest -> parse day year rest
  in
  parse None None args

(** Main entry point for download command *)
let run args =
  match parse_args args with
  | None, _ ->
      Printf.eprintf "Usage: camlukah download DAY [--year YEAR]\n";
      Printf.eprintf "  DAY  - Day number (1-25)\n";
      Printf.eprintf "  YEAR - Year (optional, defaults to current year)\n";
      Printf.eprintf "\nExamples:\n";
      Printf.eprintf
        "  camlukah download 1              # Download day 1 of current year\n";
      Printf.eprintf
        "  camlukah download 1 --year 2015  # Download day 1 of 2015\n";
      exit 1
  | Some day, year_opt -> (
      let year = Option.value year_opt ~default:(current_year ()) in

      Printf.printf "Downloading Day %d, Year %d\n" day year;
      Printf.printf "===========================\n\n";

      (* Get session cookie *)
      match Session.get_session () with
      | Error msg ->
          Printf.eprintf "Error: %s\n" msg;
          exit 1
      | Ok session -> (
          (* Download input *)
          Printf.printf "Downloading input...\n";
          (match Aoc_client.download_input ~session ~year ~day with
          | Ok input_content -> (
              let filename = Printf.sprintf "inputs/day%02d.txt" day in
              match Input.write_file filename input_content with
              | Ok () -> Printf.printf "✓ Saved input to %s\n\n" filename
              | Error msg -> Printf.eprintf "✗ Failed to save input: %s\n\n" msg
              )
          | Error msg -> Printf.eprintf "✗ Failed to download input: %s\n\n" msg);

          (* Download and display puzzle *)
          Printf.printf "Downloading puzzle description...\n";
          match Aoc_client.download_puzzle ~session ~year ~day with
          | Ok html -> (
              match Html_formatter.format_puzzle_html html with
              | Ok formatted_parts ->
                  Printf.printf "\n";
                  Html_formatter.print_puzzle formatted_parts
              | Error msg ->
                  Printf.eprintf "✗ Failed to format puzzle: %s\n" msg)
          | Error msg -> Printf.eprintf "✗ Failed to download puzzle: %s\n" msg)
      )
