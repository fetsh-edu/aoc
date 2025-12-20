(** Session cookie management for Advent of Code *)

let session_file = ".aoc-session"

(** Read session cookie from file *)
let read_session () =
  try
    let ic = open_in session_file in
    let session = input_line ic in
    close_in ic;
    Some (String.trim session)
  with Sys_error _ | End_of_file -> None

(** Save session cookie to file *)
let save_session session =
  try
    let oc = open_out session_file in
    output_string oc (String.trim session);
    output_char oc '\n';
    close_out oc;
    Ok ()
  with Sys_error msg ->
    Error (Printf.sprintf "Failed to save session: %s" msg)

(** Prompt user for session cookie *)
let prompt_for_session () =
  Printf.printf "\n";
  Printf.printf
    "To download puzzle inputs, you need your Advent of Code session cookie.\n";
  Printf.printf "\n";
  Printf.printf "How to get it:\n";
  Printf.printf "  1. Go to https://adventofcode.com and log in\n";
  Printf.printf "  2. Open browser DevTools (F12)\n";
  Printf.printf "  3. Go to Application/Storage → Cookies\n";
  Printf.printf "  4. Copy the value of the 'session' cookie\n";
  Printf.printf "\n";
  Printf.printf "Enter your session cookie: ";
  flush stdout;
  try
    let session = read_line () in
    let trimmed = String.trim session in
    if trimmed = "" then Error "Session cookie cannot be empty"
    else
      match save_session trimmed with
      | Ok () ->
          Printf.printf "✓ Session saved to %s\n\n" session_file;
          Ok trimmed
      | Error msg -> Error msg
  with End_of_file -> Error "No input provided"

(** Get session cookie (from file or prompt user) *)
let get_session () =
  match read_session () with
  | Some session -> Ok session
  | None -> prompt_for_session ()
