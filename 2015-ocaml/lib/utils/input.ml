(** Utilities for reading puzzle input files. *)

(** [read_file filename] reads the entire contents of a file as a string. Raises
    [Sys_error] if the file cannot be read. *)
let read_file filename =
  let channel = open_in filename in
  let length = in_channel_length channel in
  let content = really_input_string channel length in
  close_in channel;
  content

(** [read_day_input day] reads the input file for the given day number. Input
    files are expected to be in the 'inputs/' directory with format 'dayXX.txt'.
    Returns [Ok content] on success, [Error message] on failure. *)
let read_day_input day =
  let filename = Printf.sprintf "inputs/day%02d.txt" day in
  try Ok (read_file filename)
  with Sys_error msg ->
    Error (Printf.sprintf "Failed to read input file: %s" msg)

(** [write_file filename content] writes content to a file. Returns [Ok ()] on
    success, [Error message] on failure. *)
let write_file filename content =
  try
    let channel = open_out filename in
    output_string channel content;
    close_out channel;
    Ok ()
  with Sys_error msg -> Error (Printf.sprintf "Failed to write file: %s" msg)
