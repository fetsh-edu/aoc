(** HTTP client for Advent of Code API *)

open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix

(** Download puzzle input for a specific day and year *)
let download_input ~session ~year ~day =
  let url =
    Printf.sprintf "https://adventofcode.com/%d/day/%d/input" year day
  in
  let headers =
    Header.init_with "Cookie" (Printf.sprintf "session=%s" session)
  in

  Lwt_main.run
    ( Client.get ~headers (Uri.of_string url) >>= fun (resp, body) ->
      let status = Response.status resp in

      if Code.code_of_status status = 200 then
        Cohttp_lwt.Body.to_string body >|= fun content -> Ok content
      else if Code.code_of_status status = 400 then
        Lwt.return
          (Error "Invalid session cookie. Please check your session token.")
      else if Code.code_of_status status = 404 then
        Lwt.return
          (Error
             (Printf.sprintf
                "Day %d for year %d not found. Puzzle may not be released yet."
                day year))
      else
        Lwt.return
          (Error (Printf.sprintf "HTTP error %d" (Code.code_of_status status)))
    )

(** Download puzzle description HTML for a specific day and year *)
let download_puzzle ~session ~year ~day =
  let url = Printf.sprintf "https://adventofcode.com/%d/day/%d" year day in
  let headers =
    Header.init_with "Cookie" (Printf.sprintf "session=%s" session)
  in

  Lwt_main.run
    ( Client.get ~headers (Uri.of_string url) >>= fun (resp, body) ->
      let status = Response.status resp in

      if Code.code_of_status status = 200 then
        Cohttp_lwt.Body.to_string body >|= fun content -> Ok content
      else if Code.code_of_status status = 400 then
        Lwt.return
          (Error "Invalid session cookie. Please check your session token.")
      else if Code.code_of_status status = 404 then
        Lwt.return
          (Error
             (Printf.sprintf
                "Day %d for year %d not found. Puzzle may not be released yet."
                day year))
      else
        Lwt.return
          (Error (Printf.sprintf "HTTP error %d" (Code.code_of_status status)))
    )
