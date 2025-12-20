(** Convert Advent of Code HTML to formatted terminal output *)

open Soup
open ANSITerminal

type segment = { style : ANSITerminal.style list; text : string }
(** Text segment with style *)

type styled_item = {
  text : string;
  style_fn : string -> segment;
  position : int;
}
(** Styled item with position for inline processing *)

(** Wrap text at given width, preserving word boundaries *)
let wrap_text text width =
  let words = String.split_on_char ' ' text in
  let rec wrap acc current_line current_len = function
    | [] ->
        if current_line = "" then List.rev acc
        else List.rev (current_line :: acc)
    | word :: rest ->
        let word_len = String.length word in
        if current_len = 0 then
          (* First word on line *)
          wrap acc word word_len rest
        else if current_len + 1 + word_len <= width then
          (* Add word to current line *)
          wrap acc (current_line ^ " " ^ word) (current_len + 1 + word_len) rest
        else
          (* Start new line *)
          wrap (current_line :: acc) word word_len rest
  in
  wrap [] "" 0 words |> String.concat "\n"

(** Convert HTML element to styled segments *)
let rec element_to_segments elem =
  match Soup.name elem with
  | "em" -> (
      match Soup.attribute "class" elem with
      | Some "star" ->
          (* Yellow bold star *)
          [
            {
              style = [ Bold; yellow ];
              text = Soup.texts elem |> String.concat "";
            };
          ]
      | _ ->
          (* Bold emphasis *)
          [ { style = [ Bold ]; text = Soup.texts elem |> String.concat "" } ])
  | "code" ->
      (* Green code *)
      [
        { style = [ green; Bold ]; text = Soup.texts elem |> String.concat "" };
      ]
  | "a" ->
      (* Cyan underlined link *)
      [
        {
          style = [ Underlined; cyan ];
          text = Soup.texts elem |> String.concat "";
        };
      ]
  | "h2" ->
      (* Bold cyan heading with separator *)
      let text = Soup.texts elem |> String.concat "" |> String.trim in
      [
        {
          style = [ Bold; cyan ];
          text =
            "\n" ^ text ^ "\n" ^ String.make (String.length text) '=' ^ "\n\n";
        };
      ]
  | "p" ->
      let children_list = Soup.children elem |> Soup.to_list in
      List.concat_map
        (fun node ->
          match Soup.element node with
          | Some el ->
              (* node is an element → recurse *)
              element_to_segments el
          | None ->
              (* node is a text node etc. *)
              [ { style = []; text = Soup.texts node |> String.concat "" } ])
        children_list
      @ [ { style = []; text = "\n\n" } ]
  | "ul" | "ol" ->
      (* Process list items *)
      let items = Soup.children elem |> Soup.elements |> Soup.to_list in
      List.concat_map element_to_segments items
      @ [ { style = []; text = "\n" } ]
  | "li" ->
      (* List item - process inline styling *)
      let children_list = Soup.children elem |> Soup.to_list in
      let children =
        List.concat_map
          (fun node ->
            match Soup.element node with
            | Some el ->
                (* node is an element → recurse *)
                element_to_segments el
            | None ->
                (* node is a text node etc. *)
                [ { style = []; text = Soup.texts node |> String.concat "" } ])
          children_list
      in
      [ { style = []; text = "  • " } ]
      @ children
      @ [ { style = []; text = "\n" } ]
  | "pre" ->
      (* Preformatted code block *)
      [
        {
          style = [ green; Bold ];
          text = "\n" ^ (Soup.texts elem |> String.concat "") ^ "\n";
        };
      ]
  | "article" ->
      (* Process article children *)
      let children_list = Soup.children elem |> Soup.elements |> Soup.to_list in
      List.concat_map element_to_segments children_list
  | _ ->
      (* Default: process children or get text *)
      let children_list = Soup.children elem |> Soup.elements |> Soup.to_list in
      if children_list = [] then
        let text = Soup.texts elem |> String.concat "" in
        if text = "" then [] else [ { style = []; text } ]
      else List.concat_map element_to_segments children_list

(** Extract and format puzzle description from HTML *)
let format_puzzle_html html =
  try
    let soup = Soup.parse html in

    (* Find all article elements with class="day-desc" *)
    let articles = soup $$ "article.day-desc" |> Soup.to_list in

    if articles = [] then Error "Could not find puzzle description in HTML"
    else
      (* Process each article (usually Part 1 and Part 2) *)
      let formatted_parts =
        List.map (fun article -> element_to_segments article) articles
      in
      Ok formatted_parts
  with e ->
    Error (Printf.sprintf "Failed to parse HTML: %s" (Printexc.to_string e))

(** Print segments with proper word wrapping *)
let print_segments (segments : segment list) =
  let max_width = 80 in

  (* Split a segment into word-based sub-segments *)
  let split_segment_into_words (seg : segment) =
    if String.contains seg.text '\n' then
      (* Don't split segments with newlines *)
      [ seg ]
    else
      (* Split on spaces - put space at the START of next word, not end of previous *)
      let rec split_words acc current pos =
        if pos >= String.length seg.text then
          if current = "" then List.rev acc
          else List.rev ({ seg with text = current } :: acc)
        else
          let c = seg.text.[pos] in
          if c = ' ' then
            (* Save current word (if any) and start next word with space *)
            if current = "" then
              (* Multiple spaces or leading space - continue accumulating *)
              split_words acc " " (pos + 1)
            else
              (* Add current word, next word will start with space *)
              split_words ({ seg with text = current } :: acc) " " (pos + 1)
          else split_words acc (current ^ String.make 1 c) (pos + 1)
      in
      split_words [] "" 0
  in

  (* Flatten segments into words *)
  let word_segments = List.concat_map split_segment_into_words segments in

  (* Process word segments, tracking current line length and wrapping when needed *)
  let rec print_with_wrap (segs : segment list) current_line_len =
    match segs with
    | [] -> ()
    | (seg : segment) :: rest ->
        let text = seg.text in

        (* Check if this segment contains newlines - if so, handle specially *)
        if String.contains text '\n' then begin
          ANSITerminal.print_string seg.style text;
          (* Reset line length after newline *)
          let lines = String.split_on_char '\n' text in
          let last_line_len =
            String.length (List.nth lines (List.length lines - 1))
          in
          print_with_wrap rest last_line_len
        end
        else begin
          let seg_len = String.length text in

          (* Check if adding this word would exceed max width *)
          let should_wrap =
            current_line_len > 0
            && current_line_len + seg_len > max_width
            && String.length text > 0
            && text.[0] = ' '
          in

          if should_wrap then begin
            (* Wrap to new line and skip the leading space *)
            print_newline ();
            let trimmed = String.sub text 1 (String.length text - 1) in
            ANSITerminal.print_string seg.style trimmed;
            print_with_wrap rest (String.length trimmed)
          end
          else begin
            (* Print normally *)
            ANSITerminal.print_string seg.style text;
            print_with_wrap rest (current_line_len + seg_len)
          end
        end
  in

  print_with_wrap word_segments 0

(** Print formatted puzzle to terminal with colors *)
let print_puzzle formatted_parts =
  List.iter
    (fun segments ->
      (* Print content with styles and word wrap - h2 headers are already in segments *)
      print_segments segments)
    formatted_parts
