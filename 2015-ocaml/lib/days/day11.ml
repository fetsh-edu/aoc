(** Day 11: Corporate Policy *)

open Helpers

type t = bytes

let parse_input = String.trim >> Bytes.of_string

(** Solve part 1 of the puzzle. *)

let has_straight password =
  let rec loop i =
    if i + 2 >= Bytes.length password then false
    else
      let a = Bytes.get_uint8 password i in
      let b = Bytes.get_uint8 password (i + 1) in
      let c = Bytes.get_uint8 password (i + 2) in
      if b = a + 1 && c = b + 1 then true else loop (i + 1)
  in
  loop 0

let has_two_pairs password =
  let rec loop i seen =
    if i + 1 >= Bytes.length password then List.length seen >= 2
    else
      let a = Bytes.get password i in
      let b = Bytes.get password (i + 1) in
      if a = b && not (List.mem a seen) then loop (i + 2) (a :: seen)
      else loop (i + 1) seen
  in
  loop 0 []

let is_valid password = has_straight password && has_two_pairs password
let is_forbidden = function 'i' | 'o' | 'l' -> true | _ -> false

let next_allowed (c : char) : char =
  let rec loop c =
    let c' = Char.chr (Char.code c + 1) in
    if is_forbidden c' then loop c' else c'
  in
  loop c

let bump password i =
  Bytes.set password i (next_allowed (Bytes.get password i));
  for j = i + 1 to Bytes.length password - 1 do
    Bytes.set password j 'a'
  done

let sanitize password =
  let len = Bytes.length password in
  let rec loop i =
    if i >= len then password
    else if is_forbidden (Bytes.get password i) then begin
      bump password i;
      password
    end
    else loop (i + 1)
  in
  loop 0

let increment password =
  let rec carry i =
    if i < 0 then failwith "Password overflow";
    let c = Bytes.get password i in
    if c = 'z' then (
      Bytes.set password i 'a';
      carry (i - 1))
    else (
      Bytes.set password i (Char.chr (Char.code c + 1));
      if is_forbidden (Bytes.get password i) then bump password i)
  in
  carry (Bytes.length password - 1);
  password

let next_valid_password password =
  let password = sanitize password in
  let rec loop () =
    increment password |> ignore;
    if is_valid password then password else loop ()
  in
  loop ()

let solve_part1 (input : t) : string =
  next_valid_password (Bytes.copy input) |> Bytes.to_string

(** Solve part 2 of the puzzle. *)
let solve_part2 (input : t) : string =
  next_valid_password (Bytes.copy input)
  |> next_valid_password |> Bytes.to_string
