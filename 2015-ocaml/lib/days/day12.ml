(** Day 12: JSAbacusFramework.io *)

open Helpers

type t = string

let parse_input = String.trim
let zero_code = Char.code '0'

let count_numbers string =
  let length = String.length string in
  let rec loop index sign sum num =
    if index = length then sum + (sign * num)
    else
      match string.[index] with
      | '0' .. '9' as char ->
          loop (index + 1) sign sum ((num * 10) + Char.code char - zero_code)
      | '-' -> loop (index + 1) (-1) sum 0
      | _ -> loop (index + 1) 1 (sum + (sign * num)) 0
  in
  loop 0 1 0 0

type frame =
  | Arr of { sum : int }
  | Obj of { sum : int; red : bool; in_value : bool }

let count_not_red (string : string) : int =
  let length = String.length string in

  let parse_int index =
    let sign, index =
      if string.[index] = '-' then (-1, index + 1) else (1, index)
    in
    let rec loop index num =
      if index = length then (index, num * sign)
      else
        match string.[index] with
        | '0' .. '9' as char ->
            loop (index + 1) ((num * 10) + Char.code char - zero_code)
        | _ -> (index, num * sign)
    in
    loop index 0
  in

  let rec go index stack sum =
    if index = length then sum
    else
      match string.[index] with
      | '{' ->
          go (index + 1)
            (Obj { sum = 0; red = false; in_value = false } :: stack)
            sum
      | '[' -> go (index + 1) (Arr { sum = 0 } :: stack) sum
      | ':' -> begin
          match stack with
          | Obj o :: rest ->
              go (index + 1) (Obj { o with in_value = true } :: rest) sum
          | _ -> go (index + 1) stack sum
        end
      | '"' ->
          let next_index, is_exact_red =
            let rec scan j =
              if j = length then (j, false)
              else if string.[j] = '"' then
                if
                  j - (index + 1) = 3
                  && string.[index + 1] = 'r'
                  && string.[index + 2] = 'e'
                  && string.[index + 3] = 'd'
                then (j + 1, true)
                else (j + 1, false)
              else scan (j + 1)
            in
            scan (index + 1)
          in
          let stack =
            match stack with
            | Obj o :: rest when o.in_value ->
                Obj { o with red = o.red || is_exact_red; in_value = false }
                :: rest
            | _ -> stack
          in
          go next_index stack sum
      | '-' | '0' .. '9' ->
          let next_index, int_value = parse_int index in
          let stack =
            match stack with
            | Arr { sum } :: stack -> Arr { sum = sum + int_value } :: stack
            | Obj o :: stack ->
                Obj { o with sum = o.sum + int_value; in_value = false }
                :: stack
            | _ -> stack
          in
          go next_index stack sum
      | '}' | ']' ->
          let contrib, rest =
            match stack with
            | Arr { sum = s } :: tl -> (s, tl)
            | Obj o :: tl -> ((if o.red then 0 else o.sum), tl)
            | [] -> failwith "Unexpected end of input"
          in
          begin match rest with
          | [] -> go (index + 1) [] (sum + contrib)
          | Arr { sum = s } :: tl ->
              go (index + 1) (Arr { sum = s + contrib } :: tl) sum
          | Obj o :: tl ->
              go (index + 1)
                (Obj { o with sum = o.sum + contrib; in_value = false } :: tl)
                sum
          end
      | _ -> go (index + 1) stack sum
  in
  go 0 [] 0

(** Solve part 1 of the puzzle. *)
let solve_part1 = count_numbers >> string_of_int

(** Solve part 2 of the puzzle. *)
let solve_part2 = count_not_red >> string_of_int
