(** Day 3: Perfectly Spherical Houses in a Vacuum *)

type t = string

module Houses = Set.Make (struct
  type t = int * int

  let compare = compare
end)

(** Parse the input string into your input type. The input parameter contains
    the entire puzzle input as a string. *)
let parse_input = Fun.id

let step (x, y) = function
  | '>' -> (x + 1, y)
  | '<' -> (x - 1, y)
  | '^' -> (x, y + 1)
  | 'v' -> (x, y - 1)
  | _ -> (x, y)

(** Solve part 1 of the puzzle. *)
let solve_part1 (input : t) : string =
  let houses, _ =
    input
    |> String.fold_left
         (fun (visited, pos) dir ->
           let new_pos = step pos dir in
           (Houses.add new_pos visited, new_pos))
         (Houses.singleton (0, 0), (0, 0))
  in
  string_of_int (Houses.cardinal houses)

(** Solve part 2 of the puzzle. *)
let solve_part2 (input : t) : string =
  let houses, _, _, _ =
    input
    |> String.fold_left
         (fun (visited, santa_pos, robo_pos, santa_turn) dir ->
           if santa_turn then
             let new_pos = step santa_pos dir in
             (visited |> Houses.add new_pos, new_pos, robo_pos, false)
           else
             let new_pos = step robo_pos dir in
             (visited |> Houses.add new_pos, santa_pos, new_pos, true))
         (Houses.singleton (0, 0), (0, 0), (0, 0), true)
  in
  string_of_int (Houses.cardinal houses)
