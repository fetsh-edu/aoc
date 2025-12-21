(** Day 9: All in a Single Night *)

open Helpers

type distances = (string * string, int) Hashtbl.t

module CitiesSet = Set.Make (String)

type cities = CitiesSet.t
type t = distances * cities

(** Parse the input string into your input type. The input parameter contains
    the entire puzzle input as a string. *)
let parse_input (input : string) : t =
  let distances = Hashtbl.create 100 in
  let cities =
    input |> String.trim |> String.split_on_char '\n'
    |> List.fold_left
         (fun cities line ->
           let parts = String.split_on_char ' ' line in
           match parts with
           | [ from; _; to_; _; distance ] ->
               let distance = int_of_string distance in
               Hashtbl.add distances (from, to_) distance;
               Hashtbl.add distances (to_, from) distance;
               cities |> CitiesSet.add from |> CitiesSet.add to_
           | _ -> failwith "Invalid input")
         CitiesSet.empty
  in
  (distances, cities)

(** Solve part 1 of the puzzle. *)

let route_distance distances route =
  List.fold_left
    (fun acc (city1, city2) ->
      match Hashtbl.find_opt distances (city1, city2) with
      | Some distance -> acc + distance
      | None -> failwith "Invalid route")
    0 (List.window_by_2 route)

let solve_part1 ((distances, cities) : t) : string =
  CitiesSet.permutations cities
  |> List.map (route_distance distances)
  |> List.min |> string_of_int

(** Solve part 2 of the puzzle. *)
let solve_part2 ((distances, cities) : t) : string =
  CitiesSet.permutations cities
  |> List.map (route_distance distances)
  |> List.max |> string_of_int
