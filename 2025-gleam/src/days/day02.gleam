import gleam/bool
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import utils/input

pub fn solve() -> Result(#(String, String), String) {
  use input <- result.try(input.read_input(2))

  let ranges = prepare_input(input)
  let part1 = solve_part1(ranges)
  let part2 = solve_part2(ranges)

  Ok(#(int.to_string(part1), int.to_string(part2)))
}

pub fn prepare_input(input: String) -> List(Int) {
  input
  |> string.trim
  |> string.split(",")
  |> list.flat_map(parse_range)
}

pub fn solve_part1(input: List(Int)) -> Int {
  input
  |> list.filter(is_invalid)
  |> list.fold(from: 0, with: int.add)
}

pub fn solve_part2(input: List(Int)) -> Int {
  input
  |> list.filter(is_invalid2)
  |> list.fold(from: 0, with: int.add)
}

fn parse_range(input: String) -> List(Int) {
  let assert [first, second] = string.split(input, on: "-")
  let assert Ok(a) = int.parse(first)
  let assert Ok(b) = int.parse(second)
  list.range(a, b)
}

fn is_invalid(id: Int) -> Bool {
  let string = int.to_string(id)
  let length = string.length(string)
  use <- bool.guard(when: length |> int.is_odd, return: False)
  let half = length / 2
  let first = string.slice(string, 0, half)
  let second = string.slice(string, half, half)
  first == second
}

fn is_invalid2(id: Int) -> Bool {
  let string = int.to_string(id)
  let length = string.length(string)
  use <- bool.guard(when: length < 2, return: False)
  let doubled = string <> string
  let doubled_length = string.length(doubled)
  let double_clipped = string.slice(doubled, 1, doubled_length - 2)
  string.contains(double_clipped, string)
}
