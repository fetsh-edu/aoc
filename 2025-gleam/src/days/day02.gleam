import gleam/int
import gleam/list
import gleam/result
import gleam/string
import utils/input
import utils/result as r

pub fn solve() -> Result(#(String, String), String) {
  use input <- result.try(input.read_input(2))

  let part1 = solve_part1(input)
  let part2 = solve_part2(input)

  Ok(#(int.to_string(part1), int.to_string(part2)))
}

pub fn solve_part1(input: String) -> Int {
  input
  |> string.trim
  |> string.split(",")
  |> list.flat_map(parse_range)
  |> list.filter(is_invalid)
  |> list.fold(from: 0, with: int.add)
}

pub fn solve_part2(input: String) -> Int {
  input
  |> string.trim
  |> string.split(",")
  |> list.flat_map(parse_range)
  |> list.filter(is_invalid2)
  |> list.fold(from: 0, with: int.add)
}

fn parse_range(input: String) -> List(Int) {
  case string.split(input, on: "-") {
    [first, second] ->
      r.map2(int.parse(first), int.parse(second), fn(a, b) { list.range(a, b) })
    _ -> Error(Nil)
  }
  |> r.assert_ok
}

fn is_invalid(id: Int) -> Bool {
  let string = int.to_string(id)
  let length = string.length(string)
  case length % 2 {
    0 -> {
      let half = length / 2
      let first = string.slice(string, 0, half)
      let second = string.slice(string, half, half)
      first == second
    }
    _ -> False
  }
}

fn is_invalid2(id: Int) -> Bool {
  let string = int.to_string(id)
  let length = string.length(string)
  case length < 2 {
    True -> False
    False -> {
      let doubled = string <> string
      let doubled_length = string.length(doubled)
      let double_clipped = string.slice(doubled, 1, doubled_length - 2)
      string.contains(double_clipped, string)
    }
  }
}
