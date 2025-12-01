import gleam/int
import gleam/list
import gleam/result
import utils/input
import utils/result as r

const dial_size = 100

const start_pos = 50

pub fn solve() -> Result(#(String, String), String) {
  use input <- result.try(input.read_input(1))

  let rotations = parse_rotations(input)

  let part1 = solve_part1(rotations)
  let part2 = solve_part2(rotations)

  Ok(#(int.to_string(part1), int.to_string(part2)))
}

pub fn solve_part1(rotations: List(Int)) -> Int {
  rotations
  |> list.scan(from: start_pos, with: rotate)
  |> list.count(fn(n) { n == 0 })
}

pub fn solve_part2(rotations: List(Int)) -> Int {
  rotations
  |> list.fold(from: #(start_pos, 0), with: fn(acc, rotation) {
    let #(current_position, total_crossings) = acc
    let new_position = rotate(current_position, rotation)
    let crossings = count_zero_crossings(current_position, rotation)
    #(new_position, total_crossings + crossings)
  })
  |> fn(result) { result.1 }
}

pub fn parse_rotations(input: String) -> List(Int) {
  input
  |> input.non_empty_lines
  |> list.map(parse_rotation)
}

fn parse_rotation(input: String) -> Int {
  case input {
    "L" <> rest -> int.parse("-" <> rest)
    "R" <> rest -> int.parse(rest)
    _ -> panic as { "Invalid rotation: " <> input }
  }
  |> r.assert_ok
}

fn rotate(init: Int, rotation: Int) -> Int {
  int.modulo(init + rotation, dial_size) |> r.assert_ok
}

fn count_zero_crossings(init: Int, rotation: Int) -> Int {
  let distance = int.absolute_value(rotation)

  let first = case init, rotation >= 0 {
    0, _ -> dial_size
    _, True -> dial_size - init
    _, False -> init
  }

  case distance >= first {
    True -> 1 + { distance - first } / dial_size
    False -> 0
  }
}
