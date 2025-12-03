import gleam/bool
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import utils/input
import utils/int as i

pub fn solve() -> Result(#(String, String), String) {
  use input <- result.try(input.read_input(3))

  let batteries = prepare_input(input)

  let part1 = solve_part1(batteries)
  let part2 = solve_part2(batteries)

  Ok(#(int.to_string(part1), int.to_string(part2)))
}

pub fn prepare_input(input: String) -> List(List(Int)) {
  input
  |> string.trim
  |> string.split("\n")
  |> list.map(fn(bank) {
    bank |> string.to_graphemes |> list.map(i.parse_assert)
  })
}

pub fn solve_part1(batteries: List(List(Int))) -> Int {
  batteries
  |> list.map(fn(battery) { max_joltage(battery, 2) })
  |> list.fold(0, int.add)
}

pub fn solve_part2(batteries: List(List(Int))) -> Int {
  batteries
  |> list.map(fn(battery) { max_joltage(battery, 12) })
  |> list.fold(0, int.add)
}

fn max_joltage(battery: List(Int), target: Int) -> Int {
  battery
  |> get_highest_loop([], target)
  |> list.map(int.to_string)
  |> string.join("")
  |> i.parse_assert
}

fn get_highest_loop(
  rest: List(Int),
  chosen: List(Int),
  target: Int,
) -> List(Int) {
  let chosen_length = list.length(chosen)
  use <- bool.guard(when: chosen_length == target, return: chosen)
  let rest_length = list.length(rest)
  let assert Ok(#(index, max)) =
    rest
    |> list.take(rest_length - { target - chosen_length - 1 })
    |> list.index_map(fn(x, i) { #(i, x) })
    |> list.max(fn(x, y) { int.compare(x.1, y.1) })

  get_highest_loop(
    list.drop(rest, index + 1),
    list.append(chosen, [max]),
    target,
  )
}
