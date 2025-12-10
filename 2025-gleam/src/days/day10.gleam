import gleam/int
import gleam/list
import gleam/result
import gleam/set
import gleam/string
import utils/input

pub fn solve() -> Result(#(String, String), String) {
  use input <- result.try(input.read_input(10))

  let input_prepared = prepare_input(input)

  let part1 = solve_part1(input_prepared)
  let part2 = solve_part2(input_prepared)

  Ok(#(int.to_string(part1), int.to_string(part2)))
}

pub fn prepare_input(input: String) -> List(Machine) {
  input |> string.split("\n") |> list.filter_map(parse_machine)
}

pub fn solve_part1(input: List(Machine)) -> Int {
  input |> echo |> list.map(minimum_presess) |> int.sum
}

pub fn solve_part2(_input: List(Machine)) -> Int {
  0
}

pub type Machine {
  Machine(target: Int, buttons: List(Int), joltage: List(Int))
}

fn parse_machine(line: String) -> Result(Machine, Nil) {
  case string.split_once(line, "[") {
    Error(_) -> Error(Nil)
    Ok(#(_, rest)) -> {
      case string.split_once(rest, "]") {
        Error(_) -> Error(Nil)
        Ok(#(indicator_pattern, rest)) -> {
          let target =
            indicator_pattern
            |> string.to_graphemes
            |> list.index_fold(0, fn(acc, char, idx) {
              case char {
                "#" -> int.bitwise_or(acc, int.bitwise_shift_left(1, idx))
                _ -> acc
              }
            })
          case string.split_once(rest, "{") {
            Error(_) -> Error(Nil)
            Ok(#(buttons_string, joltage_string)) -> {
              let buttons =
                buttons_string
                |> string.trim()
                |> string.split(" ")
                |> list.map(fn(button) {
                  string.trim(button)
                  |> string.drop_end(1)
                  |> string.drop_start(1)
                  |> string.split(",")
                  |> list.fold(from: 0, with: fn(acc, s) {
                    let assert Ok(i) = int.parse(s)
                    int.bitwise_or(acc, int.bitwise_shift_left(1, i))
                  })
                })
              let joltage =
                joltage_string
                |> string.drop_end(1)
                |> string.split(",")
                |> list.filter_map(int.parse)

              Ok(Machine(target, buttons, joltage))
            }
          }
        }
      }
    }
  }
}

fn minimum_presess(machine: Machine) -> Int {
  let Machine(target, buttons, _) = machine
  bfs(buttons, target, set.from_list([0]), set.from_list([0]), 0)
}

fn bfs(
  buttons: List(Int),
  target: Int,
  current_wave: set.Set(Int),
  visited: set.Set(Int),
  depth: Int,
) -> Int {
  case current_wave |> set.contains(target) {
    True -> depth

    False -> {
      let next_wave =
        current_wave
        |> set.fold(set.new(), fn(acc, state) {
          list.fold(buttons, acc, fn(acc2, btn) {
            set.insert(acc2, int.bitwise_exclusive_or(state, btn))
          })
        })
        |> set.difference(visited)

      case set.size(next_wave) {
        0 -> -1
        _ ->
          bfs(
            buttons,
            target,
            next_wave,
            set.union(visited, next_wave),
            depth + 1,
          )
      }
    }
  }
}
