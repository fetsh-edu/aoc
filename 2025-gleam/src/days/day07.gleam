import gleam/dict.{type Dict}
import gleam/int
import gleam/option
import gleam/result
import gleam/set.{type Set}
import gleam/string
import utils/input

pub fn solve() -> Result(#(String, String), String) {
  use input <- result.try(input.read_input(7))

  let input_prepared = prepare_input(input)

  let part1 = solve_part1(input_prepared)
  let part2 = solve_part2(input_prepared)

  Ok(#(int.to_string(part1), int.to_string(part2)))
}

pub fn prepare_input(input: String) -> String {
  input |> string.trim
}

pub fn solve_part1(input: String) -> Int {
  input |> process(0, 0, set.new(), 0)
}

pub fn solve_part2(input: String) -> Int {
  input |> process2(0, 0, dict.new())
}

type Cell {
  Beam
  Splitter
}

type Pos =
  #(Int, Int)

fn to_cell(char: String) -> Result(Cell, Nil) {
  case char {
    "S" | "|" -> Ok(Beam)
    "^" -> Ok(Splitter)
    _ -> Error(Nil)
  }
}

fn process(input: String, x: Int, y: Int, beams: Set(Pos), count: Int) -> Int {
  case string.pop_grapheme(input) {
    Error(_) -> count
    Ok(#("\n", rest)) -> process(rest, 0, y + 1, beams, count)
    Ok(#(char, rest)) -> {
      let cell_x = x + 1
      let cell_y = y
      let has_above = set.contains(beams, #(cell_x, cell_y - 1))
      let #(new_beams, new_count) = case has_above, char |> to_cell {
        False, Ok(Beam) -> #(set.insert(beams, #(cell_x, cell_y)), count)
        True, Ok(Splitter) -> #(
          beams
            |> set.insert(#(cell_x + 1, cell_y + 1))
            |> set.insert(#(cell_x - 1, cell_y + 1)),
          count + 1,
        )
        True, _ -> #(set.insert(beams, #(cell_x, cell_y)), count)
        _, _ -> #(beams, count)
      }
      process(rest, x + 1, y, new_beams, new_count)
    }
  }
}

fn process2(input: String, x: Int, y: Int, paths: Dict(Pos, Int)) -> Int {
  case string.pop_grapheme(input) {
    Error(_) -> paths |> dict.values |> int.sum
    Ok(#("\n", rest)) -> process2(rest, 0, y + 1, paths)

    Ok(#(char, rest)) -> {
      let cell_x = x + 1
      let cell_y = y
      let cell_pos = #(cell_x, cell_y)
      let above_pos = #(cell_x, cell_y - 1)
      let paths_above = dict.get(paths, above_pos) |> result.unwrap(0)

      let paths_cleared = case paths_above > 0 {
        True -> dict.delete(paths, above_pos)
        False -> paths
      }

      let new_paths = case paths_above, char |> to_cell {
        0, Ok(Beam) -> dict.insert(paths_cleared, cell_pos, 1)
        n, Ok(Splitter) -> {
          let left_pos = #(cell_x - 1, cell_y + 1)
          let right_pos = #(cell_x + 1, cell_y + 1)
          paths_cleared
          |> add_paths(left_pos, n)
          |> add_paths(right_pos, n)
        }
        n, _ -> add_paths(paths_cleared, cell_pos, n)
      }

      process2(rest, x + 1, y, new_paths)
    }
  }
}

fn add_paths(paths: Dict(Pos, Int), pos: Pos, n: Int) -> Dict(Pos, Int) {
  dict.upsert(paths, pos, fn(existing) {
    existing |> option.map(fn(count) { count + n }) |> option.unwrap(n)
  })
}
