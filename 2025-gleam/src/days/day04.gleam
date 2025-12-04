import gleam/dict
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import utils/grid
import utils/input

pub fn solve() -> Result(#(String, String), String) {
  use input <- result.try(input.read_input(4))

  let input_prepared = prepare_input(input)

  let part1 = solve_part1(input_prepared)
  let part2 = solve_part2(input_prepared)

  Ok(#(int.to_string(part1), int.to_string(part2)))
}

pub fn prepare_input(input: String) -> grid.Grid(String) {
  input
  |> string.trim()
  |> string.split("\n")
  |> list.map(string.to_graphemes)
  |> grid.from_rows_where(fn(cell) { cell == "@" })
}

pub fn solve_part1(grid: grid.Grid(String)) -> Int {
  grid |> grid.count_keys(where: grid.has_fewer_neighbours8(grid, _, 4))
}

pub fn solve_part2(grid: grid.Grid(String)) -> Int {
  clean(grid, 0)
}

pub fn clean(grid: grid.Grid(String), removed: Int) -> Int {
  let #(can_be_removed, remaining) =
    grid.partition(grid, fn(pos, _) { grid.has_fewer_neighbours8(grid, pos, 4) })

  case dict.size(can_be_removed) {
    0 -> removed
    n -> {
      grid.Grid(remaining)
      |> clean(removed + n)
    }
  }
}
