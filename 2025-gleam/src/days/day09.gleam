import gleam/dict
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import utils/input

pub fn solve() -> Result(#(String, String), String) {
  use input <- result.try(input.read_input(9))

  let input_prepared = prepare_input(input)

  let part1 = solve_part1(input_prepared)
  let part2 = solve_part2(input_prepared)

  Ok(#(int.to_string(part1), int.to_string(part2)))
}

pub fn prepare_input(input: String) -> List(Tile) {
  input |> string.trim() |> string.split("\n") |> list.map(parse_tile)
}

pub fn solve_part1(input: List(Tile)) -> Int {
  input
  |> list.combination_pairs
  |> list.fold(from: 0, with: fn(best, tt) { int.max(best, area(tt.0, tt.1)) })
}

pub fn solve_part2(input: List(Tile)) -> Int {
  let slices =
    input
    |> fn(tiles) {
      case tiles {
        [] -> dict.new()
        [first, ..rest] -> build_slices_loop(first, first, rest, dict.new())
      }
    }
    |> dict.map_values(fn(_, xs) {
      list.window_by_2(list.sort(xs, int.compare))
    })

  input
  |> list.combination_pairs
  |> list.map(fn(tt) { #(tt.0, tt.1, area(tt.0, tt.1)) })
  |> list.sort(fn(a, b) { int.compare(b.2, a.2) })
  |> list.find_map(fn(tt) {
    case is_valid(tt.0, tt.1, slices) {
      True -> Ok(tt.2)
      False -> Error(Nil)
    }
  })
  |> result.unwrap(0)
}

pub type Tile {
  Tile(x: Int, y: Int)
}

fn parse_tile(line: String) -> Tile {
  let assert [Ok(x), Ok(y)] = line |> string.split(",") |> list.map(int.parse)
  Tile(x, y)
}

fn area(tile1: Tile, tile2: Tile) -> Int {
  let width = int.absolute_value(tile2.x - tile1.x) + 1
  let height = int.absolute_value(tile2.y - tile1.y) + 1
  width * height
}

fn is_valid(
  tile1: Tile,
  tile2: Tile,
  slices: dict.Dict(Int, List(#(Int, Int))),
) -> Bool {
  let #(Tile(x1, y1), Tile(x2, y2)) = #(tile1, tile2)
  let #(x_min, x_max) = #(int.min(x1, x2), int.max(x1, x2))

  list.range(int.min(y1, y2), int.max(y1, y2) - 1)
  |> list.all(fn(y) {
    dict.get(slices, y)
    |> result.unwrap([])
    |> list.any(fn(slice) { slice.0 <= x_min && x_max <= slice.1 })
  })
}

fn build_slices_loop(
  first: Tile,
  prev: Tile,
  remaining: List(Tile),
  slices: dict.Dict(Int, List(Int)),
) -> dict.Dict(Int, List(Int)) {
  case remaining {
    [] -> process_edge(prev, first, slices)
    [next, ..rest] ->
      build_slices_loop(first, next, rest, process_edge(prev, next, slices))
  }
}

fn process_edge(
  a: Tile,
  b: Tile,
  slices: dict.Dict(Int, List(Int)),
) -> dict.Dict(Int, List(Int)) {
  let #(Tile(x1, y1), Tile(x2, y2)) = #(a, b)

  case x1 == x2 {
    False -> slices
    True -> {
      list.range(int.min(y1, y2), int.max(y1, y2) - 1)
      |> list.fold(from: slices, with: fn(acc, y) {
        dict.upsert(acc, y, fn(opt) {
          opt
          |> option.map(fn(old) { [x1, ..old] })
          |> option.unwrap([x1])
        })
      })
    }
  }
}
