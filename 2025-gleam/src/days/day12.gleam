import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/set.{type Set}
import gleam/string
import utils/input

pub fn solve() -> Result(#(String, String), String) {
  use input <- result.try(input.read_input(12))

  let input_prepared = prepare_input(input)

  let part1 = solve_part1(input_prepared)
  let part2 = solve_part2(input_prepared)

  Ok(#(int.to_string(part1), int.to_string(part2)))
}

pub type Presents =
  Dict(Int, Set(ShapeCoord))

pub type ShapeCoord =
  #(Int, Int)

pub type Region {
  Region(width: Int, height: Int, presents: Dict(Int, Int))
}

pub type PuzzleInput {
  PuzzleInput(presents: Presents, regions: List(Region))
}

pub fn prepare_input(input: String) -> PuzzleInput {
  let #(presents, regions) =
    input
    |> string.trim()
    |> string.split("\n\n")
    |> list.fold(#(dict.new(), []), fn(acc, block) {
      case block |> string.contains("x") {
        True -> #(acc.0, parse_regions(block))
        False -> {
          let #(ind, shape) = parse_present(block)
          #(dict.insert(acc.0, ind, shape), acc.1)
        }
      }
    })

  PuzzleInput(presents, regions)
}

fn parse_regions(block: String) -> List(Region) {
  block
  |> string.split("\n")
  |> list.map(fn(line) {
    let assert Ok(#(dimension, presents_list)) = line |> string.split_once(": ")
    let assert Ok([w, h]) =
      dimension |> string.split("x") |> list.try_map(int.parse)
    let presents =
      presents_list
      |> string.split(" ")
      |> list.index_fold(dict.new(), fn(acc, value, index) {
        let assert Ok(value) = value |> int.parse
        dict.insert(acc, index, value)
      })
    Region(w, h, presents)
  })
}

pub fn parse_present(block: String) -> #(Int, Set(ShapeCoord)) {
  let assert Ok(#(index_string, shape_block)) =
    block |> string.split_once(":\n")
  let assert Ok(index) = index_string |> int.parse
  let shape = shape_block |> shape_to_coords(0, 0, set.new())
  #(index, shape)
}

fn shape_to_coords(
  shape: String,
  x: Int,
  y: Int,
  init: Set(ShapeCoord),
) -> Set(ShapeCoord) {
  case shape |> string.pop_grapheme {
    Error(_) -> init
    Ok(#(".", rest)) -> shape_to_coords(rest, x + 1, y, init)
    Ok(#("#", rest)) ->
      shape_to_coords(rest, x + 1, y, set.insert(init, #(x, y)))
    Ok(#("\n", rest)) -> shape_to_coords(rest, 0, y + 1, init)
    Ok(#(_, rest)) -> shape_to_coords(rest, x, y, init)
  }
}

pub fn solve_part1(input: PuzzleInput) -> Int {
  input |> summary
}

fn summary(input: PuzzleInput) -> Int {
  let PuzzleInput(presents, regions) = input
  let total_regions = list.length(regions)

  let #(surely_fit, surely_impossible) =
    list.fold(regions, #(0, 0), fn(acc, region) {
      let Region(width, height, presents_dict) = region
      let capacity = { width / 3 } * { height / 3 }
      let total_presents = presents_dict |> dict.values |> int.sum
      let can_surely_fit = capacity >= total_presents

      let needed_area =
        dict.fold(presents_dict, 0, fn(sum, present_index, quantity) {
          let assert Ok(shape_area) =
            dict.get(presents, present_index) |> result.map(set.size)
          sum + shape_area * quantity
        })
      let surely_impossible = { width * height } < needed_area

      case can_surely_fit, surely_impossible {
        True, False -> #(acc.0 + 1, acc.1)
        False, True -> #(acc.0, acc.1 + 1)
        _, _ -> acc
      }
    })

  let doubtful = total_regions - surely_fit - surely_impossible

  io.println(
    "Total regions: "
    <> int.to_string(total_regions)
    <> ", Surely Fit: "
    <> int.to_string(surely_fit)
    <> ", Surely Impossible: "
    <> int.to_string(surely_impossible)
    <> ", Doubtful: "
    <> int.to_string(doubtful),
  )

  surely_fit
}

pub fn solve_part2(_input: PuzzleInput) -> Int {
  0
}
