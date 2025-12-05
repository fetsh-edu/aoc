import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import utils/input
import utils/range.{type Range}
import utils/result as r

pub fn solve() -> Result(#(String, String), String) {
  use input <- result.try(input.read_input(5))

  let input_prepared = prepare_input(input)

  let part1 = solve_part1(input_prepared)
  let part2 = solve_part2(input_prepared)

  Ok(#(int.to_string(part1), int.to_string(part2)))
}

pub fn prepare_input(input: String) -> #(List(Range), List(Int)) {
  let assert [ranges_chunk, ids_chunk] =
    input |> string.trim() |> string.split("\n\n")

  let ranges =
    ranges_chunk
    |> string.split("\n")
    |> list.map(fn(s) { range.parse(s, "-") })
  let ids =
    ids_chunk
    |> string.split("\n")
    |> list.map(fn(s) { int.parse(s) |> r.assert_ok })

  #(ranges, ids)
}

pub fn solve_part1(input: #(List(Range), List(Int))) -> Int {
  let #(ranges, ids) = input

  list.count(ids, fn(id) { list.any(ranges, fn(r) { range.contains(r, id) }) })
}

pub fn solve_part2(input: #(List(Range), List(Int))) -> Int {
  let #(ranges, _ids) = input
  ranges |> list.sort(by: range.compare) |> merge_and_count(0, None)
}

fn merge_and_count(
  ranges: List(Range),
  total: Int,
  previous: Option(Range),
) -> Int {
  case ranges, previous {
    [], None -> total
    [], Some(prev) -> total + range.length(prev)
    [current, ..tail], None -> merge_and_count(tail, total, Some(current))
    [current, ..tail], Some(prev) -> {
      case range.overlaps(prev, current) {
        True -> merge_and_count(tail, total, Some(range.merge(prev, current)))
        False ->
          merge_and_count(tail, total + range.length(prev), Some(current))
      }
    }
  }
}
