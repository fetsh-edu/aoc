import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/pair
import gleam/result
import gleam/set.{type Set}
import gleam/string
import utils/input

pub type Graph =
  Dict(String, List(String))

pub fn solve() -> Result(#(String, String), String) {
  use input <- result.try(input.read_input(11))

  let input_prepared = prepare_input(input)

  let part1 = solve_part1(input_prepared)
  let part2 = solve_part2(input_prepared)

  Ok(#(int.to_string(part1), int.to_string(part2)))
}

pub fn prepare_input(input: String) -> Graph {
  input
  |> string.trim()
  |> string.split("\n")
  |> list.fold(from: dict.new(), with: fn(acc, line) {
    let assert Ok(#(from, rest)) = string.split_once(line, ": ")
    dict.insert(acc, from, rest |> string.split(" "))
  })
}

pub fn solve_part1(input: Graph) -> Int {
  input |> count_paths("you", "out", dict.new()) |> pair.second
}

pub fn solve_part2(input: Graph) -> Int {
  input
  |> count_paths_constrained(
    "svr",
    "out",
    set.new(),
    set.from_list(["dac", "fft"]),
    dict.new(),
  )
  |> pair.second
}

fn count_paths(
  graph: Graph,
  current: String,
  target: String,
  cache: Dict(String, Int),
) -> #(Dict(String, Int), Int) {
  case dict.get(cache, current) {
    Ok(cached) -> #(cache, cached)
    Error(_) ->
      case current == target {
        True -> #(dict.insert(cache, current, 1), 1)
        False -> {
          let #(new_memo, total) =
            dict.get(graph, current)
            |> result.unwrap([])
            |> list.fold(#(cache, 0), fn(acc, child) {
              let #(memo_acc, sum_acc) = acc
              let #(memo_next, child_count) =
                count_paths(graph, child, target, memo_acc)
              #(memo_next, sum_acc + child_count)
            })

          #(dict.insert(new_memo, current, total), total)
        }
      }
  }
}

fn count_paths_constrained(
  graph: Graph,
  current: String,
  target: String,
  seen_required: Set(String),
  required: Set(String),
  memo: Dict(#(String, Set(String)), Int),
) -> #(Dict(#(String, Set(String)), Int), Int) {
  let new_seen = case set.contains(required, current) {
    True -> set.insert(seen_required, current)
    False -> seen_required
  }

  let key = #(current, new_seen)

  case dict.get(memo, key) {
    Ok(cached) -> #(memo, cached)
    Error(_) -> {
      case current == target {
        True -> {
          let count = case set.size(new_seen) == set.size(required) {
            True -> 1
            False -> 0
          }
          #(dict.insert(memo, key, count), count)
        }
        False -> {
          let children = dict.get(graph, current) |> result.unwrap([])
          let #(final_memo, total) =
            list.fold(children, #(memo, 0), fn(acc, child) {
              let #(memo_acc, sum_acc) = acc
              let #(memo_next, count) =
                count_paths_constrained(
                  graph,
                  child,
                  target,
                  new_seen,
                  required,
                  memo_acc,
                )
              #(memo_next, sum_acc + count)
            })
          #(dict.insert(final_memo, key, total), total)
        }
      }
    }
  }
}
