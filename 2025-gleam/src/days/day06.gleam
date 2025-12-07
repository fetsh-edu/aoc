import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import utils/input
import utils/int as i
import utils/list as l

pub fn solve() -> Result(#(String, String), String) {
  use input <- result.try(input.read_input(6))

  let input_prepared = prepare_input(input)

  let part1 = solve_part1(input_prepared)
  let part2 = solve_part2(input_prepared)

  Ok(#(int.to_string(part1), int.to_string(part2)))
}

pub type Op {
  Add
  Multiply
}

pub fn prepare_input(input: String) -> List(String) {
  input |> string.split("\n") |> list.filter(fn(s) { !string.is_empty(s) })
}

pub fn solve_part1(input: List(String)) -> Int {
  input
  |> to_problems
  |> list.map(compute_problem)
  |> int.sum
}

pub fn solve_part2(input: List(String)) -> Int {
  input
  |> to_problems2(None, [], [])
  |> list.map(compute_problem)
  |> int.sum
}

fn to_problems(input: List(String)) -> List(#(Op, List(Int))) {
  let rows =
    input
    |> list.map(fn(row) {
      row
      |> string.split(" ")
      |> list.filter(fn(s) { !string.is_empty(s) })
    })

  let assert Ok(operators_row) = list.last(rows)
  let operators =
    operators_row
    |> list.map(parse_operator)

  let operands =
    rows
    |> list.take(list.length(rows) - 1)
    |> list.fold(list.repeat([], list.length(operators)), fn(acc_cols, row) {
      l.zip_with(row, acc_cols, fn(tok, col_nums) {
        let assert Ok(n) = int.parse(tok)
        [n, ..col_nums]
      })
    })
    |> list.map(list.reverse)

  list.zip(operators, operands)
}

pub type Problem =
  #(Op, List(Int))

fn to_problems2(
  rows: List(String),
  current_operator: Option(Op),
  current_nums: List(Int),
  problems: List(Problem),
) -> List(Problem) {
  let popped_result =
    rows |> list.map(string.pop_grapheme) |> list.try_map(fn(x) { x })
  case popped_result {
    Error(_) -> {
      let assert Some(op) = current_operator
      [#(op, current_nums), ..problems]
    }
    Ok(popped) -> {
      let #(heads, rests) = list.unzip(popped)
      case heads |> list.all(fn(p) { p == " " }) {
        True -> {
          let assert Some(op) = current_operator
          to_problems2(rests, None, [], [#(op, current_nums), ..problems])
        }
        False -> {
          let digits =
            heads
            |> list.take(list.length(heads) - 1)
            |> string.join("")
            |> string.trim()
            |> i.parse_assert

          let next_op =
            current_operator
            |> option.lazy_or(fn() {
              list.last(heads)
              |> result.map(parse_operator)
              |> option.from_result
            })

          to_problems2(rests, next_op, [digits, ..current_nums], problems)
        }
      }
    }
  }
}

fn parse_operator(s: String) -> Op {
  case s {
    "+" -> Add
    "*" -> Multiply
    some -> panic as { "Invalid operator: " <> some }
  }
}

fn compute_problem(problem: #(Op, List(Int))) -> Int {
  let #(operator, numbers) = problem
  case operator {
    Add -> numbers |> int.sum
    Multiply -> numbers |> int.product
  }
}
