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
  |> list.filter(fn(s) { !{ string.trim(s) |> string.is_empty } })
  |> to_problems
  |> list.map(compute_problem)
  |> int.sum
}

pub fn solve_part2(input: List(String)) -> Int {
  let assert [row1, row2, row3, row4, op_row] = input
  to_problems4(row1, row2, row3, row4, op_row, None, [], [])
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

fn to_problems4(
  row1: String,
  row2: String,
  row3: String,
  row4: String,
  op_row: String,
  current_operator: Option(Op),
  current_nums: List(Int),
  problems: List(#(Op, List(Int))),
) -> List(#(Op, List(Int))) {
  case
    string.pop_grapheme(row1),
    string.pop_grapheme(row2),
    string.pop_grapheme(row3),
    string.pop_grapheme(row4),
    string.pop_grapheme(op_row)
  {
    // At the end
    Error(_), Error(_), Error(_), Error(_), Error(_) -> {
      let assert Some(op) = current_operator
      [#(op, current_nums), ..problems]
    }

    // Empty column
    Ok(#(" ", rest1)),
      Ok(#(" ", rest2)),
      Ok(#(" ", rest3)),
      Ok(#(" ", rest4)),
      Ok(#(" ", rest_ops))
    -> {
      let assert Some(op) = current_operator
      to_problems4(rest1, rest2, rest3, rest4, rest_ops, None, [], [
        #(op, current_nums),
        ..problems
      ])
    }
    // Regular column
    Ok(#(g1, rest1)),
      Ok(#(g2, rest2)),
      Ok(#(g3, rest3)),
      Ok(#(g4, rest4)),
      Ok(#(gop, rest_ops))
    -> {
      let digits =
        [g1, g2, g3, g4] |> string.join("") |> string.trim() |> i.parse_assert

      let next_op = case current_operator {
        Some(op) -> Some(op)
        None -> Some(parse_operator(gop))
      }

      to_problems4(
        rest1,
        rest2,
        rest3,
        rest4,
        rest_ops,
        next_op,
        [digits, ..current_nums],
        problems,
      )
    }

    _, _, _, _, _ -> panic as "Rows must have the same length"
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
