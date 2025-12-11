import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/set
import gleam/string
import utils/input
import utils/time

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
  input |> list.map(minimum_presess) |> int.sum
}

pub fn solve_part2(input: List(Machine)) -> Int {
  time.measure_echo(fn() {
    input
    |> list.map(minimum_presses_ilp)
    |> int.sum
  })
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

pub fn minimum_presses_ilp(machine: Machine) -> Int {
  let Machine(_, buttons, joltage) = machine
  let n_counters = list.length(joltage)
  let n_buttons = list.length(buttons)

  // Build matrix
  let matrix =
    list.index_fold(joltage, dict.new(), fn(mat, target_val, row) {
      let mat = dict.insert(mat, #(row, n_buttons), target_val)
      list.index_fold(buttons, mat, fn(m, mask, col) {
        let val = case
          int.bitwise_and(mask, int.bitwise_shift_left(1, row)) != 0
        {
          True -> 1
          False -> 0
        }
        dict.insert(m, #(row, col), val)
      })
    })

  let #(reduced, pivot_cols) =
    gaussian_elimination(matrix, n_counters, n_buttons)

  let free_cols =
    list.range(0, n_buttons - 1)
    |> list.filter(fn(c) { !list.contains(pivot_cols, c) })

  // Compute better bounds for free variables based on original targets
  let max_target = list.fold(joltage, 0, int.max)

  find_minimum_solution_bounded(
    reduced,
    pivot_cols,
    free_cols,
    n_buttons,
    max_target,
  )
}

fn find_minimum_solution_bounded(
  matrix: Dict(#(Int, Int), Int),
  pivot_cols: List(Int),
  free_cols: List(Int),
  n_buttons: Int,
  max_target: Int,
) -> Int {
  case free_cols {
    [] -> {
      // No free variables - unique solution (if exists)
      case solve_with_free(matrix, pivot_cols, [], [], n_buttons) {
        Ok(sum) -> sum
        Error(_) -> 999_999_999
      }
    }
    _ -> {
      // Enumerate free variables with smarter bounds
      let ranges = list.map(free_cols, fn(_) { list.range(0, max_target) })
      let all_combos = cartesian_product(ranges)

      all_combos
      |> list.filter_map(fn(free_vals) {
        solve_with_free(matrix, pivot_cols, free_cols, free_vals, n_buttons)
      })
      |> list.fold(999_999_999, int.min)
    }
  }
}

fn solve_with_free(
  matrix: Dict(#(Int, Int), Int),
  pivot_cols: List(Int),
  free_cols: List(Int),
  free_vals: List(Int),
  n_buttons: Int,
) -> Result(Int, Nil) {
  let solution =
    list.zip(free_cols, free_vals)
    |> list.fold(dict.new(), fn(sol, pair) {
      let #(col, val) = pair
      dict.insert(sol, col, val)
    })

  let n_pivots = list.length(pivot_cols)

  list.range(0, n_pivots - 1)
  |> list.reverse
  |> list.try_fold(solution, fn(sol, pivot_idx) {
    let assert Ok(pivot_col) = list_at(pivot_cols, pivot_idx)
    let pivot_val = mat_get(matrix, pivot_idx, pivot_col)
    let rhs = mat_get(matrix, pivot_idx, n_buttons)

    case pivot_val == 0 {
      True -> Error(Nil)
      False -> {
        let other_sum =
          list.range(0, n_buttons - 1)
          |> list.filter(fn(c) { c != pivot_col })
          |> list.fold(0, fn(acc, c) {
            let coef = mat_get(matrix, pivot_idx, c)
            let val = dict.get(sol, c) |> result.unwrap(0)
            acc + coef * val
          })

        let numerator = rhs - other_sum

        // Handle negative pivot_val (can happen after elimination)
        let #(num, piv) = case pivot_val < 0 {
          True -> #(-numerator, -pivot_val)
          False -> #(numerator, pivot_val)
        }

        case num % piv == 0 {
          False -> Error(Nil)
          True -> {
            let val = num / piv
            case val >= 0 {
              True -> Ok(dict.insert(sol, pivot_col, val))
              False -> Error(Nil)
            }
          }
        }
      }
    }
  })
  |> result.map(fn(sol) { dict.values(sol) |> int.sum })
}

// Also normalize rows after elimination to avoid huge numbers
fn eliminate_column(
  matrix: Dict(#(Int, Int), Int),
  pivot_row: Int,
  pivot_col: Int,
  n_rows: Int,
  n_cols: Int,
) -> Dict(#(Int, Int), Int) {
  let pivot_val = mat_get(matrix, pivot_row, pivot_col)

  list.range(0, n_rows - 1)
  |> list.fold(matrix, fn(mat, row) {
    case row == pivot_row {
      True -> mat
      False -> {
        let curr_val = mat_get(mat, row, pivot_col)
        case curr_val == 0 {
          True -> mat
          False -> {
            // new_row = curr_row * pivot_val - pivot_row * curr_val
            let new_mat =
              list.range(0, n_cols - 1)
              |> list.fold(mat, fn(m, col) {
                let a = mat_get(mat, row, col)
                let b = mat_get(mat, pivot_row, col)
                mat_set(m, row, col, a * pivot_val - b * curr_val)
              })
            // Normalize row by GCD
            normalize_row(new_mat, row, n_cols)
          }
        }
      }
    }
  })
}

fn normalize_row(
  matrix: Dict(#(Int, Int), Int),
  row: Int,
  n_cols: Int,
) -> Dict(#(Int, Int), Int) {
  let values =
    list.range(0, n_cols - 1)
    |> list.map(fn(col) { mat_get(matrix, row, col) |> int.absolute_value })
    |> list.filter(fn(v) { v != 0 })

  case values {
    [] -> matrix
    _ -> {
      let g = list.fold(values, 0, gcd)
      case g > 1 {
        False -> matrix
        True ->
          list.range(0, n_cols - 1)
          |> list.fold(matrix, fn(m, col) {
            let v = mat_get(m, row, col)
            mat_set(m, row, col, v / g)
          })
      }
    }
  }
}

fn gcd(a: Int, b: Int) -> Int {
  case b {
    0 -> int.absolute_value(a)
    _ -> gcd(b, a % b)
  }
}

fn mat_get(mat: Dict(#(Int, Int), Int), row: Int, col: Int) -> Int {
  dict.get(mat, #(row, col)) |> result.unwrap(0)
}

fn mat_set(
  mat: Dict(#(Int, Int), Int),
  row: Int,
  col: Int,
  val: Int,
) -> Dict(#(Int, Int), Int) {
  dict.insert(mat, #(row, col), val)
}

fn list_at(lst: List(a), idx: Int) -> Result(a, Nil) {
  lst |> list.drop(idx) |> list.first
}

fn cartesian_product(lists: List(List(Int))) -> List(List(Int)) {
  case lists {
    [] -> [[]]
    [first, ..rest] -> {
      let rest_product = cartesian_product(rest)
      list.flat_map(first, fn(x) {
        list.map(rest_product, fn(combo) { [x, ..combo] })
      })
    }
  }
}

// Gaussian elimination over integers
fn gaussian_elimination(
  matrix: Dict(#(Int, Int), Int),
  n_rows: Int,
  n_cols: Int,
) -> #(Dict(#(Int, Int), Int), List(Int)) {
  do_elimination(matrix, 0, 0, n_rows, n_cols, [])
}

fn do_elimination(
  matrix: Dict(#(Int, Int), Int),
  pivot_row: Int,
  pivot_col: Int,
  n_rows: Int,
  n_cols: Int,
  pivot_cols: List(Int),
) -> #(Dict(#(Int, Int), Int), List(Int)) {
  case pivot_row >= n_rows || pivot_col >= n_cols {
    True -> #(matrix, list.reverse(pivot_cols))
    False -> {
      case find_pivot_row(matrix, pivot_row, pivot_col, n_rows) {
        None ->
          do_elimination(
            matrix,
            pivot_row,
            pivot_col + 1,
            n_rows,
            n_cols,
            pivot_cols,
          )
        Some(found_row) -> {
          let matrix = swap_rows(matrix, pivot_row, found_row, n_cols + 1)
          let matrix =
            eliminate_column(matrix, pivot_row, pivot_col, n_rows, n_cols + 1)
          do_elimination(matrix, pivot_row + 1, pivot_col + 1, n_rows, n_cols, [
            pivot_col,
            ..pivot_cols
          ])
        }
      }
    }
  }
}

fn find_pivot_row(
  matrix: Dict(#(Int, Int), Int),
  start_row: Int,
  col: Int,
  n_rows: Int,
) -> option.Option(Int) {
  list.range(start_row, n_rows - 1)
  |> list.find_map(fn(row) {
    case mat_get(matrix, row, col) != 0 {
      True -> Ok(row)
      False -> Error(Nil)
    }
  })
  |> option.from_result
}

fn swap_rows(
  matrix: Dict(#(Int, Int), Int),
  i: Int,
  j: Int,
  n_cols: Int,
) -> Dict(#(Int, Int), Int) {
  case i == j {
    True -> matrix
    False ->
      list.range(0, n_cols - 1)
      |> list.fold(matrix, fn(mat, col) {
        let vi = mat_get(mat, i, col)
        let vj = mat_get(mat, j, col)
        mat |> mat_set(i, col, vj) |> mat_set(j, col, vi)
      })
  }
}
