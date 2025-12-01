import gleam/list
import gleam/result
import gleam/string
import simplifile

pub fn read_input(day: Int) -> Result(String, String) {
  let day_str = case day {
    d if d < 10 -> "0" <> string.inspect(d)
    d -> string.inspect(d)
  }

  let path = "inputs/day" <> day_str <> ".txt"

  simplifile.read(path)
  |> result.map_error(fn(_) { "Failed to read input file: " <> path })
}

pub fn read_test_input(day: Int) -> Result(String, String) {
  let day_str = case day {
    d if d < 10 -> "0" <> string.inspect(d)
    d -> string.inspect(d)
  }

  let path = "test/test_inputs/day" <> day_str <> ".txt"

  simplifile.read(path)
  |> result.map_error(fn(_) { "Failed to read test input file: " <> path })
}

pub fn lines(input: String) -> List(String) {
  input
  |> string.split("\n")
  |> list.map(string.trim)
}

pub fn non_empty_lines(input: String) -> List(String) {
  input
  |> lines
  |> list.filter(fn(line) { !string.is_empty(line) })
}
