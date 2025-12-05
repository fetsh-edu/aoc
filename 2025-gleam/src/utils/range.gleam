import gleam/int
import gleam/order
import gleam/string

pub type Range {
  Range(start: Int, end: Int)
}

pub fn parse(range_str: String, separator: String) -> Range {
  let assert [start_str, end_str] = string.split(range_str, separator)
  let assert Ok(start) = start_str |> string.trim |> int.parse
  let assert Ok(end) = end_str |> string.trim |> int.parse

  Range(start, end)
}

pub fn compare(range1: Range, range2: Range) -> order.Order {
  int.compare(range1.start, range2.start)
}

pub fn contains(range: Range, value: Int) -> Bool {
  range.start <= value && value <= range.end
}

pub fn length(range: Range) -> Int {
  range.end - range.start + 1
}

pub fn merge(range1: Range, range2: Range) -> Range {
  Range(int.min(range1.start, range2.start), int.max(range1.end, range2.end))
}

pub fn overlaps(range1: Range, range2: Range) -> Bool {
  range1.start <= range2.end && range2.start <= range1.end
}
