import argv
import days/day01
import days/day02
import days/day03
import days/day04
import days/day05
import days/day06
import days/day07
import days/day08
import days/day09
import days/day10
import days/day11
import gleam/int
import gleam/io
import gleam/string

pub fn main() {
  case argv.load().arguments {
    [day_str] -> run_day(day_str)
    _ -> {
      io.println("Usage: gleam run <day>")
      io.println("Example: gleam run 1")
    }
  }
}

fn run_day(day_str: String) {
  case int.parse(day_str) {
    Ok(day) -> {
      io.println("Running Day " <> int.to_string(day))
      io.println("=" |> string.repeat(13))

      case solve_day(day) {
        Ok(#(part1, part2)) -> {
          io.println("Part 1: " <> part1)
          io.println("Part 2: " <> part2)
        }
        Error(msg) -> io.println("Error: " <> msg)
      }
    }
    Error(_) -> io.println("Invalid day number: " <> day_str)
  }
}

fn solve_day(day: Int) -> Result(#(String, String), String) {
  case day {
    1 -> day01.solve()
    2 -> day02.solve()
    3 -> day03.solve()
    4 -> day04.solve()
    5 -> day05.solve()
    6 -> day06.solve()
    7 -> day07.solve()
    8 -> day08.solve()
    9 -> day09.solve()
    10 -> day10.solve()
    11 -> day11.solve()
    _ -> Error("Day " <> int.to_string(day) <> " not implemented yet")
  }
}
