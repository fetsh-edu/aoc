# Advent of Code - Gleam

Solutions for [Advent of Code](https://adventofcode.com/) in Gleam.

## Installation

1. Install [Gleam](https://gleam.run/getting-started/installing/)

2. Clone the repository and install dependencies:
```bash
gleam deps download
```

## Usage

### Running a solution for a specific day

```bash
gleam run <day_number>
```

Examples:
```bash
gleam run 1    # Day 1
gleam run 8    # Day 8
gleam run 12   # Day 12
```

### Running tests

Run all tests:
```bash
gleam test
```

## Notes

- Input files are named with leading zero for days 1-9: `day01.txt`, `day02.txt`, etc.
- Each day module must export a `solve() -> Result(#(String, String), String)` function
- First element in the tuple is the Part 1 answer, second is the Part 2 answer
