# Camlukah 🐫🎄

An OCaml template and runner for [Advent of Code](https://adventofcode.com/) puzzles.

## Features

- 🎯 **Solve** - Run solutions for specific days or all days
- 🧪 **Test** - Run tests with example inputs
- ⚡ **Generate** - Auto-generate day templates with boilerplate
- 📥 **Download** - Fetch puzzle descriptions and inputs from AOC
- 🎨 **Pretty Output** - Colored terminal output with proper formatting

## Installation

### Prerequisites

- OCaml
- opam (OCaml package manager)

### Setup

```bash
# Clone the repository
git clone <your-repo-url>
cd camlukah

# Create local opam switch
opam switch create . 5.1.0 --deps-only

# Install dependencies
opam install dune lwt cohttp-lwt-unix lambdasoup ANSITerminal tls-lwt

# Build the project
dune build
```

## Usage

### Solving Puzzles

Run a specific day:
```bash
dune exec camlukah solve 1
```

Run a specific part:
```bash
dune exec camlukah solve 1 1 # Day 1 Part 1
dune exec camlukah solve 1 2 # Day 1 Part 2
```

### Running Tests

Test a specific day:
```bash
dune exec camlukah test 1
```

Test all days:
```bash
dune exec camlukah test
```

### Generating New Days

Generate template for a new day:
```bash
dune exec camlukah generate 5
```

This creates:
- `lib/days/day05.ml` - Solution template
- `test/day05_test.ml` - Test template
- Auto-registers the day in `bin/commands/solve.ml` and `test/test_runner.ml`

### Downloading Puzzles

Download puzzle description and input for a specific day:
```bash
dune exec camlukah download 1
```

Download from a specific year:
```bash
dune exec camlukah download 1 --year 2015
```

**First time setup:** You'll be prompted to enter your Advent of Code session cookie. Get it from your browser:
1. Log in to [adventofcode.com](https://adventofcode.com/)
2. Open browser DevTools (F12)
3. Go to Application/Storage → Cookies
4. Copy the value of the `session` cookie
5. Paste it when prompted

The session cookie is saved in `.aoc-session`.

## Project Structure

```
camlukah/
├── bin/
│   ├── main.ml              # CLI entry point
│   └── commands/            # Command implementations
│       ├── solve.ml         # Solve command
│       ├── test.ml          # Test command
│       ├── generate.ml      # Generate command
│       └── download.ml      # Download command
├── lib/
│   ├── day.ml               # Day.Solver interface
│   ├── days/                # Solution implementations
│   │   ├── day01.ml
│   │   ├── day02.ml
│   │   └── ...
│   └── utils/               # Utility modules
│       ├── input.ml         # File I/O
│       ├── session.ml       # Session management
│       ├── aoc_client.ml    # HTTP client
│       └── html_formatter.ml # HTML to terminal
├── test/
│   ├── test_runner.ml       # Test dispatcher
│   └── day01_test.ml        # Day 1 tests
├── inputs/                  # Puzzle inputs (gitignored)
│   └── day01.txt
└── dune-project
```

## Writing Solutions

Each day implements the `Day.Solver` interface:

```ocaml
module type Solver = sig
  type t
  val parse_input : string -> t
  val solve_part1 : t -> string
  val solve_part2 : t -> string
end
```

Example implementation:

```ocaml
(** Day 1: Not Quite Lisp *)

type t = string

let parse_input input =
  String.trim input

let solve_part1 input =
  let floor = String.fold_left (fun acc c ->
    match c with
    | '(' -> acc + 1
    | ')' -> acc - 1
    | _ -> acc
  ) 0 input in
  string_of_int floor

let solve_part2 input =
  let rec find_basement pos floor =
    if floor = -1 then pos
    else if pos >= String.length input then -1
    else
      let new_floor = match input.[pos] with
        | '(' -> floor + 1
        | ')' -> floor - 1
        | _ -> floor
      in
      find_basement (pos + 1) new_floor
  in
  string_of_int (find_basement 0 0)
```

## Helper Functions

The `Helpers` module provides common utilities. Add `open Helpers` at the top of your day files:

```ocaml
(** Day 5: Example *)

open Helpers

type t = unit

let parse_input _input = todo ()

let solve_part1 _input =
  todo_msg "implement part 1"
  |> tap (Printf.printf "Working on: %s\n")
```

**Available helper functions:**
- `todo ()` - placeholder for unimplemented code
- `todo_msg "message"` - todo with custom message
- `tap f x` - apply function for side effects, return value
- `flip f x y` - flip function arguments
- `id x` - identity function
- `const x _` - constant function

All generated day templates automatically include `open Helpers`.

## Writing Tests

Tests use OUnit2 with `Test_helpers` for better error messages:

```ocaml
open OUnit2
open Test_helpers

let test_part1 _ =
  let input = Camlukah.Day01.parse_input "(())" in
  assert_string_equal "0" (Camlukah.Day01.solve_part1 input);

  let input = Camlukah.Day01.parse_input "(((" in
  assert_string_equal "3" (Camlukah.Day01.solve_part1 input)

let test_part2 _ =
  let input = Camlukah.Day01.parse_input ")" in
  assert_string_equal "1" (Camlukah.Day01.solve_part2 input)

let suite =
  "Day01Tests" >::: [
    "test_part1" >:: test_part1;
    "test_part2" >:: test_part2;
  ]
```

When tests fail, you'll see helpful output like:
```
expected: 3
but got:  5
```

## Tips

- Use `dune build --watch` in one terminal for continuous compilation
- Run executables directly: `_build/default/bin/main.exe solve 1`
- Puzzle inputs go in `inputs/dayXX.txt` (auto-downloaded)

## License

MIT

## Acknowledgments

Inspired by:
- [Advent of Code](https://adventofcode.com/) by Eric Wastl
- Various AOC templates in the community
