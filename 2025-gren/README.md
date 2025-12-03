# Advent of Code Gren Runner

A CLI runner for solving Advent of Code puzzles using the Gren programming language.

## Features

- **Convenience script**: `./aoc.sh` automatically compiles and runs
- Simple CLI interface: `./aoc.sh solve <day>` or `./aoc.sh test`
- Proper unit testing with `gren-lang/test`
- Organized project structure with separate folders for solutions, inputs, and tests
- Easy to extend - just add solution file, input file, and update tests

## Prerequisites

Before you can use this runner, you need to install Gren:

### Installing Gren

1. **Install Node.js** (if not already installed)
   - Download from [nodejs.org](https://nodejs.org/) or use your package manager
   - Verify installation: `node --version`

2. **Install Gren**
   ```bash
   npm install -g gren-lang
   ```

3. **Verify Gren installation**
   ```bash
   gren --version
   ```

## Project Setup

1. **Clone or navigate to this directory**
   ```bash
   cd /path/to/2025-gren
   ```

2. **Install Gren dependencies**
   ```bash
   gren package install
   ```

3. **Make the convenience script executable**
   ```bash
   chmod +x aoc.sh
   ```

That's it! The `aoc.sh` script will handle all compilation automatically.

## Project Structure

```
2025-gren/
├── gren.json                 # Gren project configuration
├── aoc.sh                    # Convenience script (use this!)
├── aoc                       # Compiled executable (auto-created)
├── README.md                 # This file
│
├── src/                      # Core runner code
│   └── Main.gren            # CLI entry point and day loader
│
├── days/                     # Your puzzle solutions
│   ├── Day01.gren           # Solution for day 1
│   ├── Day02.gren           # Solution for day 2
│   └── ...                   # Solutions for other days
│
├── input/                    # Actual puzzle inputs
│   ├── day01.txt            # Input for day 1
│   ├── day02.txt            # Input for day 2
│   └── ...                   # Inputs for other days
│
└── tests/                    # Unit tests
    ├── gren.json            # Test project configuration
    ├── run_tests            # Compiled test executable (auto-created)
    └── src/
        ├── Main.gren        # Test suite runner
        └── Test/
            ├── Day01.gren   # Day 1 tests
            ├── Day02.gren   # Day 2 tests
            └── ...          # Tests for other days
```

## Usage

The `aoc.sh` script automatically compiles and runs everything for you!

### Running Solutions

To solve a puzzle with actual input:
```bash
./aoc.sh solve 1
```

This will:
1. Compile the main application
2. Run the solver for day 1
3. Show the results

### Running Tests

To run all unit tests:
```bash
./aoc.sh test
```

This will:
1. Compile the test suite
2. Run all tests
3. Show pass/fail results

### Getting Help

```bash
./aoc.sh help
```

### Manual Compilation (Optional)

If you prefer to compile manually:
```bash
# Compile main app
gren make Main --output=aoc
chmod +x aoc

# Compile tests
cd tests
gren make Main --output=run_tests
chmod +x run_tests
```

## Adding a New Day

When you start a new Advent of Code puzzle, you need to:

### 1. Create the Solution Module: `days/Day{NN}.gren`

```gren
module Day{NN} exposing (solve)


solve : String -> { part1 : String, part2 : String }
solve input =
    let
        -- Parse your input here
        parsed = parseInput input
    in
    { part1 = solvePart1 parsed
    , part2 = solvePart2 parsed
    }


parseInput : String -> YourDataType
parseInput input =
    -- Your parsing logic
    {}


solvePart1 : YourDataType -> String
solvePart1 data =
    -- Your part 1 solution
    "Not implemented"


solvePart2 : YourDataType -> String
solvePart2 data =
    -- Your part 2 solution
    "Not implemented"
```

### 2. Add the Actual Puzzle Input: `input/day{NN}.txt`

Copy the puzzle input from the Advent of Code website:
```bash
# Download from https://adventofcode.com/2025/day/{N}/input
# Save to input/day{NN}.txt
```

### 3. Register the Day in Main

Edit `src/Main.gren` and add your new day:

```gren
-- Add import at the top (after the other Day imports, around line 11)
import Day{NN}

-- Add case in loadDayModule function (around line 120)
loadDayModule : Int -> Maybe (String -> { part1 : String, part2 : String })
loadDayModule day =
    when day is
        1 ->
            Just Day01.solve

        {N} ->
            Just Day{NN}.solve

        _ ->
            Nothing
```

### 4. Add Unit Tests

Create `tests/src/Test/Day{NN}.gren`:

```gren
module Test.Day{NN} exposing (tests)

import Test exposing (Test, describe, test)
import Expect
import Day{NN}


tests : Test
tests =
    describe "Day {NN}"
        [ test "Part 1 with example input" <|
            \_ ->
                let
                    input =
                        """
your example input here
"""
                    result =
                        Day{NN}.solve input
                in
                Expect.equal "expected part 1" result.part1
        , test "Part 2 with example input" <|
            \_ ->
                let
                    input =
                        """
your example input here
"""
                    result =
                        Day{NN}.solve input
                in
                Expect.equal "expected part 2" result.part2
        ]
```

Then edit `tests/src/Main.gren` to add the test module:

```gren
-- Add import at the top
import Test.Day{NN} as TD{N}

-- Add to the suite list
suite : Test
suite =
    describe "Advent of Code 2025"
        [ TD1.tests
        , TD{N}.tests
        ]
```

### 5. Test and Solve

```bash
# Run tests (automatically compiles)
./aoc.sh test

# Solve the puzzle (automatically compiles)
./aoc.sh solve 2
```

## Tips

- Always write unit tests first using the example input from the puzzle
- Run tests frequently: `./aoc.sh test`
- The `solve` function must return a record with `part1` and `part2` fields (both Strings)
- Keep your input files clean - remove trailing whitespace if needed
- Use Gren's `String.lines`, `String.words`, and `String.toInt` for parsing
- The `aoc.sh` script handles all compilation automatically - no need to manually recompile!

## Troubleshooting

### "Could not find module for day X"
- Make sure you added the day case in `src/Main.gren`
- Verify the import statement is at the top of `src/Main.gren` (after other imports)
- Run `./aoc.sh solve X` again (it will recompile automatically)

### "Could not read input file"
- Check that the input file exists in the correct location
- Verify the filename matches the pattern `day{NN}.txt` with zero-padding (e.g., `day01.txt`)
- Make sure you're running from the project root directory

### Compilation errors
- Ensure all dependencies are installed: `gren package install`
- Check that your Day module has the correct signature: `solve : String -> { part1 : String, part2 : String }`
- Verify all imports are correct

## License

This is a personal Advent of Code runner. Use it however you want!
