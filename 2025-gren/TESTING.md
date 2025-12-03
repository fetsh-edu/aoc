# Testing Guide

This project uses `gren-lang/test` for unit testing.

## Running Tests

From the project root:

```bash
cd tests && ./run_tests
```

## Test Structure

Tests are organized in modules under `tests/src/Test/`:

```
tests/src/
├── Main.gren          # Test runner (imports all test modules)
└── Test/
    ├── Day01.gren     # Day 1 tests
    ├── Day02.gren     # Day 2 tests
    └── ...            # More day tests
```

The main test runner imports all test modules:

```gren
import Test exposing (Test, describe)
import Test.Day01 as TD1
import Test.Day02 as TD2

suite : Test
suite =
    describe "Advent of Code 2025"
        [ TD1.tests
        , TD2.tests
        ]
```

## Writing Tests for a New Day

When you add a new day (e.g., Day 02):

### 1. Create the test module `tests/src/Test/Day02.gren`:

```gren
module Test.Day02 exposing (tests)

import Test exposing (Test, describe, test)
import Expect
import Day02


tests : Test
tests =
    describe "Day 02"
        [ test "Part 1 with example input" <|
            \_ ->
                let
                    input =
                        """
1
2
3
"""
                    result =
                        Day02.solve input
                in
                Expect.equal "6" result.part1
        , test "Part 2 with example input" <|
            \_ ->
                let
                    input =
                        """
1
2
3
"""
                    result =
                        Day02.solve input
                in
                Expect.equal "6" result.part2
        ]
```

### 2. Register in `tests/src/Main.gren`:

```gren
-- Add import
import Test.Day02 as TD2

-- Add to suite
suite : Test
suite =
    describe "Advent of Code 2025"
        [ TD1.tests
        , TD2.tests
        ]
```

## Recompiling Tests

After updating tests:

```bash
cd tests
gren make Main --output=run_tests
chmod +x run_tests
./run_tests
```

## Test Framework Features

The `gren-lang/test` framework provides:

- `test` - Define a single test case
- `describe` - Group related tests
- `Expect.equal` - Assert equality
- `Expect.notEqual` - Assert inequality
- `fuzz` - Property-based testing with random inputs

See [gren-lang/test documentation](https://github.com/gren-lang/test) for more details.

## Best Practices

1. **Test with example input first**: Use the example input from the puzzle description
2. **Write tests before solving**: Follow TDD principles
3. **Test edge cases**: Empty input, single line, large numbers, etc.
4. **Run tests frequently**: Quick feedback loop
5. **Keep tests readable**: Use descriptive test names and clear assertions
