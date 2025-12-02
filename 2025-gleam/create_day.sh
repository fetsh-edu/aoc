#!/bin/bash

# Check if day number argument is provided
if [ -z "$1" ]; then
  echo "Usage: ./create_day.sh <day_number>"
  echo "       ./create_day.sh undo <day_number>"
  echo "Example: ./create_day.sh 2"
  echo "         ./create_day.sh undo 2"
  exit 1
fi

# Check for undo action
if [ "$1" = "undo" ]; then
  if [ -z "$2" ]; then
    echo "Error: Please specify day number to undo"
    echo "Usage: ./create_day.sh undo <day_number>"
    exit 1
  fi

  DAY=$2
  DAY_PADDED=$(printf "%02d" $DAY)

  # File paths
  DAY_FILE="src/days/day${DAY_PADDED}.gleam"
  TEST_FILE="test/day${DAY_PADDED}_test.gleam"
  TEST_INPUT="test/test_inputs/day${DAY_PADDED}.txt"
  INPUT="inputs/day${DAY_PADDED}.txt"
  MAIN_FILE="src/advent_of_code.gleam"

  echo "Removing scaffold for day $DAY:"

  # Remove files if they exist
  [ -f "$DAY_FILE" ] && rm "$DAY_FILE" && echo "  - Removed $DAY_FILE"
  [ -f "$TEST_FILE" ] && rm "$TEST_FILE" && echo "  - Removed $TEST_FILE"
  [ -f "$TEST_INPUT" ] && rm "$TEST_INPUT" && echo "  - Removed $TEST_INPUT"
  [ -f "$INPUT" ] && rm "$INPUT" && echo "  - Removed $INPUT"

  # Remove import from advent_of_code.gleam
  sed -i "/^import days\/day${DAY_PADDED}$/d" "$MAIN_FILE"

  # Remove case branch from advent_of_code.gleam
  sed -i "/^    ${DAY} -> day${DAY_PADDED}\.solve()$/d" "$MAIN_FILE"

  echo "  - Updated $MAIN_FILE (removed import and case branch)"
  echo "Undo complete!"
  exit 0
fi

DAY=$1
DAY_PADDED=$(printf "%02d" $DAY)

# File paths
DAY_FILE="src/days/day${DAY_PADDED}.gleam"
TEST_FILE="test/day${DAY_PADDED}_test.gleam"
TEST_INPUT="test/test_inputs/day${DAY_PADDED}.txt"
INPUT="inputs/day${DAY_PADDED}.txt"
MAIN_FILE="src/advent_of_code.gleam"

# Check if files already exist
if [ -f "$DAY_FILE" ]; then
  echo "Error: $DAY_FILE already exists"
  exit 1
fi

if [ -f "$TEST_FILE" ]; then
  echo "Error: $TEST_FILE already exists"
  exit 1
fi

# Create day source file
cat > "$DAY_FILE" << 'EOF'
import gleam/int
import gleam/result
import utils/input

pub fn solve() -> Result(#(String, String), String) {
  use input <- result.try(input.read_input(DAY_NUM))

  let part1 = solve_part1(input)
  let part2 = solve_part2(input)

  Ok(#(int.to_string(part1), int.to_string(part2)))
}

pub fn solve_part1(input: String) -> Int {
  0
}

pub fn solve_part2(input: String) -> Int {
  0
}
EOF

# Replace DAY_NUM placeholder with actual day number
sed -i "s/DAY_NUM/$DAY/g" "$DAY_FILE"

# Create test file
cat > "$TEST_FILE" << 'EOF'
import days/dayDAY_PADDED
import gleeunit/should
import utils/input

pub fn part1_test() {
  let assert Ok(input) = input.read_test_input(DAY_NUM)

  input
  |> dayDAY_PADDED.solve_part1
  |> should.equal(42)
}

pub fn part2_test() {
  let assert Ok(input) = input.read_test_input(DAY_NUM)

  input
  |> dayDAY_PADDED.solve_part2
  |> should.equal(100)
}
EOF

# Replace placeholders
sed -i "s/DAY_NUM/$DAY/g" "$TEST_FILE"
sed -i "s/DAY_PADDED/$DAY_PADDED/g" "$TEST_FILE"

# Create empty input files
touch "$TEST_INPUT"
touch "$INPUT"

# Update advent_of_code.gleam
# Add import after the last day import
LAST_DAY_IMPORT=$(grep -n "^import days/day" "$MAIN_FILE" | tail -1 | cut -d: -f1)
sed -i "${LAST_DAY_IMPORT}a\\import days/day${DAY_PADDED}" "$MAIN_FILE"

# Add case branch before the default case using awk
# Only match the _ case that has Error (which is in solve_day function)
awk -v day="$DAY" -v day_padded="$DAY_PADDED" '
  /^    _ -> Error/ {
    print "    " day " -> day" day_padded ".solve()"
  }
  { print }
' "$MAIN_FILE" > "$MAIN_FILE.tmp" && mv "$MAIN_FILE.tmp" "$MAIN_FILE"

echo "Created scaffold for day $DAY:"
echo "  - $DAY_FILE"
echo "  - $TEST_FILE"
echo "  - $TEST_INPUT"
echo "  - $INPUT"
echo "  - Updated $MAIN_FILE with import and case branch"
