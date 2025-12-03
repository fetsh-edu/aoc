# Quick Start Guide

## First Time Setup

```bash
# Install Gren globally
npm install -g gren-lang

# Install dependencies
gren package install

# Make the script executable
chmod +x aoc.sh
```

## Daily Workflow

### 1. Read the puzzle and get your input
Visit https://adventofcode.com/2025/day/X

### 2. Create your solution file
```bash
# Copy the template
cp days/Day01.gren days/Day02.gren
# Edit days/Day02.gren with your solution
```

### 3. Save your input
```bash
# Save to input/day02.txt
curl https://adventofcode.com/2025/day/2/input -H "cookie: session=YOUR_SESSION" > input/day02.txt
```

### 4. Add tests
Edit `tests/src/Main.gren` and add your test cases with the example input.

### 5. Register the day
Edit `src/Main.gren`:
- Add `import Day02` at the top
- Add the case in `loadDayModule` function

### 6. Test and solve!
```bash
# Run tests
./aoc.sh test

# Solve the puzzle
./aoc.sh solve 2
```

## Common Commands

```bash
./aoc.sh test        # Run all tests
./aoc.sh solve 1     # Solve day 1
./aoc.sh help        # Show help
```

## Tips

- Write tests first with the example input
- Test frequently as you develop
- The script auto-compiles everything for you
- No need to manually recompile!

## Example Test

```gren
day02Tests : Test
day02Tests =
    describe "Day 02"
        [ test "Part 1 with example input" <|
            \_ ->
                Expect.equal "expected" (Day02.solve exampleInput).part1
        ]
```

That's it! Happy coding! 🎄
