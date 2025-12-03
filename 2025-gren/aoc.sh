#!/bin/bash

# Advent of Code Gren Runner - Convenience Script
# Automatically compiles and runs tests or solutions

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

error() {
    echo -e "${RED}Error: $1${NC}" >&2
    exit 1
}

info() {
    echo -e "${YELLOW}$1${NC}"
}

success() {
    echo -e "${GREEN}$1${NC}"
}

# Check if we're in the right directory
if [ ! -f "gren.json" ]; then
    error "Must be run from project root (where gren.json is)"
fi

# Parse command
COMMAND="${1:-help}"

case "$COMMAND" in
    test)
        info "Compiling tests..."
        cd tests
        gren make Main --output=run_tests || error "Test compilation failed"
        chmod +x run_tests
        echo ""
        info "Running tests..."
        ./run_tests
        cd ..
        ;;

    solve)
        if [ -z "$2" ]; then
            error "Usage: ./aoc.sh solve <day>"
        fi

        DAY="$2"
        info "Compiling main application..."
        gren make Main --output=aoc || error "Compilation failed"
        chmod +x aoc
        echo ""
        info "Solving Day $DAY..."
        ./aoc solve "$DAY"
        ;;

    help|--help|-h)
        cat << 'EOF'
Advent of Code Gren Runner - Convenience Script

Usage:
  ./aoc.sh test           Compile and run all tests
  ./aoc.sh solve <day>    Compile and solve a specific day
  ./aoc.sh help           Show this help message

Examples:
  ./aoc.sh test           # Compile tests and run them
  ./aoc.sh solve 1        # Compile and solve day 1
  ./aoc.sh solve 15       # Compile and solve day 15

This script automatically:
- Compiles the necessary code before running
- Sets executable permissions
- Shows compilation status with colors

Manual compilation (if needed):
  gren make Main --output=aoc           # Compile main app
  cd tests && gren make Main --output=run_tests  # Compile tests
EOF
        ;;

    *)
        error "Unknown command: $COMMAND\n\nRun './aoc.sh help' for usage information"
        ;;
esac
