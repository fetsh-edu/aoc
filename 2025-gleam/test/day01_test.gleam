import days/day01
import gleeunit/should
import utils/input

pub fn part1_test() {
  let assert Ok(input) = input.read_test_input(1)

  input
  |> day01.parse_rotations
  |> day01.solve_part1
  |> should.equal(3)
}

pub fn part2_test() {
  let assert Ok(input) = input.read_test_input(1)

  input
  |> day01.parse_rotations
  |> day01.solve_part2
  |> should.equal(6)
}
