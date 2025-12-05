import days/day05
import gleeunit/should
import utils/input

pub fn part1_test() {
  let assert Ok(input) = input.read_test_input(5)

  input
  |> day05.prepare_input
  |> day05.solve_part1
  |> should.equal(3)
}

pub fn part2_test() {
  let assert Ok(input) = input.read_test_input(5)

  input
  |> day05.prepare_input
  |> day05.solve_part2
  |> should.equal(14)
}
