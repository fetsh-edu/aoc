import days/day11
import gleeunit/should
import utils/input

pub fn part1_test() {
  let assert Ok(input) = input.read_test_input(111)

  input
  |> day11.prepare_input
  |> day11.solve_part1
  |> should.equal(5)
}

pub fn part2_test() {
  let assert Ok(input) = input.read_test_input(112)

  input
  |> day11.prepare_input
  |> day11.solve_part2
  |> should.equal(2)
}
