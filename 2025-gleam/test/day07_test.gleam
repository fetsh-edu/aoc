import days/day07
import gleeunit/should
import utils/input

pub fn part1_test() {
  let assert Ok(input) = input.read_test_input(7)

  input
  |> day07.prepare_input
  |> day07.solve_part1
  |> should.equal(21)
}

pub fn part2_test() {
  let assert Ok(input) = input.read_test_input(7)

  input
  |> day07.prepare_input
  |> day07.solve_part2
  |> should.equal(40)
}
