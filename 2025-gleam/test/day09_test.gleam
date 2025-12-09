import days/day09
import gleeunit/should
import utils/input

pub fn part1_test() {
  let assert Ok(input) = input.read_test_input(9)

  input
  |> day09.prepare_input
  |> day09.solve_part1
  |> should.equal(50)
}

pub fn part2_test() {
  let assert Ok(input) = input.read_test_input(9)

  input
  |> day09.prepare_input
  |> day09.solve_part2
  |> should.equal(24)
}
