import days/day03
import gleeunit/should
import utils/input

pub fn part1_test() {
  let assert Ok(input) = input.read_test_input(3)

  input
  |> day03.prepare_input
  |> day03.solve_part1
  |> should.equal(357)
}

pub fn part2_test() {
  let assert Ok(input) = input.read_test_input(3)

  input
  |> day03.prepare_input
  |> day03.solve_part2
  |> should.equal(3_121_910_778_619)
}
