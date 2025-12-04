import days/day04
import gleeunit/should
import utils/input

pub fn part1_test() {
  let assert Ok(input) = input.read_test_input(4)

  input
  |> day04.prepare_input
  |> day04.solve_part1
  |> should.equal(13)
}

pub fn part2_test() {
  let assert Ok(input) = input.read_test_input(4)

  input
  |> day04.prepare_input
  |> day04.solve_part2
  |> should.equal(43)
}
