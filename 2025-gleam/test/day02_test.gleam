import days/day02
import gleeunit/should
import utils/input

pub fn part1_test() {
  let assert Ok(input) = input.read_test_input(2)

  input
  |> day02.prepare_input
  |> day02.solve_part1
  |> should.equal(1_227_775_554)
}

pub fn part2_test() {
  let assert Ok(input) = input.read_test_input(2)

  input
  |> day02.prepare_input
  |> day02.solve_part2
  |> should.equal(4_174_379_265)
}
