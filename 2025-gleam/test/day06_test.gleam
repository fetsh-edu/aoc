import days/day06
import gleeunit/should
import utils/input

pub fn part1_test() {
  let assert Ok(input) = input.read_test_input(6)

  input
  |> day06.prepare_input
  |> day06.solve_part1
  |> should.equal(4_277_556)
}

pub fn part2_test() {
  let assert Ok(input) = input.read_test_input(6)

  input
  |> day06.prepare_input
  |> day06.solve_part2
  |> should.equal(3_263_827)
}
