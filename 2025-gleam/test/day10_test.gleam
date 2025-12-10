import days/day10
import gleeunit/should
import utils/input

pub fn part1_test() {
  let assert Ok(input) = input.read_test_input(10)

  input
  |> day10.prepare_input
  |> day10.solve_part1
  |> should.equal(7)
}
// pub fn part2_test() {
//   let assert Ok(input) = input.read_test_input(10)

//   input
//   |> day10.prepare_input
//   |> day10.solve_part2
//   |> should.equal(100)
// }
