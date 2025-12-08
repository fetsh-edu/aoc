import days/day08
import gleeunit/should
import utils/input

pub fn part1_test() {
  let assert Ok(input) = input.read_test_input(8)

  input
  |> day08.prepare_input
  |> day08.solve_part1(10)
  |> should.equal(40)
}
// pub fn part2_test() {
//   let assert Ok(input) = input.read_test_input(8)

//   input
//   |> day08.prepare_input
//   |> day08.solve_part2
//   |> should.equal(100)
// }
