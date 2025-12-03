import gleam/int

pub fn parse_assert(s: String) -> Int {
  let assert Ok(i) = int.parse(s)
  i
}
