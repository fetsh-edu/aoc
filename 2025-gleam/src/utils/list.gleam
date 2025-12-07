import gleam/list

pub fn zip_with(
  list: List(a),
  with other: List(b),
  apply fun: fn(a, b) -> c,
) -> List(c) {
  zip_with_loop(list, other, fun, [])
}

fn zip_with_loop(
  one: List(a),
  other: List(b),
  fun: fn(a, b) -> c,
  acc: List(c),
) -> List(c) {
  case one, other {
    [first_one, ..rest_one], [first_other, ..rest_other] ->
      zip_with_loop(rest_one, rest_other, fun, [
        fun(first_one, first_other),
        ..acc
      ])
    _, _ -> list.reverse(acc)
  }
}
