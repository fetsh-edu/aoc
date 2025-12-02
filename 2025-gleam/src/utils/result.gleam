import gleam/result

pub fn assert_ok(a: Result(a, b)) -> a {
  let assert Ok(b) = a
  b
}

pub fn map2(
  result_a: Result(a, e),
  result_b: Result(b, e),
  fun: fn(a, b) -> c,
) -> Result(c, e) {
  use a <- result.try(result_a)
  use b <- result.try(result_b)
  Ok(fun(a, b))
}
