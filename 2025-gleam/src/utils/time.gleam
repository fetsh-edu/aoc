import gleam/int
import tempo/duration
import tempo/instant

pub fn measure(fun: fn() -> a) -> #(a, Int) {
  let timer = instant.now()
  let result = fun()
  let diff = timer |> instant.since |> duration.as_microseconds

  #(result, diff)
}

pub fn measure_echo(fun: fn() -> a) -> a {
  let timer = instant.now()
  let result = fun()
  let diff = timer |> instant.since |> duration.as_microseconds
  echo { "Took: " <> int.to_string(diff) <> " microseconds" }
  result
}
