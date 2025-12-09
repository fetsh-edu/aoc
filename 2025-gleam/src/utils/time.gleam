import tempo/duration
import tempo/instant

pub fn measure(fun: fn() -> a) -> #(a, Int) {
  let timer = instant.now()
  let result = fun()
  let diff = timer |> instant.since |> duration.as_microseconds

  #(result, diff)
}
