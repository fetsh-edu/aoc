pub fn not(value: Bool) -> Bool {
  !value
}

pub fn echo_with(over: a, with: fn(a) -> b) -> a {
  over |> with |> echo
  over
}
