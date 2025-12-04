import gleam/dict.{type Dict}

pub fn partition(
  dict: Dict(k, v),
  predicate: fn(k, v) -> Bool,
) -> #(Dict(k, v), Dict(k, v)) {
  dict.fold(dict, #(dict.new(), dict.new()), fn(acc, key, value) {
    let #(true_dict, false_dict) = acc
    case predicate(key, value) {
      True -> #(dict.insert(true_dict, key, value), false_dict)
      False -> #(true_dict, dict.insert(false_dict, key, value))
    }
  })
}
