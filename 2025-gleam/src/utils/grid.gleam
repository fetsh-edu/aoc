import gleam/dict.{type Dict}
import gleam/list
import utils/dict as d

pub type Grid(a) {
  Grid(cells: Dict(#(Int, Int), a))
}

pub fn from_rows_where(rows: List(List(a)), keep: fn(a) -> Bool) -> Grid(a) {
  let cells =
    rows
    |> list.index_map(fn(row, r) {
      row
      |> list.index_map(fn(cell, c) {
        case keep(cell) {
          True -> Ok(#(#(r, c), cell))
          False -> Error(Nil)
        }
      })
      |> list.filter_map(fn(x) { x })
    })
    |> list.flatten()
    |> dict.from_list()

  Grid(cells)
}

pub fn count_neighbours8(grid: Grid(a), row: Int, col: Int) -> Int {
  [
    #(-1, -1),
    #(-1, 0),
    #(-1, 1),
    #(0, -1),
    #(0, 1),
    #(1, -1),
    #(1, 0),
    #(1, 1),
  ]
  |> list.count(fn(delta) {
    let #(dr, dc) = delta
    dict.has_key(grid.cells, #(row + dr, col + dc))
  })
}

pub fn has_fewer_neighbours8(
  grid: Grid(a),
  pos: #(Int, Int),
  threshold: Int,
) -> Bool {
  let #(row, col) = pos
  count_neighbours8(grid, row, col) < threshold
}

pub fn count_keys(
  grid: Grid(a),
  where predicate: fn(#(Int, Int)) -> Bool,
) -> Int {
  dict.keys(grid.cells) |> list.count(predicate)
}

pub fn partition(
  grid: Grid(a),
  predicate: fn(#(Int, Int), a) -> Bool,
) -> #(Grid(a), Grid(a)) {
  let #(left, right) = grid.cells |> d.partition(predicate)
  #(Grid(left), Grid(right))
}

pub fn size(grid: Grid(a)) -> Int {
  dict.size(grid.cells)
}
// pub fn count_neighbours8_where(
//   grid: Grid(a),
//   row: Int,
//   col: Int,
//   predicate: fn(a) -> Bool,
// ) -> Int {
//   [
//     #(-1, -1),
//     #(-1, 0),
//     #(-1, 1),
//     #(0, -1),
//     #(0, 1),
//     #(1, -1),
//     #(1, 0),
//     #(1, 1),
//   ]
//   |> list.count(fn(delta) {
//     let #(dr, dc) = delta
//     case dict.get(grid.cells, #(row + dr, col + dc)) {
//       Ok(value) -> predicate(value)
//       Error(Nil) -> False
//     }
//   })
// }
