import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import utils/input

pub fn solve() -> Result(#(String, String), String) {
  use input <- result.try(input.read_input(8))

  let input_prepared = prepare_input(input)

  let part1 = solve_part1(input_prepared, 1000)
  let part2 = solve_part2(input_prepared)

  Ok(#(int.to_string(part1), int.to_string(part2)))
}

pub fn prepare_input(input: String) -> List(Box) {
  input |> string.trim() |> string.split("\n") |> list.index_map(parse_box)
}

pub fn solve_part1(input: List(Box), connections: Int) -> Int {
  input
  |> list.combination_pairs
  |> list.map(fn(bb) {
    let #(b1, b2) = bb
    Connection(from: b1.index, to: b2.index, distance: distance_squared(b1, b2))
  })
  |> list.sort(by: fn(c1, c2) { float.compare(c1.distance, c2.distance) })
  |> list.take(connections)
  |> list.fold(from: init_circuits(input), with: circuits_merge)
  |> fn(circuits) { circuits.sizes }
  |> dict.values
  |> list.sort(int.compare)
  |> list.reverse
  |> list.take(3)
  |> int.product
}

pub fn solve_part2(input: List(Box)) -> Int {
  input
  |> list.combination_pairs
  |> list.map(fn(bb) {
    let #(b1, b2) = bb
    Connection(from: b1.index, to: b2.index, distance: distance_squared(b1, b2))
  })
  |> list.sort(by: fn(c1, c2) { float.compare(c1.distance, c2.distance) })
  |> find_last_connection(init_circuits(input))
  |> fn(connection) {
    let Connection(from, to, _) = connection
    let assert Ok(box1) = input |> list.find(fn(b) { b.index == from })
    let assert Ok(box2) = input |> list.find(fn(b) { b.index == to })
    box1.x * box2.x
  }
}

pub type Box {
  Box(x: Int, y: Int, z: Int, index: BoxId)
}

type Connection {
  Connection(from: Int, to: Int, distance: Float)
}

type BoxId =
  Int

type Circuits {
  Circuits(parents: Dict(BoxId, BoxId), sizes: Dict(BoxId, Int))
}

fn init_circuits(boxes: List(Box)) -> Circuits {
  let parent =
    boxes
    |> list.fold(from: dict.new(), with: fn(acc, i) {
      dict.insert(acc, i.index, i.index)
    })

  let size =
    boxes
    |> list.fold(from: dict.new(), with: fn(acc, i) {
      dict.insert(acc, i.index, 1)
    })

  Circuits(parent, size)
}

fn get_root(circuits: Circuits, node_id: BoxId) -> #(Circuits, BoxId) {
  let assert Ok(parent_id) = circuits.parents |> dict.get(node_id)

  case node_id == parent_id {
    True -> #(circuits, parent_id)
    False -> {
      let #(final_circuits, root_id) = get_root(circuits, parent_id)
      let circuits_with_shortcut =
        Circuits(
          ..circuits,
          parents: dict.insert(final_circuits.parents, node_id, root_id),
        )
      #(circuits_with_shortcut, root_id)
    }
  }
}

fn circuits_merge(circuits: Circuits, conn: Connection) -> Circuits {
  let Connection(from, to, _) = conn

  let #(circuits1, from_root) = get_root(circuits, from)
  let #(circuits2, to_root) = get_root(circuits1, to)

  case from_root == to_root {
    True -> circuits2
    False -> {
      let Circuits(parents:, sizes:) = circuits2
      let assert Ok(size_from) = dict.get(sizes, from_root)
      let assert Ok(size_to) = dict.get(sizes, to_root)

      let #(big_root, small_root) = case size_from >= size_to {
        True -> #(from_root, to_root)
        False -> #(to_root, from_root)
      }

      Circuits(
        dict.insert(parents, small_root, big_root),
        sizes
          |> dict.insert(big_root, { size_to + size_from })
          |> dict.delete(small_root),
      )
    }
  }
}

fn find_last_connection(
  connections: List(Connection),
  circuits: Circuits,
) -> Connection {
  case connections {
    [] -> panic as "No connection connects all boxes"

    [conn, ..rest] -> {
      let circuits2 = circuits_merge(circuits, conn)
      let components = dict.size(circuits2.sizes)

      case components == 1 {
        True -> conn
        False -> find_last_connection(rest, circuits2)
      }
    }
  }
}

fn parse_box(input: String, index: Int) -> Box {
  let assert [Ok(x), Ok(y), Ok(z)] =
    string.split(input, ",")
    |> list.map(string.trim)
    |> list.map(int.parse)

  Box(x, y, z, index)
}

pub fn distance_squared(a: Box, b: Box) -> Float {
  let dx = int.to_float(a.x - b.x)
  let dy = int.to_float(a.y - b.y)
  let dz = int.to_float(a.z - b.z)

  dx *. dx +. dy *. dy +. dz *. dz
}
