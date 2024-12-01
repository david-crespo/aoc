import gleam/int
import gleam/io
import gleam/list
import gleam/regexp
import gleam/string
import util

pub fn part1() {
  let assert Ok(spaces) = regexp.from_string(" +")

  let #(list1, list2) =
    util.get_input(day: 1)
    |> list.filter(fn(line) { !string.is_empty(line) })
    |> list.map(fn(line) {
      let assert [Ok(a), Ok(b)] =
        regexp.split(with: spaces, content: line) |> list.map(int.parse)
      #(a, b)
    })
    |> list.unzip

  list.zip(list.sort(list1, by: int.compare), list.sort(list2, by: int.compare))
  |> list.map(fn(pair) { int.absolute_value(pair.0 - pair.1) })
  |> int.sum
  |> io.debug
}
