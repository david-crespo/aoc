import gleam/int
import gleam/io
import gleam/list
import gleam/regexp
import gleam/string
import util

fn get_lists() {
  let assert Ok(spaces) = regexp.from_string(" +")
  util.get_input_lines(day: 1)
  |> list.filter(fn(line) { !string.is_empty(line) })
  |> list.map(fn(line) {
    let assert [Ok(a), Ok(b)] =
      regexp.split(with: spaces, content: line) |> list.map(int.parse)
    #(a, b)
  })
  |> list.unzip
}

pub fn part1() {
  let #(list1, list2) = get_lists()

  list.zip(list.sort(list1, by: int.compare), list.sort(list2, by: int.compare))
  |> list.map(fn(pair) { int.absolute_value(pair.0 - pair.1) })
  |> int.sum
  |> io.debug
}

pub fn part2() {
  let #(list1, list2) = get_lists()

  list.map(list1, fn(a) { a * list.count(list2, fn(b) { b == a }) })
  |> int.sum
  |> io.debug
}
