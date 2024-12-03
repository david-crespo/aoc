import gleam/int
import gleam/io
import gleam/list
import gleam/pair
import gleam/result
import gleam/string

import util

// const example = "
// 7 6 4 2 1
// 1 2 7 8 9
// 9 7 6 2 1
// 1 3 2 4 5
// 8 6 4 4 1
// 1 3 6 7 9
// "

fn get_rows(lines: List(String)) {
  list.map(lines, fn(line) {
    line
    |> string.split(" ")
    |> list.map(fn(n) { int.parse(n) |> result.unwrap(0) })
  })
}

fn get_diffs(row: List(Int)) {
  row |> list.window_by_2 |> list.map(fn(pair) { pair.1 - pair.0 })
}

fn is_safe(diffs: List(Int)) {
  { list.all(diffs, fn(d) { d > 0 }) || list.all(diffs, fn(d) { d < 0 }) }
  && {
    list.map(diffs, int.absolute_value)
    |> list.all(fn(d) { 1 <= d && d <= 3 })
  }
}

pub fn part1() {
  let lines = util.get_input_lines(day: 2)
  // let lines = example |> string.trim |> string.split("\n")

  get_rows(lines)
  |> list.map(get_diffs)
  |> list.count(is_safe)
  |> io.debug
}

// there has to be a better way to do this. index_fold was worse
fn remove_at(lst: List(item), i: Int) -> List(item) {
  lst
  |> list.index_map(fn(x, j) { #(j != i, x) })
  |> list.filter(pair.first)
  |> list.map(pair.second)
}

pub fn part2() {
  let lines = util.get_input_lines(day: 2)
  // let lines = example |> string.trim |> string.split("\n")

  get_rows(lines)
  |> list.count(fn(row) {
    is_safe(get_diffs(row))
    || {
      list.index_map(row, fn(_item, i) { i })
      |> list.map(fn(i) { get_diffs(remove_at(row, i)) })
      |> list.any(is_safe)
    }
  })
  |> io.debug
}
