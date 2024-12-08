import gleam/io
import gleam/string
import util

pub fn part1() {
  let _lines = example()
  let lines = util.get_input_lines(day: 1)
  lines |> io.debug
}

pub fn part2() {
  let _lines = example()
  let lines = util.get_input_lines(day: 1)
  lines |> io.debug
}

pub fn example() {
  "
"
  |> string.trim
  |> string.split("\n")
}
