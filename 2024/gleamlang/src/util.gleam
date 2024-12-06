import gleam/dict
import gleam/int
import gleam/list
import gleam/string.{pad_start, split}
import simplifile

pub fn get_input(day day: Int) {
  let num = pad_start(int.to_string(day), to: 2, with: "0")
  // path is relative to cwd. could be done more robustly
  let assert Ok(file) = simplifile.read(from: "../input/day" <> num <> ".txt")
  string.trim(file)
}

pub fn get_input_lines(day day: Int) {
  get_input(day) |> split(on: "\n")
}

pub fn parse_int(s: String) {
  let assert Ok(n) = int.parse(s)
  n
}

pub fn idx(lst: List(x), at: Int) {
  list.drop(lst, at) |> list.first
}

pub type Grid(t) =
  dict.Dict(#(Int, Int), t)

pub fn to_grid(lines: List(String)) -> Grid(String) {
  lines
  |> list.index_map(fn(line, j) {
    line
    |> string.to_graphemes
    |> list.index_map(fn(c, i) { #(#(i, j), c) })
  })
  |> list.flatten
  |> dict.from_list
}
