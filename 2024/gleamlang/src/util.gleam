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
