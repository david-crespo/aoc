import gleam/int
import gleam/string.{pad_start, split}
import simplifile

pub fn get_input(day day: Int) {
  let num = pad_start(int.to_string(day), to: 2, with: "0")
  // path is relative to cwd. could be done more robustly
  let assert Ok(file) = simplifile.read(from: "../input/day" <> num <> ".txt")
  file |> string.trim |> split(on: "\n")
}
