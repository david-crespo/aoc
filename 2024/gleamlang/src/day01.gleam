import gleam/io
import gleam/list.{take}
import gleam/string.{join}
import util

pub fn run() {
  let lines = util.get_input(day: 1)
  io.println("day 1")
  io.println(join(take(lines, 5), "\n"))
}
