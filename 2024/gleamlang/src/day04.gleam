import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import pt
import util

// const example = "
// MMMSXXMASM
// MSAMXMSMSA
// AMXSXMAAMM
// MSAMASMSMX
// XMASAMXAMM
// XXAMMXXAMA
// SMSMSASXSS
// SAXAMASAAA
// MAMMMXMMMM
// MXMXAXMASX
// "

pub fn part1() {
  let lines = util.get_input_lines(day: 4)
  // let lines = example |> string.trim |> string.split("\n")

  let grid =
    lines
    |> list.index_map(fn(line, j) {
      line
      |> string.to_graphemes
      |> list.index_map(fn(c, i) { #(#(i, j), c) })
    })
    |> list.flatten
    |> dict.from_list

  grid
  |> dict.keys
  |> list.map(fn(pt1) {
    list.count(pt.dirs8, fn(dir) {
      let v1 = dict.get(grid, pt1) |> result.unwrap("")
      let pt2 = pt.add(pt1, dir)
      let v2 = dict.get(grid, pt2) |> result.unwrap("")
      let pt3 = pt.add(pt2, dir)
      let v3 = dict.get(grid, pt3) |> result.unwrap("")
      let pt4 = pt.add(pt3, dir)
      let v4 = dict.get(grid, pt4) |> result.unwrap("")
      v1 == "X" && v2 == "M" && v3 == "A" && v4 == "S"
    })
  })
  |> int.sum
  |> io.debug
}
// pub fn part2() {
//   todo
// }
