import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/result
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

  let grid = util.to_grid(lines)

  grid
  |> dict.keys
  |> list.map(fn(pt1) {
    list.count(pt.dirs8, fn(dir) {
      let v1 = dict.get(grid, pt1)
      let pt2 = pt.add(pt1, dir)
      let v2 = dict.get(grid, pt2)
      let pt3 = pt.add(pt2, dir)
      let v3 = dict.get(grid, pt3)
      let pt4 = pt.add(pt3, dir)
      let v4 = dict.get(grid, pt4)

      { [v1, v2, v3, v4] |> result.values } == ["X", "M", "A", "S"]
    })
  })
  |> int.sum
  |> io.debug
}

pub fn part2() {
  // let lines = example |> string.trim |> string.split("\n")

  // 2039 too high
  // 1956 too high
  // 1948 (xor) also wrong, didn't say high or low
  // 1952 (or) also wrong, didn't say high or low
  // turns out they only wanted diagonal, not UDLR :(
  let lines = util.get_input_lines(day: 4)
  let grid = util.to_grid(lines)

  grid
  |> dict.keys
  |> list.count(fn(point) {
    case dict.get(grid, point) {
      Ok("A") -> {
        let four_points =
          pt.nb_diag(point)
          |> list.map(dict.get(grid, _))
          |> result.values

        four_points == ["M", "M", "S", "S"]
        || four_points == ["S", "M", "M", "S"]
        || four_points == ["S", "S", "M", "M"]
        || four_points == ["M", "S", "S", "M"]
      }

      _ -> False
    }
  })
  |> io.debug
}
