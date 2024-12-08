import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/set
import gleam/string
import pt
import util

pub fn part1() {
  // let lines = example()
  let lines = util.get_input_lines(day: 8)
  // 
  let max_y = list.length(lines) |> int.subtract(1)
  let max_x =
    list.first(lines) |> result.unwrap("") |> string.length |> int.subtract(1)

  lines
  |> util.to_grid
  |> dict.filter(fn(_, v) { v != "." })
  |> dict.to_list
  |> list.group(fn(p) { p.1 })
  |> dict.values
  |> list.flat_map(fn(pts) {
    list.map(pts, fn(p) { p.0 })
    |> list.combination_pairs
    |> list.flat_map(fn(pair) {
      let #(a, b) = pair
      let d = #(b.0 - a.0, b.1 - a.1)

      [pt.add(b, d), pt.sub(a, d)]
      |> list.filter(fn(p) {
        p.0 >= 0 && p.0 <= max_x && p.1 >= 0 && p.1 <= max_y
      })
    })
  })
  |> set.from_list
  |> set.size
  |> io.debug
}

pub fn part2() {
  // let lines = example()
  let lines = util.get_input_lines(day: 8)
  // 
  let max_y = list.length(lines) |> int.subtract(1)
  let max_x =
    list.first(lines) |> result.unwrap("") |> string.length |> int.subtract(1)

  lines
  |> util.to_grid
  |> dict.filter(fn(_, v) { v != "." })
  |> dict.to_list
  |> list.group(fn(p) { p.1 })
  |> dict.values
  |> list.flat_map(fn(pts) {
    list.map(pts, fn(p) { p.0 })
    |> list.combination_pairs
    |> list.flat_map(fn(pair) {
      let #(a, b) = pair
      let d = #(b.0 - a.0, b.1 - a.1)

      list.range(-60, 60)
      |> list.map(fn(i) { pt.add(a, pt.scale(d, i)) })
      |> list.filter(fn(p) {
        p.0 >= 0 && p.0 <= max_x && p.1 >= 0 && p.1 <= max_y
      })
    })
  })
  |> set.from_list
  |> set.size
  |> io.debug
}

pub fn example() {
  "
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
"
  |> string.trim
  |> string.split("\n")
}
