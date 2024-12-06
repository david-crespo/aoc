import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/set.{type Set}
import gleam/string
import pt
import util

const example = "
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
"

pub type Dir {
  Up
  Down
  Left
  Right
}

pub type Guard {
  Guard(pos: pt.Pt, dir: pt.Pt)
}

pub fn step(g: Guard) {
  Guard(pt.add(g.pos, g.dir), g.dir)
}

pub fn turn(g: Guard) {
  let dir = case g.dir {
    p if p == pt.up -> pt.right
    p if p == pt.right -> pt.down
    p if p == pt.down -> pt.left
    p if p == pt.left -> pt.up
    _ -> panic
  }
  Guard(g.pos, dir)
}

pub type Grid(t) {
  Grid(pts: dict.Dict(#(Int, Int), t), max_x: Int, max_y: Int)
}

pub fn move(
  guard: Guard,
  trail: Set(pt.Pt),
  grid: Grid(String),
) -> #(Guard, Set(pt.Pt)) {
  let Guard(pos, dir) = guard
  case pos {
    p if p.0 >= 0 && p.0 <= grid.max_x && p.1 >= 0 && p.1 <= grid.max_y -> {
      // if there's an obstacle in from of us, turn, otherwise step
      let next_pos = step(guard).pos
      let value_at_next = dict.get(grid.pts, next_pos)
      case value_at_next {
        Ok(v) -> {
          case v {
            "#" -> move(turn(guard), trail, grid)
            _ -> move(step(guard), set.insert(trail, next_pos), grid)
          }
        }
        _ -> #(guard, trail)
      }
    }
    // if we're out of bounds, we're done!
    p -> #(Guard(p, dir), trail)
  }
}

pub fn part1() {
  let lines = util.get_input_lines(day: 6)
  // let lines = example |> string.trim |> string.split("\n")

  let grid = lines |> util.to_grid

  let max_y = int.subtract(list.length(lines), 1)
  let max_x =
    int.subtract(string.length(list.first(lines) |> result.unwrap("")), 1)

  let grid = Grid(grid, max_x, max_y)

  let guard_start =
    dict.filter(grid.pts, fn(_k, v) { v == "^" })
    |> dict.keys
    |> list.first
    |> result.unwrap(#(-1, -1))

  let guard_start = Guard(pos: guard_start, dir: pt.up)

  guard_start
  |> io.debug

  let trail = set.from_list([guard_start.pos])
  move(guard_start, trail, grid).1
  |> set.to_list
  |> list.length
  |> io.debug
}

pub fn part2() {
  let lines = util.get_input_lines(day: 6)

  lines
  |> io.debug
}
