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

fn make_grid(lines: List(String)) {
  let grid = lines |> util.to_grid

  let max_y = int.subtract(list.length(lines), 1)
  let max_x =
    int.subtract(string.length(list.first(lines) |> result.unwrap("")), 1)

  Grid(grid, max_x, max_y)
}

pub fn move(
  guard: Guard,
  trail: Set(Guard),
  grid: Grid(String),
) -> #(Guard, Set(Guard)) {
  case guard.pos {
    p if p.0 >= 0 && p.0 <= grid.max_x && p.1 >= 0 && p.1 <= grid.max_y -> {
      // if there's an obstacle in from of us, turn, otherwise step
      let next_guard = step(guard)
      case dict.get(grid.pts, next_guard.pos) {
        Ok(v) -> {
          case v {
            "#" -> move(turn(guard), trail, grid)
            _ -> move(next_guard, set.insert(trail, next_guard), grid)
          }
        }
        _ -> #(guard, trail)
      }
    }
    // if we're out of bounds, we're done!
    _ -> #(guard, trail)
  }
}

fn get_trail(grid: Grid(String)) {
  let guard_start =
    dict.filter(grid.pts, fn(_k, v) { v == "^" })
    |> dict.keys
    |> list.first
    |> result.unwrap(#(-1, -1))

  let guard_start = Guard(pos: guard_start, dir: pt.up)

  let trail = set.from_list([guard_start])
  move(guard_start, trail, grid).1
}

pub fn part1() {
  let lines = util.get_input_lines(day: 6)
  // let lines = example |> string.trim |> string.split("\n")

  lines
  |> make_grid
  |> get_trail
  // for trail purposes we only care about the positions. crossing the same
  // point with different directions is irrelevant
  |> set.map(fn(g) { g.pos })
  |> set.size
  |> io.debug
}

pub fn part2() {
  let lines = util.get_input_lines(day: 6)
  // let lines = example |> string.trim |> string.split("\n")

  let trail =
    lines
    |> make_grid
    |> get_trail
    |> set.map(fn(g) { g.pos })
    |> set.size

  // For each point in the trail, we're going to try setting that point to
  // an obstacle and then run move until we get a loop, which is defined as a
  // repeat entry in the trail with the same direction. This means we need to
  // start recording the direction in the trail too.

  trail
  |> io.debug
}
