import gleam/dict
import gleam/io
import gleam/list
import gleam/result
import gleam/set.{type Set}
import pt
import util

// const example = "
// ....#.....
// .........#
// ..........
// ..#.......
// .......#..
// ..........
// .#..^.....
// ........#.
// #.........
// ......#...
// "

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

pub type Grid(t) =
  dict.Dict(#(Int, Int), t)

pub fn move(
  guard: Guard,
  trail: Set(Guard),
  grid: Grid(String),
) -> #(Guard, Set(Guard), Bool) {
  let next_guard = step(guard)
  let is_loop = set.contains(trail, next_guard)
  let next_value = dict.get(grid, next_guard.pos)
  case is_loop, next_value {
    // if we've looped, we are done
    True, _ -> #(guard, trail, True)
    // if there's an obstacle in front of us, turn, otherwise step
    _, Ok("#") -> move(turn(guard), set.insert(trail, turn(guard)), grid)
    _, Ok(_) -> move(next_guard, set.insert(trail, next_guard), grid)
    // if next_value is not Ok(), we've fallen off the grid
    _, _ -> #(guard, trail, False)
  }
}

fn run(grid: Grid(String)) {
  let guard_start =
    dict.filter(grid, fn(_k, v) { v == "^" })
    |> dict.keys
    |> list.first
    |> result.unwrap(#(-1, -1))

  let guard_start = Guard(pos: guard_start, dir: pt.up)

  let trail = set.from_list([guard_start])
  move(guard_start, trail, grid)
}

pub fn part1() {
  let lines = util.get_input_lines(day: 6)
  // let lines = example |> string.trim |> string.split("\n")

  lines
  |> util.to_grid
  |> run
  |> fn(x) { x.1 }
  // get trail
  // for trail purposes we only care about the positions. crossing the same
  // point with different directions is irrelevant
  |> set.map(fn(g) { g.pos })
  |> set.size
  |> io.debug
}

pub fn part2() {
  let lines = util.get_input_lines(day: 6)
  // let lines = example |> string.trim |> string.split("\n")

  let grid = lines |> util.to_grid

  let trail = grid |> run |> fn(x) { x.1 } |> set.map(fn(g) { g.pos })

  // For each point in the trail, make that point an an obstacle and then run
  // until we get a loop, which is defined as a repeat entry in the trail (trail
  // includes direction).

  trail
  |> set.to_list
  |> list.count(fn(obs) {
    let new_grid = dict.insert(grid, obs, "#")
    run(new_grid).2
  })
  |> io.debug
}
