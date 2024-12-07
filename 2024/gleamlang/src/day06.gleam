import gleam/dict
import gleam/io
import gleam/list
import gleam/result
import gleam/set.{type Set}
import pt
import util

// import gleam/string

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
  grid: Grid(String),
  guard: Guard,
  trail: Set(Guard),
) -> #(Guard, Set(Guard), Bool) {
  let new_trail = set.insert(trail, guard)
  let next_guard = step(guard)
  let is_loop = set.contains(trail, next_guard)
  let next_value = dict.get(grid, next_guard.pos)
  case is_loop, next_value {
    // if we've looped, we are done
    True, _ -> #(guard, new_trail, True)
    // if there's an obstacle in front of us, turn, otherwise step
    _, Ok("#") -> move(grid, turn(guard), new_trail)
    _, Ok(_) -> move(grid, next_guard, new_trail)
    // if next_value is not Ok(), we've fallen off the grid
    _, _ -> #(guard, new_trail, False)
  }
}

fn get_start(grid: Grid(String)) {
  dict.filter(grid, fn(_k, v) { v == "^" })
  |> dict.keys
  |> list.first
  |> result.unwrap(#(-1, -1))
}

pub fn part1() {
  let lines = util.get_input_lines(day: 6)
  // let lines = example |> string.trim |> string.split("\n")

  let grid = lines |> util.to_grid
  let start = Guard(pos: get_start(grid), dir: pt.up)

  grid
  |> move(start, set.new())
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
  let start = Guard(pos: get_start(grid), dir: pt.up)
  let trail =
    grid
    |> move(start, set.new())
    |> fn(x) { x.1 }
    |> set.map(fn(g) { g.pos })

  // For each point in the trail, make that point an an obstacle and then run
  // until we get a loop, which is defined as a repeat entry in the trail (trail
  // includes direction).

  trail
  |> set.to_list
  |> list.count(fn(obs) {
    let new_grid = dict.insert(grid, obs, "#")
    move(new_grid, start, set.new()).2
  })
  |> io.debug
}
