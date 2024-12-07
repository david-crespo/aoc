import gleam/int
import gleam/io
import gleam/list
import gleam/string
import util

// const example = "
// 190: 10 19
// 3267: 81 40 27
// 83: 17 5
// 156: 15 6
// 7290: 6 8 6 15
// 161011: 16 10 13
// 192: 17 8 14
// 21037: 9 7 18 13
// 292: 11 6 16 20
// "

pub fn eval(target: Int, nums: List(Int)) {
  case nums {
    [a] -> a == target
    [a, b, ..rest_nums] ->
      eval(target, [a + b, ..rest_nums]) || eval(target, [a * b, ..rest_nums])
    _ -> panic
  }
}

pub fn concat(a: Int, b: Int) {
  util.parse_int(int.to_string(a) <> int.to_string(b))
}

pub fn eval2(target: Int, nums: List(Int)) {
  case nums {
    [a] -> a == target
    [a, b, ..rest_nums] -> {
      eval2(target, [a + b, ..rest_nums])
      || eval2(target, [a * b, ..rest_nums])
      || eval2(target, [concat(a, b), ..rest_nums])
    }
    _ -> panic
  }
}

pub fn parse_line(line: String) {
  let assert [v, nums] = string.split(line, ": ")
  let v = util.parse_int(v)
  let nums = string.split(nums, " ") |> list.map(util.parse_int)
  #(v, nums)
}

pub fn part1() {
  // let lines = example |> string.trim |> string.split("\n")
  let lines = util.get_input_lines(day: 7)

  lines
  |> list.map(parse_line)
  |> list.filter(fn(p) { eval(p.0, p.1) })
  |> list.map(fn(p) { p.0 })
  |> int.sum
  |> io.debug
}

pub fn part2() {
  // let lines = example |> string.trim |> string.split("\n")
  let lines = util.get_input_lines(day: 7)

  lines
  |> list.map(parse_line)
  |> list.filter(fn(p) { eval2(p.0, p.1) })
  |> list.map(fn(p) { p.0 })
  |> int.sum
  |> io.debug
}
