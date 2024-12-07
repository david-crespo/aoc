import gleam/int
import gleam/io
import gleam/list
import gleam/string
import util

const example = "
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
"

pub type Op {
  Add
  Mult
}

fn apply(a: Int, b: Int, op: Op) {
  case op {
    Add -> a + b
    Mult -> a * b
  }
}

pub fn eval(nums: List(Int), ops: List(Op)) -> Int {
  case nums, ops {
    [a], [] -> a
    [a, b, ..rest_nums], [op, ..rest_ops] -> {
      eval([apply(a, b, op), ..rest_nums], rest_ops)
    }
    _, _ -> panic
  }
}

pub fn eval2(p: #(Int, List(Int))) {
  let #(target, nums) = p
  case nums {
    [a] -> a == target
    [a, b, ..rest_nums] -> {
      eval2(#(target, [a + b, ..rest_nums]))
      || eval2(#(target, [a * b, ..rest_nums]))
    }
    _ -> panic
  }
}

pub fn part1() {
  // let lines = example |> string.trim |> string.split("\n")
  let lines = util.get_input_lines(day: 7)

  lines
  |> list.map(fn(line) {
    let assert [v, nums] = string.split(line, ": ")
    let v = util.parse_int(v)
    let nums = string.split(nums, " ") |> list.map(util.parse_int)
    #(v, nums)
  })
  |> list.filter(eval2)
  |> list.map(fn(p) { p.0 })
  |> int.sum
  |> io.debug
}
