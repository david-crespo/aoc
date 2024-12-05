import gleam/int
import gleam/io
import gleam/list
import gleam/pair
import gleam/result
import gleam/string
import util.{parse_int}

const example = "
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
"

pub fn part1() {
  let input = util.get_input(day: 05)
  // let input = example |> string.trim
  let assert [rules, updates] = string.split(input, "\n\n")

  let rules =
    rules
    |> string.split("\n")
    |> list.map(fn(s) {
      let assert [x, y] = string.split(s, "|") |> list.map(parse_int)
      #(x, y)
    })

  updates
  |> string.split("\n")
  |> list.map(fn(u) { string.split(u, ",") |> list.map(parse_int) })
  |> list.filter(fn(update) {
    let pairs = list.combination_pairs(update)
    // if it doesn't contain the flipped rule that means there is no
    // rule prohibiting the original
    !list.any(pairs, fn(pair) { list.contains(rules, pair.swap(pair)) })
  })
  |> list.map(fn(update) {
    let idx = update |> list.length |> int.divide(2) |> result.unwrap(0)
    util.idx(update, idx)
  })
  |> result.values
  |> int.sum
  |> io.debug
}

pub fn part2() {
  todo
  // let lines = util.get_input_lines(day: 05)
}
