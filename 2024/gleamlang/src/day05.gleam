import gleam/int
import gleam/io
import gleam/list
import gleam/pair
import gleam/result
import gleam/set
import gleam/string
import util.{parse_int}

// const example = "
// 47|53
// 97|13
// 97|61
// 97|47
// 75|29
// 61|13
// 75|53
// 29|13
// 97|29
// 53|29
// 61|53
// 97|53
// 61|29
// 47|13
// 75|47
// 97|75
// 47|61
// 75|61
// 47|29
// 75|13
// 53|13

// 75,47,61,53,29
// 97,61,53,29,13
// 75,29,13
// 75,97,47,61,53
// 61,13,29
// 97,13,75,29,47
// "

fn parse(input: String) {
  let assert [rules, updates] = string.split(input, "\n\n")
  let rules =
    rules
    |> string.split("\n")
    |> list.map(fn(s) {
      let assert [x, y] = string.split(s, "|") |> list.map(parse_int)
      #(x, y)
    })

  let updates =
    updates
    |> string.split("\n")
    |> list.map(fn(u) { string.split(u, ",") |> list.map(parse_int) })

  #(rules, updates)
}

fn middle(lst: List(x)) {
  let idx = lst |> list.length |> int.divide(2) |> result.unwrap(0)
  util.idx(lst, idx)
}

pub fn part1() {
  let input = util.get_input(day: 05)
  // let input = example |> string.trim

  let #(rules, updates) = parse(input)

  updates
  |> list.filter(fn(update) {
    let pairs = list.combination_pairs(update)
    // if it doesn't contain the flipped rule that means there is no
    // rule prohibiting the original
    !list.any(pairs, fn(pair) { list.contains(rules, pair.swap(pair)) })
  })
  |> list.map(middle)
  |> result.values
  |> int.sum
  |> io.debug
}

fn swap(update: List(Int), rule: #(Int, Int)) {
  case update {
    [a, b, ..rest] if a == rule.1 && b == rule.0 -> [rule.0, rule.1, ..rest]
    [a, ..rest] -> [a, ..swap(rest, rule)]
    _ -> update
  }
}

pub fn part2() {
  // wrong: 5359, 5233
  let input = util.get_input(day: 05)
  // let input = example |> string.trim

  let #(rules, updates) = parse(input)

  // flipping the rules around gets you a set of pairs that aren't allowed
  let bad_pairs = rules |> list.map(pair.swap) |> set.from_list

  updates
  |> list.filter(fn(update) {
    list.any(list.combination_pairs(update), set.contains(bad_pairs, _))
  })
  |> list.map(fn(update) {
    // silly, but you just do it until it's stable
    list.fold(list.range(0, 15), update, fn(acc, _) {
      list.fold(rules, acc, swap)
    })
  })
  |> list.map(middle)
  |> result.values
  |> int.sum
  |> io.debug
}
