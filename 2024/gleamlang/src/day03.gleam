import gleam/int
import gleam/io
import gleam/list
import gleam/option.{Some}
import gleam/regexp
import gleam/string
import util

fn mul_match(m: regexp.Match) {
  let assert [Some(a), Some(b)] = m.submatches
  util.parse_int(a) * util.parse_int(b)
}

pub fn part1() {
  let assert Ok(re) = regexp.from_string("mul\\((\\d+),(\\d+)\\)")
  util.get_input(day: 3)
  |> regexp.scan(with: re, content: _)
  |> list.map(mul_match)
  |> int.sum
  |> io.debug
}

pub fn part2() {
  let assert Ok(re) =
    regexp.from_string("mul\\((\\d+),(\\d+)\\)|do\\(\\)|don't\\(\\)")

  util.get_input(day: 3)
  |> regexp.scan(with: re, content: _)
  |> list.fold(#(0, True), fn(acc, m) {
    case string.slice(m.content, at_index: 0, length: 3) {
      "mul" -> {
        case acc.1 {
          True -> #(acc.0 + mul_match(m), acc.1)
          False -> acc
        }
      }
      "do(" -> #(acc.0, True)
      "don" -> #(acc.0, False)
      _ -> panic
    }
  })
  |> io.debug
}
