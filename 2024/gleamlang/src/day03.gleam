import gleam/int
import gleam/io
import gleam/list
import gleam/option.{Some}
import gleam/regexp
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

pub type State {
  State(sum: Int, do: Bool)
}

pub fn part2() {
  let assert Ok(re) =
    regexp.from_string("mul\\((\\d+),(\\d+)\\)|do\\(\\)|don't\\(\\)")

  util.get_input(day: 3)
  |> regexp.scan(with: re, content: _)
  |> list.fold(State(0, True), fn(state, m) {
    case m.content {
      "mul" <> _ if state.do -> {
        State(..state, sum: state.sum + mul_match(m))
      }
      "mul" <> _ if !state.do -> state
      "do()" -> State(..state, do: True)
      "don't()" -> State(..state, do: False)
      _ -> panic
    }
  })
  |> io.debug
}
