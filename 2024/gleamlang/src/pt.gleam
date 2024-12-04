pub type Pt =
  #(Int, Int)

const up = #(0, -1)

const down = #(0, 1)

const left = #(-1, 0)

const right = #(1, 0)

const up_left = #(-1, -1)

const up_right = #(1, -1)

const down_left = #(-1, 1)

const down_right = #(1, 1)

pub const dirs4 = [up, down, left, right]

pub const dirs8 = [
  up, down, left, right, up_left, up_right, down_left, down_right,
]

pub fn add(a: Pt, b: Pt) -> Pt {
  #(a.0 + b.0, a.1 + b.1)
}
