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

// deliberate that all of these are in clockwise order

pub const dirs4 = [up, right, down, left]

pub const dirs_diag = [up_right, down_right, down_left, up_left]

pub const dirs8 = [
  up, up_right, right, down_right, down, down_left, left, up_left,
]

pub fn add(a: Pt, b: Pt) -> Pt {
  #(a.0 + b.0, a.1 + b.1)
}
