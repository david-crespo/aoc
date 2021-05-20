mutable struct Moon
  x::Int64
  y::Int64
  z::Int64
  vx::Int64
  vy::Int64
  vz::Int64
end

mutable struct Axis
  p::Int64
  v::Int64
end

function compare(a, b)
  b > a ? 1 : b < a ? -1 : 0
end

function tickVelAxis(a1::Axis, a2::Axis)
  a1.v += compare(a1.p, a2.p)
  a2.v += compare(a2.p, a1.p)
end

function tickVel(m1::Moon, m2::Moon)
  m1.vx += compare(m1.x, m2.x)
  m1.vy += compare(m1.y, m2.y)
  m1.vz += compare(m1.z, m2.z)

  m2.vx += compare(m2.x, m1.x)
  m2.vy += compare(m2.y, m1.y)
  m2.vz += compare(m2.z, m1.z)
end

function tickPos(m::Moon)
  m.x += m.vx
  m.y += m.vy
  m.z += m.vz
end

function nomatch(m1, m2)
  m1.x !== m2.x || m1.y !== m2.y || m1.z !== m2.z || m1.vx !== m2.vx || m1.vy !== m2.vy || m1.vz !== m2.vz
end

# x-step = 286332
# y-step = 161428
# z-step = 102356

function runAxis()
  m1 = Moon(13, -13, -2, 0, 0, 0)
  m2 = Moon(16, 2, -15, 0, 0, 0)
  m3 = Moon(7, -18, -12, 0, 0, 0)
  m4 = Moon(-3, -8, -8, 0, 0, 0)

  a1 = Axis(m1.z, m1.vz)
  a2 = Axis(m2.z, m2.vz)
  a3 = Axis(m3.z, m3.vz)
  a4 = Axis(m4.z, m4.vz)

  ia1 = Axis(m1.z, m1.vz)
  ia2 = Axis(m2.z, m2.vz)
  ia3 = Axis(m3.z, m3.vz)
  ia4 = Axis(m4.z, m4.vz)

  count = 0
  # while true && count < 100000000
  while true
    count += 1

    if count % 10000000 == 0
      print(count / 1000000, "\n")
    end

    tickVelAxis(a1, a2)
    tickVelAxis(a1, a3)
    tickVelAxis(a1, a4)
    tickVelAxis(a2, a3)
    tickVelAxis(a2, a4)
    tickVelAxis(a3, a4)
  
    a1.p += a1.v
    a2.p += a2.v
    a3.p += a3.v
    a4.p += a4.v

    if (
      a1.p == ia1.p && a2.p == ia2.p && a3.p == ia3.p && a4.p == ia4.p && 
      a1.v == ia1.v && a2.v == ia2.v && a3.v == ia3.v && a4.v == ia4.v
    )
      print(count, "\n")
      return
    end
  end
end

function run()
  # 2772

  # m1 = Moon(-1, 0, 2, 0, 0, 0)
  # m2 = Moon(2, -10, -7, 0, 0, 0)
  # m3 = Moon(4, -8, 8, 0, 0, 0)
  # m4 = Moon(3, 5, -1, 0, 0, 0)

  # slow one

  # m1 = Moon(-8, -10, 0, 0, 0, 0)
  # m2 = Moon(5, 5, 10, 0, 0, 0)
  # m3 = Moon(2, -7, 3, 0, 0, 0)
  # m4 = Moon(9, -8, -3, 0, 0, 0)

  # real input

  m1 = Moon(13, -13, -2, 0, 0, 0)
  m2 = Moon(16, 2, -15, 0, 0, 0)
  m3 = Moon(7, -18, -12, 0, 0, 0)
  m4 = Moon(-3, -8, -8, 0, 0, 0)

  im1 = deepcopy(m1)
  im2 = deepcopy(m2)
  im3 = deepcopy(m3)
  im4 = deepcopy(m4)

  count = 0
  # while true && count < 100000000
  while true
    count += 1

    if count % 10000000 == 0
      print(count / 1000000, "\n")
    end

    tickVel(m1, m2)
    tickVel(m1, m3)
    tickVel(m1, m4)
    tickVel(m2, m3)
    tickVel(m2, m4)
    tickVel(m3, m4)
  
    tickPos(m1)
    tickPos(m2)
    tickPos(m3)
    tickPos(m4)

    if !nomatch(m1, im1) && !nomatch(m2, im2) && !nomatch(m3, im3) && !nomatch(m4, im4)
      print(count, "\n")
      return
    end
  end
end