sample = """
sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew"""


function to_pt(s)::Pt
  moves = [m.captures[1] for m in eachmatch(r"(e|se|sw|w|nw|ne)", s)]
  x, y = (0, 0)
  for m in moves
    if m == "e"
      x += 1
    elseif m == "se"
      y -= 1
      x += 1
    elseif m == "sw"
      y -= 1
    elseif m == "w"
      x -= 1
    elseif m == "nw"
      y += 1
      x -= 1
    elseif m == "ne"
      y += 1
    end
  end
  (x, y)
end

function counter(iter)
  ct = Dict()
  for x in iter
    if !haskey(ct, x)
      ct[x] = 0
    end
    ct[x] += 1
  end
  ct
end

function run_a(inp)
  tiles = map(to_pt, split(inp, "\n", keepempty=false))
  c = counter(tiles)
  println(length([k for (k, v) in c if v % 2 == 1]))
end

const Pt = Tuple{Int8, Int8}
const Grid = Dict{Pt, Bool}

memo = Dict{Pt, Array{Pt}}()

function neighbors(pt::Pt)
  if haskey(memo, pt)
    return memo[pt]
  end

  x, y = pt
  pts = [
    (x + 1, y),
    (x - 1, y),
    (x, y + 1),
    (x, y - 1),
    (x - 1, y + 1),
    (x + 1, y - 1),
  ]
  memo[pt] = pts
  pts
end

function step(d::Grid)::Grid
  new_d = Grid()
  n = 100
  for x in -n:n
    for y in -n:n
      pt::Pt = (x, y)
      curr = get(d, pt, false)
      ct = count(n -> get(d, n, false), neighbors(pt))
      if curr && (ct == 0 || ct > 2)
        new_d[pt] = false
      elseif !curr && ct == 2
        new_d[pt] = true
      else
        new_d[pt] = curr
      end
    end
  end
  new_d
end

function run_b(inp)
  tiles = map(to_pt, split(inp, "\n", keepempty=false))
  c = counter(tiles)
  d = Grid((k, true) for (k, v) in c if v % 2 == 1)

  for i in 1:100
    d = step(d)
  end
  println(length([k for (k, v) in d if v]))
end

run(`clear`)
println("PART 1")
run_a(sample)
input = open(f->read(f, String), "input.txt")
run_a(input)

println("\nPART 2")
run_b(sample)
run_b(input)
