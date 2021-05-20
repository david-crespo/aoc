sample = """
Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###..."""

const Grid = BitArray{2}
@enum Dir up=0 right=1 down=2 left=3

struct Tile
  id::Int
  grid::Grid
  edges::Dict{Dir, BitArray}
end

function Base.show(io::IO, grid::Grid)
  for row in eachrow(grid)
    println(join(map(c -> c ? "#" : ".", row)))
  end
end

function get_edges(tile::Grid)
  Dict{Dir, BitArray}(
    up    => tile[:, 1],
    right => tile[end, :],
    down  => tile[:, end],
    left  => tile[1, :],
  )
end

function parse_tile(s)
  lines = split(s, "\n", keepempty=false)
  id = parse(Int, lines[1][6:9])
  grid = split.(lines[2:end], "")  # Array{String} -> Array{Array{String}}
  grid = hcat(grid...)  # Array{Array{String}} -> Array{String, 2}
  grid = isequal.("#", grid) # Array{String, 2} -> BitArray{2}
  grid = Grid(grid')
  id, Tile(id, grid, get_edges(grid))
end

function get_matching_edge(tile1, tile2)
  for (d1, e1) in tile1.edges
    for (_, e2) in tile2.edges
      if e1 == e2 || e1 == reverse(e2)
        return d1
      end
    end
  end
end

function get_matching_edges(tiles)
  me = Dict{Int, Dict{Dir, Int}}()
  for (id1, tile1) in tiles
    me[id1] = Dict()
    for (id2, tile2) in tiles
      if id1 != id2
        d1 = get_matching_edge(tile1, tile2)
        if !isnothing(d1)
          me[id1][d1] = tile2.id 
        end
      end 
    end 
  end
  me
end

function run_a(inp)
  tiles = Dict(map(parse_tile, split(inp, "\n\n")))
  me = get_matching_edges(tiles)
  corners = [id for (id, edges) in me if length(edges) == 2]
  @show (*)(corners...)
end

function remove_n(neighbors, n)
  for ns in values(neighbors)
    delete!(ns, n)
  end
end

function run_b(inp)
  tiles = Dict(map(parse_tile, split(inp, "\n\n")))
  S = Int(sqrt(length(tiles)))
  me = get_matching_edges(tiles)

  neighbors = Dict((n, Set(union(values(ds)...))) for (n, ds) in me)

  ngrid = Array{Int, 2}(undef, S, S)

  corners = [id for (id, edges) in me if length(edges) == 2]
  first_corner = corners[1]
  ngrid[1,1] = first_corner
  remove_n(neighbors, first_corner)

  outer = [id for (id, edges) in me if length(edges) in [2,3]]

  for x in 2:S
    prev = ngrid[x-1, 1]
    next = first(intersect(neighbors[prev], outer))
    remove_n(neighbors, next)
    ngrid[x, 1] = next
  end

  for y in 2:S
    for x in 1:S
      above = ngrid[x, y-1]
      next = first(neighbors[above])
      remove_n(neighbors, next)
      ngrid[x,y] = next
    end
  end

  dirs = Dict{Tuple{Int,Int}, Dir}()
  for (n, ds) in me
    for (d0, n0) in ds
      dirs[(n, n0)] = d0
    end
  end

  TS = size(first(values(tiles)).grid)[1] - 2  # border gets trimmed
  fgrid = Grid(zeros(Bool, S*TS, S*TS))
  # @show fgrid

  ngrid = reverse(ngrid, dims=2)'
  
  for (y, row) in enumerate(eachrow(ngrid))
    for (x, n) in enumerate(row)
      if x < S
        x_neighbor = ngrid[x + 1, y]
        x_neighbor_dir_init = right
      else
        x_neighbor = ngrid[x - 1, y]
        x_neighbor_dir_init = left
      end

      x_neighbor_dir_final = dirs[(n, x_neighbor)]
      # mod is always > 0, but % is not for some reason
      xdiff = mod(Int(x_neighbor_dir_init) - Int(x_neighbor_dir_final), 4)

      if y < S
        y_neighbor = ngrid[x, y + 1]
        y_neighbor_dir_init = down
      else
        y_neighbor = ngrid[x, y - 1]
        y_neighbor_dir_init = up
      end

      y_neighbor_dir_final = dirs[(n, y_neighbor)]
      ydiff = mod(Int(y_neighbor_dir_init) - Int(y_neighbor_dir_final), 4)

      tile = tiles[n].grid

      @show x y n xdiff ydiff
      println()
      @show tile
      println()
      @show tile[2:end-1, 2:end-1]

      # for i in 1:ydiff
      #   tile = rotr90(tile)
      # end
      # if xdiff != ydiff
      #   tile = reverse(tile, dims=1)
      # end

      # @show tile

      fx, fy = (x - 1) * TS + 1 , (y - 1) * TS + 1
      fgrid[fx:fx+TS-1, fy:fy+TS-1] = tile[2:end-1, 2:end-1]

      println()
    end
  end

  @show ngrid
  @show fgrid
  # @show rot180(fgrid)
end 

run(`clear`)
println("PART 1")
run_a(sample)
input = open(f->read(f, String), "input.txt")
run_a(input)

println("\nPART 2")
run_b(sample)
# run_b(input)
