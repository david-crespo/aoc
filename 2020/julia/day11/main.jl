sample = """
L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"""

const Board = Array{Int8, 2}

function parse_cell(c)::Int8
  if c == '.'
    0
  elseif c == 'L'
    1
  else
    2
  end
end

function parse_row(s)::Array{Int8}
  map(parse_cell, collect(s))
end

function parse(input)::Board
  board = map(parse_row, split(input, "\n", keepempty=false))
  hcat(board...)
end

function adj_oc(board::Board, pt::Tuple{Int, Int})::Int
  x, y = pt
  mx, my = size(board)
  xrange = intersect(1:mx, x-1:x+1)
  yrange = intersect(1:my, y-1:y+1)
  n = sum([1 for col in view(board, xrange, yrange) for c in col if c == 2])
  if board[x,y] == 2
    n -= 1
  end
  n
end

function step_a(board::Board)::Board
  new_board = copy(board)
  for (y, col) in enumerate(eachcol(board))
    for (x, c) in enumerate(col)
      if c != 0
        pt = (x, y)
        n = adj_oc(board, pt)
        if c == 1 && n == 0
          new_board[pt...] = 2
        elseif c == 2 && n >= 4
          new_board[pt...] = 1
        end
      end
    end
  end
  new_board
end

function run_until_stable(board, step)
  b1 = nothing
  b2 = board
  i = 1
  while b1 != b2
    b1 = b2
    b2 = step(b2)
    i += 1
  end

  println(i - 1, " steps")
  println("answer: ", sum(c == 2 for c in vcat(b2...)))
  println()
end

function show_board(board::Board)
  cstr = Dict{Int8, String}(0=>".", 1=>"L", 2=>"#")
  for col in eachcol(board)
    println(join(cstr[c] for c in col))
  end
  println()
end


sample = parse(sample)
run_until_stable(sample, step_a)

input = open(f->read(f, String), "input.txt")
input = parse(input)
run_until_stable(input, step_a)

