sample = """
Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10"""


sample2 = """
Player 1:
43
19

Player 2:
2
29
14"""

function parse_deck(s)
  lines = split(s, "\n", keepempty=false)[2:end]
  reverse([parse(Int8, c) for c in lines])
end

function parse_decks(inp)
  a, b = split(inp, "\n\n")
  parse_deck(a), parse_deck(b)
end

function run_a(inp)
  a, b = parse_decks(inp)

  while length(a) > 0 && length(b) > 0
    ac, bc = pop!(a), pop!(b)
    if ac > bc
      a = vcat([bc, ac], a)
    else
      b = vcat([ac, bc], b)
    end
  end

  w = isempty(b) ? a : b
  score = sum(i * c for (i, c) in enumerate(w))
  @show score
end

const DeckPair = Tuple{Array{Int8}, Array{Int8}}
memo = Dict{DeckPair, DeckPair}()

function play(a, b)
  k0 = (a, b)
  if haskey(memo, k0)
    return memo[k0]
  end

  prev_rounds = Set{DeckPair}()

  while !isempty(a) && !isempty(b)
    k = (copy(a), copy(b))
    ac, bc = pop!(a), pop!(b)
    # @show ac, bc
    if k in prev_rounds
      a = vcat([bc, ac], a)
      continue
    end

    push!(prev_rounds, k)

    if ac <= length(a) && bc <= length(b)
      a2, b2 = a[end-ac+1:end], b[end-bc+1:end]
      ra, rb = play(a2, b2)
      if !isempty(ra)
        a = vcat([bc, ac], a)
      else
        b = vcat([ac, bc], b)
      end
    else
      if ac > bc
        a = vcat([bc, ac], a)
      else
        b = vcat([ac, bc], b)
      end
    end
  end

  memo[k0] = (a, b)
  a, b
end

function run_b(inp)
  a, b = parse_decks(inp)
  a, b = play(a, b)
  w = isempty(b) ? a : b
  score = sum(i * c for (i, c) in enumerate(w))
  @show score
end

run(`clear`)
println("PART 1")
run_a(sample)
input = open(f->read(f, String), "input.txt")
run_a(input)

println("\nPART 2")
run_b(sample)
run_b(sample2)
# run_b(input)
