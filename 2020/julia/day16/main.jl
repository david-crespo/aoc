sample = """
class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12
"""

sample2 = """
class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9
"""

function parse_int(n)
  parse(Int, n)
end

function parse_range(s)
  a, b = map(parse_int, split(s, "-"))
  Set(a:b)
end

function parse_rule(s)
  k, rs = split(s, ": ")
  rs = map(parse_range, split(rs, " or "))
  rs = union(rs...)
  k, rs
end

function parse_ticket(t)
  map(parse_int, split(t, ","))
end

function parse_input(input)
  rules, mine, nearby = split(input, "\n\n")

  rules = Dict(map(parse_rule, split(rules, "\n"))) 

  mine = parse_ticket(split(mine, "\n")[2])

  nearby = deleteat!(split(nearby, "\n", keepempty=false), 1)
  nearby = map(parse_ticket, nearby) 

  rules, mine, nearby
end

function is_valid(t, rules)
  for n in t
    if !any(n in range for (k, range) in rules)
      return false
    end
  end
  true
end

function run_a(input)
  rules, mine, nearby = parse_input(input)
  error = 0
  for t in nearby
    for n in t
      if !any(n in range for (k, range) in rules)
        error += n
      end
    end
  end
  println(error)
end 

function run_b(input)
    rules, mine, nearby = parse_input(input)
    valid = filter(t -> is_valid(t, rules), nearby)

    poss = Dict{String,Set{Int}}()
    for (k, ns) in rules
      for i in 1:length(mine)
        if all(t[i] in ns for t in valid)
          vals = get(poss, k, Set())
          poss[k] = push!(vals, i)
        end
      end
    end
    
    mapping = Dict{String, Int}()
    while length(mapping) < length(mine)
      k, idxs = first(filter(p -> length(p[2]) == 1, poss))
      i = only(idxs)
      mapping[k] = i 
      for (k, idxs) in poss
        pop!(idxs, i, 0)
      end
    end

    answer = 1
    for (k, i) in mapping
      if startswith(k, "departure")
        answer *= mine[i]
      end
    end
    println(answer)
end

println("PART 1")
run_a(sample)
input = open(f->read(f, String), "input.txt")
run_a(input)

println("\nPART 2")
run_b(sample2)
run_b(input)
