struct NewStack
end

struct Cut
    n::Int
end

struct DealInc
    inc::Int
end

Cmd = Union{NewStack,Cut,DealInc}
Program = Array{Cmd}

function parseProgram()
    program = Cmd[]
    open("static/day22-input.txt") do file
        for ln in eachline(file)
            if ln === "deal into new stack"
                push!(program, NewStack())
            elseif startswith(ln, "cut")
                n = parse(Int, split(ln, " ")[2])
                push!(program, Cut(n))
            elseif startswith(ln, "deal with inc")
                inc = parse(Int, split(ln, " ")[4])
                push!(program, DealInc(inc))
            end
        end
    end
    program
end

function wrap(i::Int, size::Int)::Int
    i < 0 ? i + size : i >= size ? i - size : i
end

function cutIdx(i::Int, deckSize::Int, n::Int)::Int
    cutAt = n > 0 ? n : deckSize + n
    wrap(i - cutAt, deckSize)
end

function dealNewStackIdx(i::Int, deckSize::Int)::Int
    deckSize - 1 - i
end

function dealWithIncIdx(i::Int, deckSize::Int, inc::Int)::Int
    (i * inc) % deckSize
end

function processIdx(i::Int, deckSize::Int, program::Program)::Int
    for cmd in program
        if cmd isa NewStack
            i = dealNewStackIdx(i, deckSize)
        elseif cmd isa Cut
            i = cutIdx(i, deckSize, cmd.n)
        elseif cmd isa DealInc
            i = dealWithIncIdx(i, deckSize, cmd.inc)
        end
    end
    i
end

function programToCode(program::Program)
    for cmd in program
        if cmd isa NewStack
            println("i = dealNewStackIdx(i, deckSize)")
        elseif cmd isa Cut
            println("i = cutIdx(i, deckSize, $(cmd.n))")
        elseif cmd isa DealInc
            println("i = dealWithIncIdx(i, deckSize, $(cmd.inc))")
        end
    end
end

function run()
    program = parseProgram()
    # programToCode(program)
    # return

    println(processIdx(2019, 10007, program))

    initIdx = 2020
    i = initIdx
    i = processIdx(i, 119315717514047, program)
    count = 0
    # diffs = Set{Int}()
    while i !== initIdx # && count < 10000000
        count += 1
        newi = processIdx(i, 119315717514047, program)
        # diff = newi - i
        # if in(diff, diffs)
        #     println("repeat at")
        # end
        # push!(diffs, diff)
        i = newi
        if (count % 1000000 === 0)
            println(count / 1000000, "M")
        end
    end
    println("took n cycles to repeat:", count)
end