samples = ["0,3,6", "1,3,2", "2,1,3", "1,2,3"]

function run(inp, N=2020)
  last_spoken = Dict{Int32,Int32}()
  nums = map(s -> parse(Int, s), split(inp, ","))
  for (i, n) in enumerate(nums[1:end-1])
    last_spoken[n] = i
  end

  i = length(nums)
  lst = last(nums)
  while i < N
    new_num = haskey(last_spoken, lst) ? i - last_spoken[lst] : 0
    last_spoken[lst] = i
    lst = new_num
    i+=1
  end

  println(lst)
end

inp = "0,1,5,10,3,12,19"

println("=============== A ===============\n")
for s in samples
  run(s)
end
run(inp)

println("\n=============== B ===============\n")
N = 30000000
run(samples[1], N)
run(inp, N)

