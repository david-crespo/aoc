sample_input = """
199
200
208
210
200
207
240
269
260
263
"""

{:ok, real_input} = File.read("input/day01.txt")

parse = fn s ->
  s
  |> String.trim()
  |> String.split("\n")
  |> Enum.map(&elem(Integer.parse(&1), 0))
end

run = fn input ->
  ns = parse.(input)
end

IO.puts("=======================")
run.(sample_input)
run.(real_input)

# IO.puts("")
# run2(sampleInput)
# run2(realInput)
