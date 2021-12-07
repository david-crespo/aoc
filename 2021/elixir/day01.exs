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
  parse.(input)
  |> Enum.chunk_every(2, 1, :discard)
  |> Enum.count(fn [a, b] -> b > a end)
  |> IO.inspect()
end

run2 = fn input ->
  parse.(input)
  |> Enum.chunk_every(3, 1, :discard)
  |> Enum.map(&Enum.sum/1)
  |> Enum.chunk_every(2, 1, :discard)
  |> Enum.count(fn [a, b] -> b > a end)
  |> IO.inspect()
end

IO.puts("=======================")
run.(sample_input)
run.(real_input)

IO.puts("")
run2.(sample_input)
run2.(real_input)
