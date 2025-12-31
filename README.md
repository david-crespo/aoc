# [Advent of Code](https://adventofcode.com/) solutions

Some of these (all of 2020 and a few in 2019) were done live during the event, the rest were done later.

| Year | Language   | Pt 1 rank (avg / best) | Pt 2 rank (avg / best) |
| ---- | ---------- | ---------------------- | ---------------------- |
| 2019 | TypeScript | 9415 / 749             | 7607 / 569             |
| 2020 | Python     | 1471 / 118             | 1501 / 115             |

In both years, I used [Julia](https://julialang.org/) a few times for brute forcing (which rarely works because the problems are designed to resist it), or sometimes if I was curious how the performance would compare. Julia is a wonderful language for AoC: you can write code without explicit type annotations, which makes it feel like Python, and then add types as needed to help the compiler speed things up.

### LLM policy

Using LLMs to write code or even to think about algorithms seems to me to defeat
the purpose of Advent of Code. However, I have no problem using LLMs with web
search for questions like "num to string in racket". I generally know what I am
trying to do and just want to know how it's done in a given language. It's easy
not to include any details from the problem. If I'm working in a language I know
well, I would also be fine with using an LLM to do a well-specified but tedious
inline code transformation as long as I'm not competing for speed in a private
leaderboard. Finally, after I've completed a day and gotten the code as good
as I'm going to get it on my own, I may ask an LLM to tell me how it could have
been done more idiomatically. I will not commit the suggestions, but I might
link to a gist of the response.

<details>
<summary>Actual LLM-powered searches I ran while working with Racket</summary>

`ai` here is my [llm-cli](https://github.com/david-crespo/llm-cli). `aisf` uses
the latest Gemini Flash model with search on.

```sh
aisf how to split a string in racket
aisf -r what about splitting on an index
aisf in racket how to compare two strings eq
ai -m glm in racket how to check if two strings equal
aif how to multiply numbers in racket
aif scan and fold in racket
ai -r 'what do the square brackets mean in the for/fold'
aif modulo operation in racket
aif how to init empty list in racket
ai -m glm how to count instances of a value in a list in rac
ai -r 'can you do partial application with eq?'
ai -r can you create a list of N copies of a value
ai -r flatmap
ai -r how to read a file text
ai -r get item at list index
ai -r flip args of a function
aisf how to flip args of a function in racket
aisf 'in racket, how to test if regex matches str'
ai -r elaborate more on match groups in px
aisf in rack how to spread a list as args to a function that
aisf num to string in racket
aisf filter in racket
aisf in racket how to print a variable
aisf compose two funcs in racket
ai -r what is '#\9'
ai -r how to convert that to a number
aisf in racket how to convert char of digit to number
aisf in racket how to define intermediate variables inside a
aisf in racket, how to get max num in list and how to get in
ai -r how to sort list
ai -r how to get value by index
ai -r logical or in racket
ai -r how to get sublist from index n on
```

</details>
