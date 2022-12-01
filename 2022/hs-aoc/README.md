# hs-aoc

## Running

### Setup

Had to run these to get stack to ignore its own configured GHC and use the one I
have set globally with ghcup, which is 9.2.4 because the language server is not
compatible with 9.2.5.

```
stack config set install-ghc false --global
stack config set system-ghc  true  --global
```

### Compile and run directly

```
stack build && stack exec hs-aoc-exe
```

### Live runner

```
./run_hs
```

## Project structure notes

I was able to solve the first puzzle without bringing in any external
dependencies, though my solution would have been shorter and quicker to write if
I didn't have to do so much by hand. This doesn't matter yet, but I'm sure I'll
actually need to pull in a dependency eventually. (Though it's kind of an
interesting constraint to never bring in anything. I'm probably not good enough
at Haskell for that.)

So... I initialized a project with Stack and was able to get the day 1 solution
running through it. This seems to be the recommended way, not using cabal
directly. There's so much overhead in terms of project files and stuff, though,
that instead of a separate project for each day, I'd rather have a separate file
in `src/` for each day and just change the import in `Main.hs`.
