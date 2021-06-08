Note that rust-analyzer in VS Code doesn't work unless the `rust` dir is opened directly (as opposed to as a subdir within the full repo.)

To create the source file and empty input file for a day, run

```sh
bin/init-day 03
```

To automatically run a day on file changes, use [entr](http://eradman.com/entrproject/) (`brew install entr`):

```sh
./bin/watch
```
