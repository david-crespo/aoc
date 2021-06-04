Note that rust-analyzer in VS Code doesn't work unless the `rust` dir is opened directly (as opposed to as a subdir within the full repo.)

To start a day, run

```sh
bin/start-day.sh 03
```

To automatically run a day on file changes, use [entr](http://eradman.com/entrproject/) (`brew install entr`):

```sh
ls src/day02.rs | entr cargo run
```
