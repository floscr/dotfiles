import argparse
import lib/utils
import lib/timer
import strformat
from fp/either import fold
import sugar

var p = newParser("My Program"):
  command("list"):
    flag("-a", "--all")
    run:
      runTimerList(opts.all) |> echo
  command("in"):
    arg("time", help="Clock", default="", nargs = -1)
    run:
      runTimerIn(opts.time)
        .fold(
          x => &"Error: {x}\n{p.help}",
          x => $x,
        ) |>
      echo
  command("next"):
    run:
      runNext() |> echo
p.run()
