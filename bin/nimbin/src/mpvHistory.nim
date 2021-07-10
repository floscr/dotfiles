import os
import osproc
import strutils
import sequtils
import strformat
import sugar
import algorithm
import lib/utils
import fp/option
import lib/fpUtils
import lib/mpvUtils

proc main(): any =
  let items = getFileNames()

  let rofi = items
    .map(x => x.title)
    .join("\n")

  let index = execProcess(&"echo \"{rofi}\" | rofi -i -levenshtein-sort -dmenu -p \"Play\" -format i").replace("\n", "")

  discard index
    .some
    .notEmpty
    .map(parseInt)
    .map((x) => items[x])
    .tap(
      proc(x: LogFile) =
        discard execShellCmd(&"mpv --player-operation-mode=pseudo-gui \"{x.path}\"")
    )

main()
