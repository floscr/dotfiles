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

const splitChar = ";;;;"
const logFile = expandtilde("~/.cache/mpv_history.log");

type
  LogFile = ref object
    path: string
    title: string

proc getFileNames(): seq[LogFile] =
  return logFile
    .readfile
    .strip
    .splitLines
    .reversed
    .map(x => x.split(splitChar)[2])
    .deduplicate
    .map(path => LogFile(
      path: path,
      title: extractFilename(path),
    ))

proc main(): any =
  let items = getFileNames()

  let rofi = items
    .map(x => x.title)
    .join("\n")

  let index = execProcess(&"echo '{rofi}'| rofi -i -levenshtein-sort -dmenu -p \"Play\" -format i").replace("\n", "")

  discard index
    .some
    .notEmpty
    .map(parseInt)
    .map((x) => items[x])
    .tap(
      proc(x: LogFile) =
        discard execShellCmd(&"mpv \"{x.path}\"")
    )

main()
