import os
import osproc
import strutils
import sequtils
import strformat
import sugar
import algorithm
import lib/utils

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

  let rofi = items.map(x => x.title)
    .join("\n")

  let response = execProcess(&"echo '{rofi}'| rofi -i -levenshtein-sort -dmenu -p \"Play\"").replace("\n", "")

  if response != "":
    let item = items.findIt(it.title == response)
    echo item.path
    discard execShellCmd(&"mpv \"{item.path}\"")

main()
