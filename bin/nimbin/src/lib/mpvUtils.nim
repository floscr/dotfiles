import os
import strutils
import sequtils
import sugar
import algorithm

const splitChar = ";;;;"
const logFile = expandtilde("~/.cache/mpv_history.log");

type
  LogFile* = ref object
    path*: string
    title*: string

proc getFileNames*(): seq[LogFile] =
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
