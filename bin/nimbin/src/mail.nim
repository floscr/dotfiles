import os, osproc, strutils, sequtils, strformat
import lib/utils
import argparse
import sugar

let mailDir = expandTilde("~/.mail")
let accounts = ["Work/Inbox/new"]

let color = "#f0c674"
let prefix = ""

proc polyBarPrettyPrint(count: int): string =
  if count == 0:
    return ""
  else:
    return &"%{{F{color}}}{prefix} {count}%{{F-}}"

proc newCount(): int =
  let files = accounts
    .map((x) => joinPath(mailDir, x))
    .map((x) => walkDir(x, true) |> toSeq |> len)
    .foldl(a + b)
    
  return files

var p = newParser("mail"):
  command("newCount"):
    run:
      newCount() |>
      polyBarPrettyPrint |>
      echo

p.run(@["newCount"])
