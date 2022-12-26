import os
import osproc
import sequtils
import strformat
import strutils
import lib/utils
import lib/fpUtils
import fp/list
import fp/option
import regex
import sugar

{.experimental.}

proc parseId(str: string): string =
  var m: RegexMatch
  discard str.match(re" *(?P<id>[0-9a-z]*).*", m)
  m.group("id", str)[0]

proc scratchWindows(): Option[string] =
  return execProcess("xwininfo -root -children")
    .split("\n")
    .asList
    .drop(6)
    .filter(x => x.contains("-scratch"))
    # Ignore spawn process window
    .filter(x => not(x.contains("10x10")))
    .headOption

proc createTerm(): void =
  discard startProcess("/home/floscr/.nix-profile/bin/alacritty", args = ["--class", "alacritty-scratch"])

proc hide(id: string): void =
  discard execShellCmd(&"xdotool windowunmap {id}")

proc show(id: string): void =
  discard execShellCmd(&"xdotool windowmap {id}")
  discard execShellCmd(&"xdotool windowmove {id} x 37")
  discard execShellCmd(&"bspc node {id} --to-desktop $(bspc query --desktop  -D focused)")

proc toggle(): Option[string] =
  scratchWindows()
  .map(parseId)
  .bitap(
    () => createTerm(),
    proc (x: string) =
      if (execProcess(&"xwininfo -id {x}").contains("IsUnMapped")):
        echo "show"
        show(x)
      else:
        echo "hide"
        hide(x)
  )

discard toggle()
