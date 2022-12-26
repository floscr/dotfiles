import times
import os
import osproc
import sugar
import strformat
import fp/either
import tempfile
import lib/shUtils
import lib/fpUtils

const imagesPath = getEnv("$XDG_SCREENSHOTS_DIR", "~/Media/Screencapture").expandTilde()
let (_, tmpPath) = mkstemp()

proc saveScreenshot(): any =
  let t = now()

  discard existsOrCreateDir(imagesPath)

  let dst = imagesPath.joinPath("screencapture-" & t.format("yyyy-MM-dd-HH-mm:ss") & ".png")
  copyFile(tmpPath, dst)

  discard execShellCmd(&"notify-send \"Screenshot saved\" \"Copied to clipboard\n{dst}\"")

  dst

proc main() =
  discard sh("maim --delay=0.1 -us > $\"" & tmpPath & "\"")
      .tap((_) => execShellCmd("xclip -selection clipboard -t image/png \"" & tmpPath & "\""))
      .tap((x: any) => saveScreenshot())

main()
