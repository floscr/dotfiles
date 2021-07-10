import os
import osproc
import strutils
import sequtils
import strformat
import sugar
import algorithm
import lib/utils
import fp/option
import fp/list
import lib/fpUtils
import lib/mpvUtils

proc main(): any =
    let windowName = execProcess("xdotool getwindowfocus getwindowname").replace("\n", "")

    discard getFileNames()
        .asList
        .headOption
        .tap(
        proc(x: LogFile) =
            discard execShellCmd(&"mpv --player-operation-mode=pseudo-gui \"{x.path}\"")
        )

main()
