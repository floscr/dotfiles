import os, osProc
import sequtils, strutils, strformat
import lib/utils

const headsetUnitName = "headset_head_unit"
const a2dpUnitName = "a2dp_sink_aac"

let sinkname = execProcess("pacmd", args=["list-sinks"], options={poUsePath})
    .split("\n")
    .findIt(it.contains("name: <bluez_sink."))

let targetSinkName = if sinkname.contains(headsetUnitName): a2dpUnitName
                     else: headsetUnitName

discard execShellCmd(&"pacmd set-card-profile bluez_card.04_52_C7_C6_1B_68 {targetSinkName}")
