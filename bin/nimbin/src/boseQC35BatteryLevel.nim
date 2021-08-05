import sugar
import system
import strformat
import strutils
import sequtils
import fp/either
import fp/list
import fp/option
import lib/shUtils
import lib/fpUtils

# Find via bluetoothctl
const device = "04:52:C7:C6:1B:68"

proc intToBatteryIcon(level: int): string =
  case level:
    of 0..10:
      ""
    of 11..20:
      ""
    of 21..30:
      ""
    of 31..50:
      ""
    of 51..70:
      ""
    of 71..high(int):
      ""
    else:
      ""

proc main(): any =
  let res = sh(&"/etc/profiles/per-user/floscr/bin/bt-device -i {device}")
  .flatMap((x: string) => x
           .split("\n")
           .asList
           .findX((x: string) => x.contains("Connected: 1"))
           .asEither(&"Not connected to device {device}")
  )
  .flatMap((x: string) => sh(&"/etc/profiles/per-user/floscr/bin/based-connect {device} -b"))
  .map((x: string) => x
       .parseInt
       .some
       .map(intToBatteryIcon)
       .getOrElse(&"Could not parse input from based connect {x}")
  )

  if res.isLeft:
    # stderr.writeLine("Error: ", res.errorMsg)
    ""
  else:
    res.get

echo main()
