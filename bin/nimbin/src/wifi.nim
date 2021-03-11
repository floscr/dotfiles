import sugar
import system
import strutils
import sequtils
import fp/either
import fp/list
import fp/option
import lib/utils
import lib/shUtils
import lib/fpUtils

{.experimental.}

type
  ConfigItem = ref object
    name: string
    uuid: string
    typeId: string
    device: string

proc extractNetworkData(xs: seq[string]): Option[ConfigItem] =
  xs
    .some
    .filter(xs => xs.len == 4)
    .map(x => ConfigItem(
      name: x[0],
      uuid: x[1],
      typeId: x[2],
      device: x[3]
    ))

proc wifiSsid(): any =
  sh("nmcli con show --active")
  .flatMap((x: string) => x
    .split("\n")
    .map((x: string) => x
      .rsplit(" ")
      .filter(x => x != "")
      .some
      .flatMap((x: seq[string]) => extractNetworkData(x))
    )
    .traverse((x: Option[ConfigItem]) => x)
    .asEither("Could not parse \n\n" & x)
  )
  .flatMap((xs: seq[ConfigItem]) => xs
    .asList
    .findX((x: ConfigItem) => x.typeId == "wifi")
    .asEither("No wifi row found")
  )

proc main(cmd = "name"): any =
  if cmd == "name":
    echo wifiSsid()
    .fold(
      x => "Error:\n" & x,
      (x: ConfigItem) => x.name,
    )

when isMainModule:
  import cligen; dispatch(main)
