import sugar
import system
import strutils
import sequtils
# import unpack
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
           # .map((x: ConfigItem) => x.typeId)
           .findX((x: ConfigItem) => x.typeId == "wifi")
           .asEither("No wifi row found")
  )

    # .grep("wifi")
    # .asList
    # .headOption
    # .notEmpty
    # .map(x => x.getColumn(0))
    # .notEmpty

echo wifiSsid()
.fold(
  x => "Error:\n" & x,
  (x: ConfigItem) => x.name,
)


# echo @[1, 2]
#     .asList.findMe((x: int) => false)

# echo @[1.some, 2.some, 3.some].traverse((x: Option[int]) => x)
