import system
import lib/utils
import fp/option
import fp/list
import sugar

proc wifiSsid(): Option[string] =
  shellCommand("nmcli con show --active")
    .grep("wifi")
    .asList
    .headOption
    .notEmpty
    .map(x => x.getColumn(0))
    .notEmpty

echo wifiSsid().getOrElse("")
