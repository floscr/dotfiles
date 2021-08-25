Config { font = "xft:Iosevka Mono-8:medium"
        , additionalFonts = [ "xft:Iosevka Mono:size=6:style=bold", "xft:FontAwesome:size=8" ]
        , borderColor = "black"
        , border = FullB 4
        , bgColor = "#141517"
        , fgColor = "#c5c8c6"
        , position = TopW L 100
        , textOffsets       = [ 22, 22, 22 ]
        , commands = [
                Run Weather "SBPA" ["-t","<tempC>°C","-L","18","-H","25","--normal","green","--high","red","--low","lightblue"] 18000

                , Run MultiCpu [
                    "-t", "<fn=1><fc=#767B81>CPU  </fc></fn><total><fc=#767B81>%</fc>"
                    , "--ppad", "2"
                    , "--align", "l"
                    , "--padchars", " "
                    ] 10

                , Run Memory [
                    "-t", "<fn=1><fc=#767B81>MEM  </fc></fn><usedratio><fc=#767B81>%</fc>"
                    , "--ppad", "2"
                    , "--align", "l"
                    , "--padchars", " "
                    ] 10

                , Run DiskU
                  [("/", "<free>")]
                  ["-L", "20", "-H", "50", "-m", "1", "-p", "3"]
                  20

                , Run Date "<fn=2></fn> %a %d.%m <fn=2></fn> %H:%M" "date" 10
                , Run UnsafeStdinReader
                , Run Weather "ZHHH" ["-t"," <tempC>°C","-L","48","-H","16"] 36000
                , Run Com "network-monitor-status" [] "networkMonitorStatus" 60
                , Run Battery [
                        "--template" , "<acstatus>"
                        , "--Low"      , "25"
                        , "--High"     , "50"
                        , "--"
                        --battery specific options
                        -- discharging
                        , "-o" , "<leftipat> <left>%"
                        -- AC
                        , "-O" , "<leftipat> <left>%"
                        , "-i" , "<leftipat> <left>%"
                        , "--off-icon-pattern"  , ""
                        , "--lows"              , "<fn=2></fn>"
                        , "--mediums"           , "<fn=2></fn>"
                        , "--highs"             , "<fn=2></fn>"
                        , "--on-icon-pattern"   , "<fn=2></fn>"
                        , "--idle-icon-pattern" , "<fn=2></fn>"
                        , "-A" , "5"
                        , "-a" , "notify-send -u critical --hint=string:x-dunst-stack-tag:low_battery -a Battery -i /run/current-system/sw/share/icons/Papirus-Dark/48x48/status/battery-empty.svg \"Battery Low\" \"Your computer will turn of soon\""
                ] 50
        ]
        , sepChar = "%"
        , alignSep = "}{"
        , template = " %UnsafeStdinReader% } { %networkMonitorStatus%     %multicpu%     %memory%     %disku%     %date%     %battery% "
        }
