Config { font = "xft:Iosevka Mono-10:medium"
        , additionalFonts = [ "xft:Iosevka:size=7:style=bold", "xft:FontAwesome:size=10" ]
        , borderColor = "black"
        , border = FullB 4
        , bgColor = "#04010a"
        , fgColor = "#c5c8c6"
        , position = TopW L 100
        , alpha = 80
        , textOffsets       = [ 22, 22, 24 ]
        , commands = [
                Run MultiCpu [
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
                  [("/", "<fn=1><fc=#767B81>DSK  </fc></fn><free>")]
                  ["-L", "20", "-H", "50", "-m", "1", "-p", "3"]
                  20
                , Run Date "  %a %d.%m <fn=1><fc=#767B81>%b</fc></fn>     %H:%M" "date" 10
                , Run UnsafeStdinReader

                , Run Com "bose_battery_level" ["04:52:C7:C6:1B:68"] "bose_battery_level" 50

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
                        , "--lows"              , ""
                        , "--mediums"           , ""
                        , "--highs"             , ""
                        , "--on-icon-pattern"   , ""
                        , "--idle-icon-pattern" , ""
                        , "-A" , "5"
                        , "-a" , "dunstify -u critical -a Battery \"Battery Low\" \"Your computer will turn of soon\" > /tmp/battery_notification_id"
                ] 50
        ]
        , sepChar = "%"
        , alignSep = "}{"
        , template = " %UnsafeStdinReader% } { %bose_battery_level%     %multicpu%     %memory%     %disku%     %date%     %battery% "
        }
