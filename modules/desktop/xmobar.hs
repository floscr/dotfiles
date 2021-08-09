Config { font = "xft:Iosevka Mono-8:medium,FontAwesome: size=8"
        , borderColor = "black"
        , border = FullB 4
        , bgColor = "#141517"
        , fgColor = "#c5c8c6"
        , position = TopW L 100
        , textOffset       = 20
        , commands = [
                Run Weather "SBPA" ["-t","<tempC>°C","-L","18","-H","25","--normal","green","--high","red","--low","lightblue"] 18000
                , Run Cpu ["-L","3","-H","50"] 10
                , Run Memory ["-t","<used>"] 10
                , Run Date " %a %d.%m  %H:%M" "date" 10
                , Run StdinReader
                , Run Weather "ZHHH" ["-t"," <tempC>°C","-L","48","-H","16"] 36000

                -- , Run Com "/home/floscr/.config/dotfiles/bin/weather vienna" [] "weather" 1000

                , Run DiskU [("/", "<free>")]
                        ["-L", "20", "-H", "50", "-m", "1", "-p", "3"] 20
                -- battery monitor
                , Run Battery [
                        "--template" , "<acstatus> (<timeleft>)"
                        , "--Low"      , "25"
                        , "--High"     , "50"
                        , "--low"      , "#cc6666"
                        , "--high"     , "#b5bd68"
                        , "--"
                        --battery specific options
                        -- discharging
                        , "-o" , "<leftipat> <left>%"
                        -- AC
                        , "-O" , "<leftipat> <left>%"
                        , "-i" , "<leftipat> <left>%"
                        , "--off-icon-pattern"  , ""
                        , "--lows"              , "<fn=1>\xf244</fn>" --
                        , "--mediums"           , "<fn=1>\xf243</fn>" --
                        , "--highs"             , "<fn=1>\xf242</fn>" --
                        , "--on-icon-pattern"   , "<fn=1>\xf0e7</fn>" --
                        , "--idle-icon-pattern" , "<fn=1>\xf0e9</fn>" --
                        , "-A" , "5"
                        , "-a" , "notify-send -u critical --hint=string:x-dunst-stack-tag:low_battery -a Battery -i /run/current-system/sw/share/icons/Papirus-Dark/48x48/status/battery-empty.svg \"Battery Low\" \"Your computer will turn of soon\""
                ] 50
        ]
        , sepChar = "%"
        , alignSep = "}{"
        , template = "%StdinReader% } { %cpu% | %memory% %disku% %date% %battery% %ZHHH% <action=`pavucontrol` button=1>%volume%</action>"
        }
