Config { font = "xft:Iosevka Mono-6:medium,FontAwesome: size=6"
        , borderColor = "black"
        , border = TopB
        , bgColor = "black"
        , fgColor = "grey"
        , position = TopW L 100
        , commands = [ Run Weather "SBPA" ["-t","<tempC>°C","-L","18","-H","25","--normal","green","--high","red","--low","lightblue"] 18000
                        , Run Cpu ["-L","3","-H","50","--normal","lightgreen","--high","red"] 10
                        , Run Memory ["-t","Mem: <used>MB"] 10
                        , Run Date " %a %d.%m  %H:%M" "date" 10
                        , Run StdinReader
                        , Run DiskU [("/", "<used>/<size>")]
                          ["-L", "20", "-H", "50", "-m", "1", "-p", "3"] 20
                          ]
                        ]
        , sepChar = "%"
        , alignSep = "}{"
        , template = "%StdinReader% } { %cpu% | %memory%   : %disku%   %date%   "
        }
