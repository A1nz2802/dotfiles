import Xmobar

box :: String -> String -> String
box color text = "<box type=Bottom width=2 mb=2 color=" ++ color ++ ">" ++ "<fc=" ++ color ++ ">" ++ text ++ "</fc></box>   "

simpleBox :: String -> String -> String
simpleBox color text = "<fc=" ++ color ++ ">" ++ text ++ "</fc>"

config :: Config
config = defaultConfig
    { -- Appearance
      font         = "UbuntuMono NF Bold 13"
    , bgColor      = "#2D2A2E"
    , fgColor      = "#FCFCFA"

    -- Layout
    , position     = TopSize L 100 30
    , lowerOnStart = True
    , hideOnStart  = False
    , allDesktops  = True
    , persistent   = True

    , commands = [
        -- System info
        Run $ Com ".local/bin/kernel" [] "kernel" 36000,
        Run $ Com ".local/bin/keyboard_layout" [] "keyboard" 36000,
        Run $ Com ".local/bin/pacupdate" [] "updates" 3000,

        -- Network info
        Run $ Com ".local/bin/target" [] "target" 36000,
        Run $ Com ".local/bin/vpn_status" [] "vpn" 36000,
        Run $ Com ".local/bin/ethernet_status" [] "ip" 36000,
        Run $ Network "enp12s0" ["-t", "<fn=2>\xf01a</fn>  <rx>kb  <fn=2>\xf01b</fn>  <tx>kb"] 150,    

        -- System Resources
        Run $ Cpu ["-t", "cpu: (<total>%)","-H","50","--high","red"] 20,
        Run $ Memory ["-t", "mem: <used>M (<usedratio>%)"] 20,
        Run $ DiskU [("/", "nvme: <free> free")] [] 60,
        
        -- Time and date
        Run $ Date "%b %d %Y - (%H:%M) " "date" 50,

        -- System Tray Padding
        Run $ Com ".config/xmobar/trayer-padding-icon.sh" [] "trayerpad" 20,

        -- Worskpaces and window title
        Run UnsafeStdinReader
        ]

    -- Template
    , sepChar = "%"
    , alignSep = "}{"
    , template = templateStr
    }

templateStr :: String
templateStr = "<fc=#FF6188> \xf35e </fc> %UnsafeStdinReader% }{ "
        ++ box "#FCFCFA" "\xf0bd0  %target%"      
        ++ box "#bad761" "\xf099d  %vpn%"        
        ++ box "#9cd1bb" "\xf0200  %ip%"         
        ++ simpleBox "#FCFCFA" "\xe777  "
        ++ box "#FF6188" "\xf08c7  %kernel%"      
        ++ box "#A9DC76" "\xf4bc  %cpu%"         
        ++ box "#FC9867" "\xf538  %memory%"      
        ++ box "#FFD866" "\xf0c7  %disku%"       
        ++ box "#AB9DF2" "\xf030c  lang: %keyboard%" 
        ++ box "#78DCE8" "\xf019  %updates%"     
        ++ box "#FF6188" "%enp12s0%"
        ++ box "#A9DC76" "\xf00f0  %date%"        
        ++ "%trayerpad%"

main :: IO ()
main = xmobar config