import Xmobar

box :: String -> String -> String
box color text = "<box type=Bottom width=2 mb=2 color=" ++ color ++ ">" ++ "<fc=" ++ color ++ ">" ++ text ++ "</fc></box>   "

simpleBox :: String -> String -> String
simpleBox color text = "<fc=" ++ color ++ ">" ++ text ++ "</fc>"

config :: Config
config = defaultConfig
    { -- Appearance
      font         = "UbuntuMono NF Bold 13"
    , bgColor      = "#fdf6e3"
    , fgColor      = "#657b83"

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
templateStr = "<fc=#dc322f> \xf35e </fc> %UnsafeStdinReader% }{ "
        ++ box "#002b36" "\xf0bd0  %target%"      
        ++ box "#cb4b16" "\xf099d  %vpn%"        
        ++ box "#6c71c4" "\xf0200  %ip%"         
        ++ simpleBox "#002b36" "\xe777  "
        ++ box "#dc322f" "\xf08c7  %kernel%"      
        ++ box "#859900" "\xf4bc  %cpu%"         
        ++ box "#268bd2" "\xf538  %memory%"      
        ++ box "#b59800" "\xf0c7  %disku%"       
        ++ box "#d33682" "\xf030c  lang: %keyboard%" 
        ++ box "#2aa198" "\xf019  %updates%"     
        ++ box "#dc322f" "%enp12s0%"
        ++ box "#286bd2" "\xf00f0  %date%"        
        ++ "%trayerpad%"

main :: IO ()
main = xmobar config