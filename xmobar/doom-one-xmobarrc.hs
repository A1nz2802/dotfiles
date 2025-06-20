import Xmobar

box :: String -> String -> String
box color text = "<box type=Bottom width=2 mb=2 color=" ++ color ++ ">" ++ "<fc=" ++ color ++ ">" ++ text ++ "</fc></box>   "

simpleBox :: String -> String -> String
simpleBox color text = "<fc=" ++ color ++ ">" ++ text ++ "</fc>"

config :: Config
config = defaultConfig
    { -- Appearance
      font         = "UbuntuMono NF Bold 13"
    , bgColor      = "#282c34"
    , fgColor      = "#ff6c6b"

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
templateStr = "<fc=#ff6c6b> \xf35e </fc> %UnsafeStdinReader% }{ "
        ++ box "#dfdfdf" "\xf0bd0  %target%"      
        ++ box "#5699af" "\xf099d  %vpn%"        
        ++ box "#da8548" "\xf0200  %ip%"         
        ++ simpleBox "#dfdfdf" "\xe777  "
        ++ box "#51afef" "\xf08c7  %kernel%"      
        ++ box "#ecbe7b" "\xf4bc  %cpu%"         
        ++ box "#ff6c6b" "\xf538  %memory%"      
        ++ box "#a9a1e1" "\xf0c7  %disku%"       
        ++ box "#98be65" "\xf030c  lang: %keyboard%" 
        ++ box "#c678dd" "\xf019  %updates%"     
        ++ box "#da8548" "%enp12s0%"
        ++ box "#46d9ff" "\xf00f0  %date%"        
        ++ "%trayerpad%"

main :: IO ()
main = xmobar config