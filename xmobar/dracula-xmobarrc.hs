import Xmobar

box :: String -> String -> String
box color text = "<box type=Bottom width=2 mb=2 color=" ++ color ++ ">" ++ "<fc=" ++ color ++ ">" ++ text ++ "</fc></box>   "

simpleBox :: String -> String -> String
simpleBox color text = "<fc=" ++ color ++ ">" ++ text ++ "</fc>"

config :: Config
config = defaultConfig
    { -- Appearance
      font         = "UbuntuMono NF Bold 13"
    , bgColor      = "#292a36"
    , fgColor      = "#f8f8f2"

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
templateStr = "<fc=#BF616A> \xf35e </fc> %UnsafeStdinReader% }{ "
        ++ box "#f8f8f2" "\xf0bd0  %target%"      
        ++ box "#9aedfe" "\xf099d  %vpn%"        
        ++ box "#ff92d0" "\xf0200  %ip%"         
        ++ simpleBox "#f8f8f2" "\xe777  "
        ++ box "#bd93f9" "\xf08c7  %kernel%"      
        ++ box "#f1fa8c" "\xf4bc  %cpu%"         
        ++ box "#ff5555" "\xf538  %memory%"      
        ++ box "#5af78e" "\xf0c7  %disku%"       
        ++ box "#ff79c6" "\xf030c  lang: %keyboard%" 
        ++ box "#8be9fd" "\xf019  %updates%"     
        ++ box "#ff6e67" "%enp12s0%"
        ++ box "#caa9fa" "\xf00f0  %date%"        
        ++ "%trayerpad%"

main :: IO ()
main = xmobar config