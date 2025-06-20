import Xmobar

box :: String -> String -> String
box color text = "<box type=Bottom width=2 mb=2 color=" ++ color ++ ">" ++ "<fc=" ++ color ++ ">" ++ text ++ "</fc></box>   "

simpleBox :: String -> String -> String
simpleBox color text = "<fc=" ++ color ++ ">" ++ text ++ "</fc>"

config :: Config
config = defaultConfig
    { -- Appearance
      font         = "UbuntuMono NF Bold 13"
    , bgColor      = "#2E3440"
    , fgColor      = "#D8DEE9"

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
        ++ box "#D8DEE9" "\xf0bd0  %target%"      
        ++ box "#88C0D0" "\xf099d  %vpn%"        
        ++ box "#B48EAD" "\xf0200  %ip%"         
        ++ simpleBox "#D8DEE9" "\xe777  "
        ++ box "#8FBCBB" "\xf08c7  %kernel%"      
        ++ box "#A3BE8C" "\xf4bc  %cpu%"         
        ++ box "#81A1C1" "\xf538  %memory%"      
        ++ box "#EBCB8B" "\xf0c7  %disku%"       
        ++ box "#B48EAD" "\xf030c  lang: %keyboard%" 
        ++ box "#88C0D0" "\xf019  %updates%"     
        ++ box "#BF616A" "%enp12s0%"
        ++ box "#A3BE8C" "\xf00f0  %date%"        
        ++ "%trayerpad%"

main :: IO ()
main = xmobar config