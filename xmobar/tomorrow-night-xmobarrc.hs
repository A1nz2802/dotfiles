import Xmobar
import System.Process (readProcess)
import qualified Data.Map as M

type ColorMap = M.Map String String

-- main config
makeConfig :: String -> Config
makeConfig iface = defaultConfig
  { font = "UbuntuMono NF Bold 13"
  , bgColor = getColor "bg"
  , fgColor = getColor "fg"
  , position = TopSize L 100 30
  , lowerOnStart = True
  , hideOnStart = False
  , allDesktops = True
  , persistent = True
  , commands = [
      Run $ Com ".local/bin/kernel" [] "kernel" 36000,
      Run $ Com ".local/bin/keyboard_layout" [] "keyboard" 36000,
      Run $ Com ".local/bin/pacupdate" [] "updates" 30000,
      Run $ Com ".local/bin/target" [] "target" 60,
      Run $ Com ".local/bin/vpn_status" [] "vpn" 60,
      Run $ Com ".local/bin/ethernet_status" [] "ip" 36000,
      Run $ Network iface ["-t", "<fn=2>\xf01a</fn>  <rx>kb <fn=2>\xf01b</fn>  <tx>kb"] 15,
      Run $ Cpu ["-t", "cpu: (<total>%)","-H","50","--high","red"] 50,
      Run $ Memory ["-t", "mem: <used>M (<usedratio>%)"] 20,
      Run $ DiskU [("/", "nvme: <free> free")] [] 60,
      Run $ Date "%b %d %Y - (%H:%M)" "date" 50,
      Run $ Com ".config/xmobar/trayer-padding-icon.sh" [] "trayerpad" 20,
      Run UnsafeStdinReader
    ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = templateStr iface
  }

colorScheme :: ColorMap
colorScheme = M.fromList [
    ("bg", "#1d1f21"),
    ("fg", "#ffffff"),
    ("xmonad", "#ff3334"),
    ("target", "#ffffff"),
    ("vpn", "#b77ee0"),
    ("ip", "#f0c674"),
    ("sep", "#ffffff"),
    ("kernel", "#cc6666"),
    ("cpu", "#b5bd68"),
    ("memory", "#e6c547"),
    ("disk", "#81a2be"),
    ("keyboard", "#b294bb"),
    ("updates", "#70c0ba"),
    ("network", "#ff3334"),
    ("date", "#9ec400")
  ]

templateStr :: String -> String
templateStr iface = "<fc=" ++ getColor "xmonad" ++ "> \xf35e </fc> %UnsafeStdinReader% }{ "
  ++ box (getColor "target") "\xf0bd0  %target%"
  ++ box (getColor "vpn") "\xf099d  %vpn%"
  ++ box (getColor "ip") "%ip%"
  ++ simpleBox (getColor "sep") "\xe777  "
  ++ box (getColor "kernel") "\xf08c7  %kernel%"
  ++ box (getColor "cpu") "\xf4bc  %cpu%"
  ++ box (getColor "memory") "\xf538 %memory%"
  ++ box (getColor "disk") "\xf0c7  %disku%"
  ++ box (getColor "keyboard") "\xf030c  lang: %keyboard%"
  ++ box (getColor "updates") "\xf019  %updates%"
  ++ box (getColor "network") ("%" ++ iface ++ "%")
  ++ endbox (getColor "date") "\xf00f0  %date%"
  ++ "%trayerpad%"

-- helpers
getColor :: String -> String
getColor key = M.findWithDefault "#ffffff" key colorScheme

box :: String -> String -> String
box color text = "<box type=Bottom width=2 mb=2 color=" ++ color ++ ">" ++ "<fc=" ++ color ++ ">" ++ text ++ "</fc></box>   "

simpleBox :: String -> String -> String
simpleBox color text = "<fc=" ++ color ++ ">" ++ text ++ "</fc>"

endbox :: String -> String -> String
endbox color text = "<box type=Bottom width=2 mb=2 color=" ++ color ++ ">" ++ "<fc=" ++ color ++ ">" ++ text ++ "</fc></box>"

getInterface :: IO String
getInterface = do
  output <- readProcess "/home/a1nz/.local/bin/iface" [] ""
  return (init output)

main :: IO ()
main = do
  iface <- getInterface
  xmobar (makeConfig iface)