module Main (main) where

import Xmobar
import Text.Printf (printf)

config :: Config
config = defaultConfig 
  { font = "JetBrainsMono Nerd Font Bold 10"
  , border = NoBorder
  , bgColor = "#1F1F28"
  , fgColor = "#DCD7BA"
  , position = Top
  , commands = [ Run XMonadLog
               , Run $ Cpu ["-t","<total>%"] 10
               , Run $ Memory ["-t","<used> Mb"] 10
               , Run $ DiskU [("/","<used>/<size>")] [] 10
               , Run $ Com "uname" ["-r"] "uname" 36000
               , Run $ CommandReader "rofi -show drun" "menu"
               , Run $ Date "%b %_d - %I:%M" "date" 10
               ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = printf " %s | %%XMonadLog%% }{ %s %s %s %s %s " linux' uname' disk' cpu' mem' date'
  }
  where
    linux' = "<action=`rofi -show drun` button=1><fc=#16161D,#7E9CD8> \61820 </fc></action>"
    uname'= "<fc=#DCD7BA>%uname%</fc>"
    disk' = "<fc=#16161D,#FF9E3B> \61600  </fc><fc=#16161D,#DCA561> %disku% </fc>"
    cpu'  = "<fc=#16161D,#658594> \61704  </fc><fc=#16161D,#7E9CD8> %cpu% </fc>"
    mem'  = "<fc=#16161D,#957FB8> \984203  </fc><fc=#16161D,#938AA9> %memory% </fc>"
    date' = "<fc=#16161D,#E46876> \985428 </fc><fc=#16161D,#D27E88> %date% </fc>"

main :: IO ()
main = xmobar config  -- or: configFromArgs config >>= xmobar
