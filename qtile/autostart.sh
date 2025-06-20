#!/bin/sh

# systray battery icon
cbatticon -u 5 &
# systray volume
volumeicon &
# xorg compositor
picom -b
# set wallpaper with nitrogen
nitrogen --restore

# background with feh
# feh --bg-scale /home/a1nz/Downloads/archred.png
# maps keyboard spanish
# setxkbmap latam
