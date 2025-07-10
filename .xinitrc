#!/bin/sh
xrdb -merge ~/.Xresources &

export XCURSOR_THEME="Kafka"
export XCURSOR_SIZE=64

exec xmonad
