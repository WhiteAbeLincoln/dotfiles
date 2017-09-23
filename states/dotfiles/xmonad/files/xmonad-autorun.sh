#! /bin/bash

compton -b &
# xautolock -locker $HOME/bin/lockscript &
# $HOME/.screenlayout/screen_layout.sh &
sh $HOME/.fehbg &
dunst &
stalonetray &
wid=$(xdotool search --class stalonetray)
xdotool windowunmap $wid
$HOME/bin/barrun &
