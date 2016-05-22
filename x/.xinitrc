#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then







    xrdb -merge $sysresources

fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then







    xrdb -merge -I=$HOME "$userresources"

fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# twm &
# xclock -geometry 50x50-1+1 &
# xterm -geometry 80x50+494+51 &
# xterm -geometry 80x20+494-0 &
# exec xterm -geometry 80x66+0+0 -name login

# swap caps and escape
setxkbmap -option caps:swapescape
xsetroot -cursor_name left_ptr &

# Window manager specific programs

# homepage server
$HOME/Documents/Projects/homepage/server/server.py &

function xmonad_programs {
    # start the compositor
    compton -b &

    xscreensaver &

    # fix screen settings
    $HOME/.screenlayout/screen_layout.sh

    # set the wallpaper
    sh ~/.fehbg &

    # start the notification daemon
    dunst &

    # start lemonbar
#    ~/bin/barrun &
}

function pantheon_programs {
    gsettings-data-convert &
    xdg-user-dirs-gtk-update &
    /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
    /usr/lib/gnome-settings-daemon/gnome-settings-daemon &
    /usr/lib/gnome-user-share/gnome-user-share &
    eval $(gnome-keyring-daemon --start --components=pkcs11,secrets,ssh,gpg)
    export GNOME_KEYRING_CONTROL GNOME_KEYRING_PID GPG_AGENT_INFO SSH_AUTH_SOCK
}

session=${1:-xmonad}

case $session in
    awesome           ) exec awesome;;
    bspwm             ) exec bspwm;;
    catwm             ) exec catwm;;
    cinnamon          ) exec cinnamon-session;;
    dwm               ) exec dwm;;
    enlightenment     ) exec enlightenment_start;;
    ede               ) exec startede;;
    fluxbox           ) exec startfluxbox;;
    gnome             ) exec gnome-session;;
    gnome-classic     ) exec gnome-session --session=gnome-classic;;
    i3|i3wm           ) exec i3;;
    icewm             ) exec icewm-session;;
    jwm               ) exec jwm;;
    kde               ) exec startkde;;
    mate              ) exec mate-session;;
    monster|monsterwm ) exec monsterwm;;
    notion            ) exec notion;;
    openbox           ) exec openbox-session;;
    unity             ) exec unity;;
    xfce|xfce4        ) exec startxfce4;;
    pantheon|elementary) pantheon_programs; exec cerbere;;
    xmonad            ) xmonad_programs; exec xmonad;;
    # No known session, try to run it as command
    *) exec $1;;
esac
