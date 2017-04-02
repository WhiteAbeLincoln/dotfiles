#!/usr/bin/env python3

import sys
import os
import json
import re
import subprocess
import os.path as path

def main(argv=None):
    if argv is None:
        argv = sys.argv

    directory = argv[1]

    if not os.path.isdir(directory):
        return 66

    colors = read_colors(path.join(directory, "colors.json"))
    write_xresources(colors)
    write_bash(colors)
    write_termite(colors)
    write_vim(colors)
    write_rofi(colors)
    write_wallpaper(directory)

    return 0

def read_colors(colors_file):
    if not os.path.isfile(colors_file):
        sys.exit(66)

    with open(colors_file) as f:
        return json.load(f)

def write_xresources(colors):
    with open(os.getenv("HOME") + "/.Xresources.d/colors", "w") as f:
        f.write("*.foreground: {}\n".format(colors["foreground"]))
        f.write("*.background: {}\n".format(colors["background"]))
        if "cursorColor" in colors:
            f.write("*.cursorColor: {}\n".format(colors["cursorColor"]))
        for idx,item in enumerate(colors["color"]):
            f.write("*.color{}: {}\n".format(idx, item))

    subprocess.run(["xrdb", os.getenv("HOME") + "/.Xresources"])

def write_bash(colors):
    with open(os.getenv("HOME") + "/.bash_colors", "w") as f:
        f.write('export COLORS_foreground="{}"\n'.format(colors["foreground"]))
        f.write('export COLORS_background="{}"\n'.format(colors["background"]))
        if "cursorColor" in colors:
            f.write('export COLORS_cursorColor="{}"\n'.format(colors["cursorColor"]))

        for idx,item in enumerate(colors["color"]):
            f.write('export COLORS_color{}="{}"\n'.format(idx, item))

def write_termite(colors):
    config_home = ""
    if os.getenv("XDG_CONFIG_HOME"):
        config_home = os.getenv("XDG_CONFIG_HOME")
    else:
        config_home = os.getenv("HOME") + "/.config"

    p = re.compile(r'(?<=##COLORS##)([\s\S]+?)(?=##ENDCOLORS##)', re.IGNORECASE)

    text  = "foreground= {}\n".format(colors["foreground"])
    text += "background= {}\n".format(colors["background"])
    if "cursorColor" in colors:
        text += "cursor= {}\n".format(colors["cursorColor"])

    for idx, item in enumerate(colors["color"]):
        text += "color{}= {}\n".format(idx, item)

    with open(config_home+"/termite/config", 'r+') as f:
        d = f.readlines()
        final_text = re.sub(p, "\n"+text, ''.join(d))
        f.seek(0)
        f.write(final_text)
        f.truncate()

    subprocess.run(["killall", "-USR1", "termite"])

def write_vim(colors):
    p = re.compile(r'(?<=""COLORS"")([\s\S]+?)(?=""ENDCOLORS"")', re.IGNORECASE)

    text  = "let g:terminal_color_foreground = \"{}\"\n".format(colors["foreground"])
    text += "let g:terminal_color_background = \"{}\"\n".format(colors["background"])
    if "cursorColor" in colors:
        text += "let g:terminal_color_cursor= \"{}\"\n".format(colors["cursorColor"])

    for idx, item in enumerate(colors["color"]):
        text += "let g:terminal_color_{} = \"{}\"\n".format(idx, item)

    with open(os.getenv("HOME") + "/.vim_colors", 'r+') as f:
        d = f.readlines()
        final_text = re.sub(p, "\n"+text, ''.join(d))
        f.seek(0)
        f.write(final_text)
        f.truncate()

def write_wallpaper(directory):
    wallpaper = directory + "/wallpaper"
    if os.path.isfile(wallpaper):
        subprocess.run(["feh", "--bg-fill", wallpaper])

def write_rofi(colors):
    with open(os.getenv("HOME") + "/.Xresources.d/rofi_colors", "w") as f:
        f.write("rofi.color-normal: {},{},{},{},{}\n"
                .format(
                    colors["background"], colors["foreground"], colors["background"], colors["color"][12], colors["background"]
                ))
        f.write("rofi.color-urgent: {},{},{},{},{}\n"
                .format(
                    colors["background"], colors["color"][1], colors["background"], colors["color"][9], colors["background"]
                ))
        f.write("rofi.color-active: {},{},{},{},{}\n"
                .format(
                    colors["background"], colors["color"][6], colors["background"], colors["color"][14], colors["background"]
                ))
        f.write("rofi.color-window: {},{},{}\n"
                .format(
                    colors["background"], colors["color"][8], colors["background"]
                ))
        f.write("rofi.separator-style: none")

    subprocess.run(["xrdb", os.getenv("HOME") + "/.Xresources"])


if __name__ == "__main__":
    sys.exit(main())
