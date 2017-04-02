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
        f.write("*.foreground: #{}\n".format(colors[5]))
        f.write("*.background: #{}\n".format(colors[0]))
        f.write("*.cursorColor: #{}\n".format(colors[5]))

        f.write("*.color{}: #{}\n".format(0, colors[0]))
        f.write("*.color{}: #{}\n".format(1, colors[8]))
        f.write("*.color{}: #{}\n".format(2, colors[11]))
        f.write("*.color{}: #{}\n".format(3, colors[10]))
        f.write("*.color{}: #{}\n".format(4, colors[13]))
        f.write("*.color{}: #{}\n".format(5, colors[14]))
        f.write("*.color{}: #{}\n".format(6, colors[12]))
        f.write("*.color{}: #{}\n".format(7, colors[5]))

        f.write("*.color{}: #{}\n".format(8, colors[3]))
        f.write("*.color{}: #{}\n".format(9, colors[9]))
        f.write("*.color{}: #{}\n".format(10, colors[1]))
        f.write("*.color{}: #{}\n".format(11, colors[2]))
        f.write("*.color{}: #{}\n".format(12, colors[4]))
        f.write("*.color{}: #{}\n".format(13, colors[6]))
        f.write("*.color{}: #{}\n".format(14, colors[15]))
        f.write("*.color{}: #{}\n".format(15, colors[7]))

    subprocess.run(["xrdb", os.getenv("HOME") + "/.Xresources"])

def write_bash(colors):
    with open(os.getenv("HOME") + "/.bash_colors", "w") as f:
        f.write('export COLORS_foreground="#{}"\n'.format(colors[5]))
        f.write('export COLORS_background="#{}"\n'.format(colors[0]))
        f.write('export COLORS_cursorColor="#{}"\n'.format(colors[6]))

        for idx,item in enumerate(colors):
            f.write('export COLORS_color{}="#{}"\n'.format(idx, item))

def write_termite(colors):
    config_home = ""
    if os.getenv("XDG_CONFIG_HOME"):
        config_home = os.getenv("XDG_CONFIG_HOME")
    else:
        config_home = os.getenv("HOME") + "/.config"

    p = re.compile(r'(?<=##COLORS##)([\s\S]+?)(?=##ENDCOLORS##)', re.IGNORECASE)

    text  = "foreground= #{}\n".format(colors[5])
    text += "foreground_bold= #{}\n".format(colors[6])
    text += "cursor= #{}\n".format(colors[6])
    text += "background= #{}\n".format(colors[0])

    text += "color{} = #{}\n".format(0, colors[0])
    text += "color{} = #{}\n".format(8, colors[3])
    text += "color{} = #{}\n".format(7, colors[5])
    text += "color{} = #{}\n".format(15, colors[7])

    text += "color{} = #{}\n".format(1, colors[8])
    text += "color{} = #{}\n".format(9, colors[8])

    text += "color{} = #{}\n".format(2, colors[11])
    text += "color{} = #{}\n".format(10, colors[11])

    text += "color{} = #{}\n".format(3, colors[10])
    text += "color{} = #{}\n".format(11, colors[10])

    text += "color{} = #{}\n".format(4, colors[13])
    text += "color{} = #{}\n".format(12, colors[13])

    text += "color{} = #{}\n".format(5, colors[14])
    text += "color{} = #{}\n".format(13, colors[14])

    text += "color{} = #{}\n".format(6, colors[12])
    text += "color{} = #{}\n".format(14, colors[12])

    text += "color{} = #{}\n".format(16, colors[9])
    text += "color{} = #{}\n".format(17, colors[15])
    text += "color{} = #{}\n".format(18, colors[1])
    text += "color{} = #{}\n".format(19, colors[2])
    text += "color{} = #{}\n".format(20, colors[4])
    text += "color{} = #{}\n".format(21, colors[6])

    with open(config_home+"/termite/config", 'r+') as f:
        d = f.readlines()
        final_text = re.sub(p, "\n"+text, ''.join(d))
        f.seek(0)
        f.write(final_text)
        f.truncate()

    subprocess.run(["killall", "-USR1", "termite"])

def write_vim(colors):
    p = re.compile(r'(?<=""COLORS"")([\s\S]+?)(?=""ENDCOLORS"")', re.IGNORECASE)

    text = ''

    for idx in range(10):
        text += 'let s:base0{} = "{}"\n'.format(idx, colors[idx])

    text += 'let s:base0{} = "{}"\n'.format('A', colors[10])
    text += 'let s:base0{} = "{}"\n'.format('B', colors[11])
    text += 'let s:base0{} = "{}"\n'.format('C', colors[12])
    text += 'let s:base0{} = "{}"\n'.format('D', colors[13])
    text += 'let s:base0{} = "{}"\n'.format('E', colors[14])
    text += 'let s:base0{} = "{}"\n'.format('F', colors[15])
    num0 = int("0x"+colors[0], 16)
    num7 = int("0x"+colors[7], 16)

    if num0 > num7:
        # this is a light theme
        text += 'set background=light\n'
    else:
        text += 'set background=dark\n'

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
        f.write("rofi.color-normal: #{},#{},#{},#{},#{}\n"
                .format(
                    colors[0], colors[5], colors[0], colors[12], colors[0]
                ))
        f.write("rofi.color-urgent: #{},#{},#{},#{},#{}\n"
                .format(
                    colors[0], colors[1], colors[0], colors[9], colors[0]
                ))
        f.write("rofi.color-active: #{},#{},#{},#{},#{}\n"
                .format(
                    colors[0], colors[6], colors[0], colors[14], colors[0]
                ))
        f.write("rofi.color-window: #{},#{},#{}\n"
                .format(
                    colors[0], colors[8], colors[0]
                ))
        f.write("rofi.separator-style: none")

    subprocess.run(["xrdb", os.getenv("HOME") + "/.Xresources"])


if __name__ == "__main__":
    sys.exit(main())
