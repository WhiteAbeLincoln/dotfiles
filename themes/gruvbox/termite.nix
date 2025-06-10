{
  bg,
  fg,
  color0,
  gray,
  reds,
  greens,
  yellows,
  blues,
  purples,
  aquas,
  contrasts,
}: {
  backgroundColor = bg;
  foregroundColor = fg;
  foregroundBoldColor = fg;
  colorsExtra = ''
    color0 = ${color0}
    color8 = ${gray}

    color1 = ${reds.primary}
    color9 = ${reds.secondary}

    color2 = ${greens.primary}
    color10 = ${greens.secondary}

    color3 = ${yellows.primary}
    color11 = ${yellows.secondary}

    color4 = ${blues.primary}
    color12 = ${blues.secondary}

    color5 = ${purples.primary}
    color13 = ${purples.secondary}

    color6 = ${aquas.primary}
    color14 = ${aquas.secondary}

    color7 = ${contrasts.primary}
    color15 = ${contrasts.secondary}
  '';
}
