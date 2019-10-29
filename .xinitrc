xrdb ~/.Xresources
set -x PATH ~/.cargo/bin:$PATH
xkbcomp -w 0 ~/.config/xkbmap $DISPLAY
