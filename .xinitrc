xrdb ~/.Xresources
export QT_QPA_PLATFORMTHEME="qt5ct"
xkbcomp -w 0 ~/.config/xkbmap $DISPLAY
exec i3
