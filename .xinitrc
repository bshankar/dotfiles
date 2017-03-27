xrdb ~/.Xresources
export QT_QPA_PLATFORMTHEME="qt5ct"
xkbcomp -w 0 ~/.config/xkbmap $DISPLAY

#if [ -d /etc/X11/xinit/xinitrc.d ] ; then
# for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
#  [ -x "$f" ] && . "$f"
# done
# unset f
#fi
exec ck-launch-session sway

