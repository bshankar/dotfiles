
# set editor to nvim
set -x EDITOR nvim
set -x BROWSER firefox
set -x WEECHAT_HOME $HOME/.config/weechat
set -x NVIM_TUI_ENABLE_TRUE_COLOR 1
set -x ANDROID_HOME $HOME/.android/Sdk
set -x ANDROID_NDK_ROOT $HOME/.android/android-ndk-r15
set -x ANDROIDSDK $ANDROID_HOME
set -x ANDROIDNDK $ANDROID_NDK_ROOT
set -x ANDROIDNDKVER 10.3.2

set -gx PATH /usr/lib/ccache/bin $PATH
# Hack to make python2 detect lldb
set -x PYTHONPATH /usr/lib/python2.7/site-packages/lldb

alias vim=/usr/bin/nvim
alias diff="/usr/bin/diff --color"
alias clang++="/usr/bin/clang++ -std=c++11"
alias tmux="tmux -f ~/.config/tmux/tmux.conf"
alias java='java -Xms512M -Xmx2G -Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
alias adbc='~/.local/share/android/android-sdk/platform-tools/adb connect 10.0.0.7'
alias adbi='~/.local/share/android/android-sdk/platform-tools/adb install -r'
alias adbl='~/.local/share/android/android-sdk/platform-tools/adb logcat'

# Some yaourt shortcuts
alias y='yaourt'
alias yu='yaourt -U '
alias yupg='yaourt -Syu'
alias yaupg='yaourt -Syua'
alias yin='yaourt -S'
alias yrem='sudo pacman -Rs'
alias demo='/home/ebs/.screenlayout/demo.sh'
alias clean='/home/ebs/node_modules/cleanlang/bin/command.js'