# start X at login
if status --is-login
    if test -z "$DISPLAY" -a $XDG_VTNR -eq 1
        exec startx -- -keeptty
    end
end

# set editor to nvim
set -x EDITOR nvim
set -x BROWSER firefox
set -x WEECHAT_HOME $HOME/.config/weechat
set -x NVIM_TUI_ENABLE_TRUE_COLOR 1

# Hack to make python2 detect lldb
set -x PYTHONPATH /usr/lib/python2.7/site-packages/lldb

alias vim=/usr/bin/nvim
alias clang++="/usr/bin/clang++ -std=c++11"
alias tmux="tmux -f ~/.config/tmux/tmux.conf"
alias java='java -Xms512M -Xmx2G -Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'

# Some pacman shortcuts
alias bb='bauerbil'
alias bba='sudo bauerbill --aur'
alias bbupg='sudo bauerbill -Syu'
alias bbaupg='sudo bauerbill --aur -Syu'
alias bbsearch='sudo bauerbill -Ss'
alias bbasearch='sudo bauerbill --aur -Ss'
alias bbinstall='sudo bauerbill -S'
alias bbainstall='sudo bauerbill --aur -S'
alias bbdel='sudo pacman -Rs'
