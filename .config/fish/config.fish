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
alias vim=/usr/bin/nvim
alias clang++="/usr/bin/clang++ -std=c++11"
alias tmux="tmux -f ~/.config/tmux/tmux.conf"
