function fish_title
  true
end

function fish_greeting
  true
end

# set editor to nvim
set -x EDITOR nvim
set -x BROWSER firefox
set -gx PATH /home/ebs/.cargo/bin /home/ebs/.emacs.d/bin $PATH
set -x RUSTC_WRAPPER /usr/bin/sccache

alias vim=/usr/bin/nvim
alias gcc='/usr/bin/sccache /usr/bin/gcc'
alias cc='/usr/bin/sccache /usr/bin/gcc'
alias g++='/usr/bin/sccache /usr/bin/g++'
alias c++='/usr/bin/sccache /usr/bin/g++'

starship init fish | source
zoxide init fish | source
