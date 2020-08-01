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
alias vim=/usr/bin/nvim

starship init fish | source
zoxide init fish | source
