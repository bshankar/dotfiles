function fish_title
  true
end

function fish_greeting
  true
end

# set editor to nvim
set -x EDITOR nvim
set -x BROWSER firefox-developer-edition

keychain -q --agents gpg,ssh; keychain -q --eval id_ed25519

alias vim=/usr/bin/nvim
alias diff="/usr/bin/diff --color"
alias java='java -Xms512M -Xmx2G -Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'

alias ys="yarn serve"
alias yb="yarn build"
alias yl="yarn lint"
alias yi="yarn install"

alias aria2c="aria2c -x8 -s8"
alias ls="exa"

alias kp="kubectl --context=adori-prod"
alias kd="kubectl --context=adori-dev"
alias kb="kubectl --context=adori-beta"

alias kup="kubectl config use-context adori-prod"
alias kud="kubectl config use-context adori-dev"
alias kud="kubectl config use-context adori-beta"

alias kpb="kp apply -f /home/ebs/Documents/code/adori/infra/prod/backend-v5.yaml"
alias kdb="kd apply -f /home/ebs/Documents/code/adori/infra/dev/backend-v5.yaml"
alias kpt="kp apply -f /home/ebs/Documents/code/adori/infra/prod/thumbor-server.yaml"
alias kdt="kd apply -f /home/ebs/Documents/code/adori/infra/dev/thumbor-server.yaml"

alias tppcb="kup; telepresence --copy-deployment backend-v5 --also-proxy=10.128.0.0/16"
alias tpdcb="kud; telepresence --copy-deployment backend-v5"
alias tpdsb="kud; telepresence --swap-deployment backend-v5"