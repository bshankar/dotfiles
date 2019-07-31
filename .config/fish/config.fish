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
alias cat="bat"

alias kd="kubectl --context=adori-dev"
alias kb="kubectl --context=adori-beta"
alias ke="kubectl exec (kubectl get po | awk '{print $1;}' | fzf) -it sh"

alias kud="kubectl config use-context adori-dev"
alias kub="kubectl config use-context adori-beta"

alias kbb="kb apply -f /home/ebs/Documents/code/adori/infra/beta/backend-v5-1.yaml"
alias kdb="kd apply -f /home/ebs/Documents/code/adori/infra/dev/backend-v5-1.yaml"

alias tpdcb="kud; telepresence --copy-deployment backend-v5"
alias tpdsb="kud; telepresence --swap-deployment backend-v5"
alias tpbcb="kub; telepresence --copy-deployment backend-v5"

bass export LC_ALL=en_US.utf-8
bass export LANG=en_US.utf-8
set -gx PATH /home/ebs/.cargo/bin $PATH