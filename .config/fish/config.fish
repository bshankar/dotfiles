function fish_title
  true
end

# set editor to nvim
set -x EDITOR nvim
set -x BROWSER firefox-developer

alias vim=/usr/bin/nvim
alias diff="/usr/bin/diff --color"
alias java='java -Xms512M -Xmx2G -Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
alias adbc='~/.local/share/android/android-sdk/platform-tools/adb connect 10.0.0.7'
alias adbi='~/.local/share/android/android-sdk/platform-tools/adb install -r'
alias adbl='~/.local/share/android/android-sdk/platform-tools/adb logcat'

cowfortune
