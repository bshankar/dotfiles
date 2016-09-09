Dotfiles
========

My configuration files for i3 window manager, neovim, fish, termite, tmux, weechat and dunst.
Also contains a script to start a software wifi access point and the font Inconsolata-g patched
with nerd fonts patcher. Here are some highlights: 

i3 wm
-----
Replaced the default status bar with goi3bar and and use font awesome icons.

neovim
------
Using Gruvbox theme. 30+ plugins and some neat tweaks with markdown, C, C++, Python, Scala 
and Go in mind. 

fish
----
Starts i3 wm at login, sets the configuration files for tmux, weechat and some environment 
variables for neovim and clang (use c++11 by default using alias). 
fish_greeting uses cowfortune to give a random quote when terminal is invoked.

weechat
-------
Uses Font awesome icons

termite
-------
Uses Gruvbox theme and Inconsolata-g font.

.packages/my_apps.txt
---------------------
List of installed packages (arch linux)

Gtk/qt theme
------------
Theme is based on clearlooks, faenza icons and Cantarell fonts.

.initSoftAp
-----------
(Requires hostapd and dnsmasq installed)
Create a wifi hotspot. Very handy for sharing your wired internet of 
your pc/ laptop with an Android phone.

$ sudo ./.initSoftAp <wifi interface> <wired interface>
Ex: sudo ./.initSoftAp wlp5s0 enp9s0


TODO
====
Make an installation script
