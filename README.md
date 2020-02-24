# Emacs configuration

Cross platform emacs configuration.
Should work in Linux/Mac OS X/Windows.
For packages and features see `emacs_init.org`.

## Installation

Installation is as simple as cloning the repo and start Emacs.

Linux/Mac: `cd && git clone https://github.com/storvik/.emacs.d`

Windows Git Bash: `cd && cd AppData/Roaming && git clone https://github.com/storvik/.emacs.d`

## Caps lock and Ctrl

Remap caps lock to ctrl in windows by running `remapcapslock.reg`.

## Emacs 27

Build Emacs 27 in Ubuntu by running:

``` shell
git clone --branch emacs-27 --depth=1 https://github.com/emacs-mirror/emacs.git
cd emacs/
sudo apt-get build-dep emacs26
./autogen.sh
./configure
make
sudo make install
```
