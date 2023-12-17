# My dot files for setting up new laptops in the future

## Emacs theme

Put the file `./emacs-theme/actraiser-theme.el` under the folder `~/.emacs.d` in order to use the theme.

## Using keyd (key-daemon) to change out layout of multiple keyboards

[keyd](https://github.com/rvaiya/keyd) is a tool for managing layout of different keyboards. What you need to do is to install it and set up the config file under `/etc/keyd/default.conf`.

Check out the files in the repo for my version of the `default.conf`.

Here are some of the useful commands that `keyd` provides:

```
sudo systemctl enable keyd && sudo systemctl start keyd # run this after install keyd

sudo keyd monitor # this would monitor your key presses from multiple input sources

sudo key reload # this would apply the changes to the system

sudo journalctl --pager-end --unit=keyd # check for errors with keyd
```

## Keyboard layout for edge case scenerio (check the newer guide)

## Persistent keyboard layout switch

https://bbs.archlinux.org/viewtopic.php?id=262467

https://stackoverflow.com/questions/58019772/need-to-toggle-layout-globally-instead-of-per-window-using-setxkbmap

https://askubuntu.com/questions/501659/how-to-swap-command-and-control-keys-with-xkb-step-by-step/501660#501660

## The guide itself

Here is the guide to set up a custom keyboard layout (swap capslock with control and backspace with backslash):

Before doing anything, download the custom symbol file and put it under the folder `/usr/share/X11/xkb/symbols`.

First, edit `/usr/share/X11/xkb/rules/evdev`, add the following entry under `! option = symbols`:

```
custom:hhkb_layout                   = +custom(hhkb_layout)
```

Second, add the following under `/usr/share/X11/xkb/rules/evdev.lst`:

```
custom:hhkb_layout                   Adapt HHKB layout for normal keyboards
```

Thirdly, get the device id of your desired keyboard to map on (you need to have xinput installed):

```
xinput -list | grep -i key
```

Here is an example output:

```
⎣ Virtual core keyboard                         id=3    [master keyboard (2)]                                                              [0/2167]
    ↳ Virtual core XTEST keyboard               id=5    [slave  keyboard (3)]
    ↳ Power Button                              id=6    [slave  keyboard (3)]
    ↳ Video Bus                                 id=7    [slave  keyboard (3)]
    ↳ Video Bus                                 id=8    [slave  keyboard (3)]
    ↳ Power Button                              id=9    [slave  keyboard (3)]
    ↳ Sleep Button                              id=10   [slave  keyboard (3)]
    ↳ Topre Corporation HHKB Professional       id=11   [slave  keyboard (3)]
    ↳ DELL0926:00 044E:1220 UNKNOWN             id=15   [slave  keyboard (3)]
    ↳ Intel HID events                          id=16   [slave  keyboard (3)]
    ↳ Intel HID 5 button array                  id=17   [slave  keyboard (3)]
    ↳ Dell WMI hotkeys                          id=18   [slave  keyboard (3)]
    ↳ AT Translated Set 2 keyboard              id=19   [slave  keyboard (3)]
```

If I want to remap device with `id=15`, I would do the following:

```
setxkbmap -device 15 -option custom:hhkb_layout
```

Finally, if you want to make the changes permanent, add the `setxkbmap` line to your startup script (.bashrc or .xinitrc)

## Fonts

- Iosevka

## Programs for development

- urxvt:        terminal
- tmux:         terminal overlay (multiplexer)
- vim:          text editor
- emacs:        text editor
- mypaint:      paint utility
- xournalapp:   note taking app
- i3:           window manager
- i3status:     statusbar
- i3lock:       screen locker
- flameshot:    screenshoot tool
- evince/mupdf: pdf viewer
- chromium:     web browser
- feh:          image viewer
- ibus:         for multiple language support
- obs:          screen recorder, streaming
- [alttab](https://github.com/sagb/alttab): alt-tab feature for x11

- build-essential
- GLFW, GL, SDL
- pulseaudio
- autoconf
- unzip
- wget
- libtool

## Guide to install custom fonts without root priviledges

for Truetype fonts, the installation process would be like this:

```
mkdir ~/.fonts
```

Move your fonts to `~/.fonts`

```
mv *.ttf ~/.fonts
```

Create the fonts.dir and fonts scale files

```
cd ~/.fonts
mkfontscale
mkfontdir
fc-cache -fv ~/.fonts
```

The new font should now be available. Check if the font has been added by running `fc-list`

```
fc-list
```

You are all set
