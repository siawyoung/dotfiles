# Programming the GH60

I'm using a mostly standard Poker 60% layout, with the option and command keys switched for Mac usage. As for the fn layer, it's inspired by HHKB, with the rightmost key on the bottom row acting as fn toggle and the `[;'/` cluster as arrow keys, as well as `asd` for media keys (volume up, down, mute). `fn + esc` also toggles the backtick/tilde key (i.e. `fn + shift + esc` for tilde)

# Steps

To create or modify keymaps, we use EasyAVR, and `dfu_programmer` for flashing the board.

```
$ brew install dfu_programmer
$ git clone https://github.com/dhowland/EasyAVR
$ cd EasyAVR
$ chmod +x easykeymap.sh
$ ./easykeymap.sh
```

Select "File > New Default Layout" or "File > Open Saved Layout" to create or modify keymap respectively.

Once done, save the keymap as a data file, and build it with "File > Save Layout As" and "File > Build Firmware".

Then, to flash the board, press the BOOT button located beneath the keyboard. At this point, your keyboard should stop working. Then, erase the board and flash it given the location of the built keymap.

```
$ dfu_programmer atmega32u4 erase
$ dfu_programmer atmega32u4 flash ./gh60.hex
```

Then, press the BOOT button to reboot the keyboard, plug the keyboard out and in again, and everything should work.
