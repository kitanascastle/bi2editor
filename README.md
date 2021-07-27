# Project Title

Battle Isle II + III Map Editor

## Description

A map editor for Battle Isle II, Battle Isle III, and Battle Isle 2020 mission files (*.DAT).
This tool allows editing all features of existing Battle Isle maps as well as creating new
maps from scratch. Also supports special features from Battle Isle 2020.

## Getting Started

### System requirements

- Microsoft Windows XP/2003/Vista/Win7/Win8/Win10
- Microsoft DirectX 9.0c
- ca 20 MB free disk space
- PowerBasic Console Compiler - PBCC 6.0

If you just want to use the map editor without compiling it first, you can
download the latest executable from:
https://www.kitana.org/arena/bi2/bi2editor.html

### Installation for Battle Isle II

1. Extract the content of the ZIP file to any folder. This folder becomes the
   application folder of the Battle Isle II Map Editor.

2. Copy the whole content of your "Battle Isle II" folder into the "BI2" folder
   of the just created application folder. The Battle Isle II Map Editor will
   overwrite the file "MISS000.DAT" when testing a map, so it is smarter to
   copy Battle Isle II to the editor's application folder instead of copying
   the editor to the folder from which you want to play the original maps one
   day.

3. Delete all savegames from the folder "BI2\SAV" so Battle Isle II will
   directly load your map instead of the last saved game. This saves you some
   clicks when testing your maps.

4. Copy the whole content of your "DOSBox" installation to the folder "DOSBox"
   of the just created application folder.

5. Open file "DOSBox\bi2ed.conf" in a text editor and modify the bottom-most
   line to make the game correctly detect your "Battle Isle II" CD. Currently
   this line should read:

   mount e f:\ -t cdrom -usecd 0 -ioctl -label BATTLEISLE2

   "e" is the drive letter that you have specified as CD-ROM drive during the
   installation of "Battle Isle II" and "f:\" is your Windows path to your
   CD drive.

### Installation for Battle Isle III

1. Extract the content of the ZIP file to any folder. This folder becomes the
   application folder of the Battle Isle III Map Editor.

2. Copy the whole content of your "Battle Isle III" folder into the "BI3" folder
   of the just created application folder. The Battle Isle III Map Editor will
   overwrite the file "MISS100.DAT" when testing a map, so it is smarter to
   copy Battle Isle III to the editor's application folder instead of copying
   the editor to the folder from which you want to play the original maps one
   day.

### Executing program

Run bi2editor.exe

## Operation

Left mouse button on map: Draw
Right mouse button on map: Select shop
Hold right mouse button: Scroll map
Mouse wheel: Zoom map
Shift: Select map section
ESC: Unselect all
F: Fill selected section with selected terrain
CTRL+C: Copy selected section
CTRL+V: Paste copied selection at cursor position
CTRL+Z: Undo last change
CTRL+O: Open map
CTRL+S: Save map
DEL: Delete unit at cursor position or delete object if no unit at cursor position
Left mouse button on production/content slot: Insert selected unit
Right mouse button on production/content slot: Clear slot
0..9: Show DF-layer 0..9
,: Hide DF-layer and show normal layer only
S: Highlight shops
I: Select map element at cursor position
F5: Test map inBattle Isle II

## Author

Daniel Bekowies - sourcerer@kitana.org

## Version History

v1.008 / 13-July-2021
* Added support for Battle Isle 3

## License

This project is licensed under the GNU General Public License v3.0 - see the LICENSE.md file for details