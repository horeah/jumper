# jumper
Reach your files with a few keystrokes!

# What
Jumper is a tool that allows you to quickly open or visit files and directories by fuzzy-searching their path and/or name.

# How
Jumper shows a searchable list of files and directories; the list is populated as follows:
 1. Recent files or places that you have visited from Jumper (the first time you run Jumper: your home dir)
 2. Files and directories from Windows' "Recently Used" list
 3. Online Office documents from Microsoft Office's "Recents" list
 4. The rest of your hard drive(s), excluding "system" directories

You can quickly filter this list by typing in the text field; the search is fuzzy, so you can type pieces of a file's path and/or name until you zero in on the entry you want (or the list gets short enough that selecting the desired file with the mouse or the Up/Down arrow keys becomes practical).

After using Jumper a few times, the files and places that you tend to use more frequently "bubble up" in the list and become easier to find. The more you use it, the better it gets ;)

# Rationale
This tool is targeted at intermediate-level, efficiency-oriented Windows users who are familiar with how files and directories work and would rather type a few keys than painfully navigate the directory structure using point&click. It is spartan by design, avoids bells/whistles and doesn't try to hide anything from the user. It does one job (help you reach your files and places) and hopefully it does it well.

# Usage
Enter or double-click will open the selected file (using the default application) and close Jumper.

If you hold Ctrl, the containing directory is opened instead and the entry is selected (except for online Office documents, which are opened in the web browser)

If you hold Shift, Jumper stays open.

Esc will clear the current filter text if there's any, or close Jumper if the text field is empty.

# Installation
There is no installer, so the recommended "installation" process goes like this:
 1. Unpack the distribution zip somewhere on your hard drive
 2. Navigate to the extracted directory and run "jumper.exe"
 3. Right-click on Jumper's taskbar entry and click "Pin to taskbar"
 4. [optional] Right-click on the taskbar entry, then right-click on "Jumper", then "Properties"; add a "Shortcut key" such as "Ctrl-Alt-J" to be able to start Jumper at any time using your keyboard
