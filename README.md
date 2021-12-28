# jumper
Reach your files with a few keystrokes!

# What
Jumper is a tool that allows you to quickly open or visit files and directories by fuzzy-searching their path and/or name.

It is designed to be a "quick-launcher for your files", NOT a file search tool (no advanced querying, sorting is not customizable, etc.)

# How
Jumper shows a searchable list of files and directories; the list is populated as follows:
 1. Recent files or directories that you have visited from Jumper
 2. Files and places from Windows' "Recently Used" list
 3. Files and directories that are "close" to the above recents in the directory structure
 4. Online and offline Office documents from Microsoft Office's "Recents" list
 5. The rest of your hard drive(s), excluding "system" directories

You can quickly filter this list by typing in the text field; the search is fuzzy, so you can type pieces of a file's path and/or name until you zero in on the entry you want (or the list gets short enough that selecting the desired file with the mouse or the Up/Down arrow keys becomes practical).

After using Jumper a few times, two things happen: 
 1. The files and places that you tend to use more frequently "bubble up" in the list and become easier to find (i.e. Jumper "learns" your habits)
 2. You get better at figuring out relevant substrings to quickly find the desired files (i.e. you learn to use Jumper)

The more you use it, the better it gets ;)

# Rationale
This tool is targeted at intermediate-level, efficiency-oriented Windows users who are familiar with how files and directories work and would rather type a few keys than painfully navigate the directory structure using point&click. It is spartan by design, avoids bells/whistles and doesn't try to hide anything from the user. It does one job (help you reach your files and places) and hopefully does it well.

# Usage
Enter or double-click will open the selected file (using the default application) and close Jumper.

If you hold Ctrl, the containing directory is opened instead and the entry is selected (for online Office documents, the containing directory is opened in the web browser)

If you hold Shift, Jumper stays open.

Esc will clear the current filter text if there's any, or close Jumper if the text field is empty.

# Installation
There is no installer, so the recommended "installation" process goes like this:
 1. Unpack the distribution zip somewhere on your hard drive
 2. Navigate to the extracted directory and run "jumper.exe"
 3. Right-click on Jumper's taskbar entry and click "Pin to taskbar"
 4. [optional] Right-click on the taskbar entry, then right-click on "Racket GUI Application", then "Properties"; add a "Shortcut key" such as "Ctrl-Alt-J" to be able to start Jumper at any time using your keyboard
