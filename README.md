# Botes
Botes is a simple notes keeping application made with [Lazarus IDE](https://www.lazarus-ide.org/). Written mostly for personaly use, potentially unstable and buggy - backup your allnotes.txt if you use it!

![sshot0](sshots/sshot0.png)

# Demo
Here is a self contained zip with all you need to try botes out on 64 bit Windows: [link](https://ln.sync.com/dl/bf24dbdb0/g6crp9vj-cn8cfcbi-77hjbnv9-6nxajmcp).
For reference the filename contains the commit hash from which it was built.

# Controls
* Ctrl + S - save changes.
* Ctrl + N - open a new tab.
* Ctrl + W - close current tab.
* Esc - move focus between the text area and the serach bar.
* Ctrl + Page Up/Down - switch between the open tabs.
* Ctrl + Shift + Page Up/Down - move the currently open tab left/right.
* Ctlr + F - use find (beware - it's quite crude).

# Font
For TSynEdit (the main text area) I use a popular but non-standard monoscpaced font called DejaVu Sans Mono that comes from a permissively licensed family of fonts called DejaVu Fonts. If you do not have it installed then the text area might look wrong, different than in the screenshots, use some fallback font, etc. You can get the DejaVu Fonts here: [link](https://dejavu-fonts.github.io/).

# Idea
The concept of botes materialized due to few varied reasons:
* Loss of varius txt note files I kept during an urgent disk reformat after a virus struck
* Proliferation of txt files with per project notes, todo lists, etc. in many locations
* My desire to have some fun using Object Pascal again after a long break
* Speed and ease with which simple applications in Lazarus using LCL can be made
* The *interesting* reactions I get for saying I use Pascal in 2017

The idea behind botes is to be a simple, highly shortcut and keyborad controlable application to store notes in a single txt file (for ease of backing up, copying, etc.), split into sections tagged by hashtags. That file right now is named allnotes.txt and resides next to the exe, this may change or be configurable (or the application may allow opening and working with different files - but always with one at a time) in a future release.

A section is simply a block of text from one line with one or more hashtags until another such line or end of file. All blocks tagged with a certain hashtag can be looked up by typing the hashtag (without the hash sign) into the bar at the top. There are also tabs to have more than one section open at once but it still works within a single file, these are saved in tabs.txt and reopened at next launch.

# Building
There is an .lpi file included, it should allow buildiding out of the box on a fresh Lazarus installation. You can also ask me for an exe for Windows if you really want one. Should also build out of the box on other toolkits and systems LCL supports but that wasn't tested.

# Future development
I write botes mainly for my personal use and my future development plans for it change as new needs arise and I toy with ideas and LCL widgets but I welcome any feedback.

# Dracula
In the repo there is a copy of the original book Dracula by Bram Stoker, I added it to serve as a big lorem ipsum style piece of text to make sure there are no performance regressions due to looking for hashtags, styling lines, etc. I could have used any book or generated garbage but I specifically chose the Dracula book because I really like it and the character of Dracula himself as portrayed in other more recent media.

# Name
The name botes comes from the word notes and the first letter of my first name.
