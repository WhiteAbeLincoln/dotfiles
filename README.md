# dotfiles

This repository uses gnu [stow](https://www.gnu.org/software/stow/manual/stow.html) to manage dotfiles. See [here](http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html) for more information.

## Installing
You can use [make](https://www.gnu.org/software/make/) to install dependencies and stow the files.
First ensure the configuration options in `Makefile.inc` are correct, then run `make all`.
If you aren't running Arch Linux, you may have to modify the `DEPS` variable and change the package names in the sub makefiles.

To install an individual option, switch to its directory and run `make install`.

### Example
```
~/dotfiles      $ make all          # Installs dependencies and stows all files by recursively running Make
~/dotfiles/git  $ make install      # Installs git and stows git configuration
```
