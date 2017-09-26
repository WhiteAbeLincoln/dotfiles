# dotfiles

This repository uses [saltstack](https://saltstack.com) to manage dotfiles. See [tutorial](https://docs.saltstack.com/en/getstarted) for more information.

## Installing
You can use [make](https://www.gnu.org/software/make/) to install dependencies.
First ensure the configuration options in `Makefile.inc` are correct, then run `make install`.
To manually install, first run `make python-reqs`, source the virtual env with `. .venv/bin/activate`,
then run `sudo salt-call state.apply $STATEFILE`. Depending on your distribution, you may have to export your current path to sudo (`sudo env "PATH=$PATH" salt-call ....`)

### Example
```
~/dotfiles      $ sudo salt-call basic.tmux
```
