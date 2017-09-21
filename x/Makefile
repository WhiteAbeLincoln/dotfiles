include ../Makefile.inc
DEPS = xorg xorg-xinit compton
POST_INSTALL = post-install

.PHONY: post-install
post-install:
	echo "Install a driver package from the list"
	echo "$(pacman -Ss xf86-video)"
