# Change these settings to match your configuration

INSTALL = sudo pacman -S --noconfirm
AURINSTALL = yes | sudo aura -Aa
UNINSTALL = sudo pacman -Rcns
ROOT = /home/abe/dotfiles
HOME = /home/abe

## Don't Touch

.PHONY: install
install: install-deps
	stow -d $(ROOT) -t $(HOME) $(CURR)

.PHONY: install-deps
install-deps:
ifdef DEPS
	$(INSTALL) $(DEPS)
endif
ifdef AURDEPS
	$(AURINSTALL) $(AURDEPS)
endif


.PHONY: uninstall-deps
uninstall-deps:
ifdef DEPS
	$(UNINSTALL) $(DEPS)
endif
ifdef AURDEPS
	$(UNINSTALL) $(AURDEPS)
endif

.PHONY: noop
noop:
	echo noop $(CURR)

# vim: ft=make
