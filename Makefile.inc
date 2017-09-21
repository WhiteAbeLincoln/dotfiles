# Change these settings to match your configuration

INSTALL = sudo pacman -S --noconfirm
AURINSTALL = yes | sudo aura -Aa
UNINSTALL = sudo pacman -Rcns
ROOT = /home/abe/dotfiles
HOME = /home/abe

## Don't Touch

# override these in sub makefiles
# for more granular options
# like installing pip packages
# or installing with curl | bash
PRE_INSTALL = noop
POST_INSTALL = noop
POST_DEPS = noop
POST_UNINSTALL = noop

.PHONY: install
install: install-deps
	stow -d $(ROOT) -t $(HOME) $$(basename $(CURDIR))
	@$(MAKE) $(POST_INSTALL)

.PHONY: install-deps
install-deps:
	@$(MAKE) $(PRE_INSTALL)
ifdef INSTALL
	-$(INSTALL) $(DEPS)
endif
ifdef AURINSTALL
	-$(AURINSTALL) $(AURDEPS)
endif
	@$(MAKE) $(POST_DEPS)

.PHONY: uninstall-deps
uninstall-deps:
ifdef INSTALL
	-$(UNINSTALL) $(DEPS)
endif
ifdef AURINSTALL
	-$(UNINSTALL) $(AURDEPS)
endif
	@$(MAKE) $(POST_UNINSTALL)

.PHONY: noop
noop: ;

# vim: ft=make
