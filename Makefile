include Makefile.inc
DEPS = stow
DIRS = $(wildcard */)

.PHONY: all
all: install-deps
	- for d in $(DIRS); do (cd $$d; CURR=$$d $(MAKE) install); done

.PHONY: all-install-deps
all-install-deps:
	- for d in $(DIRS); do (cd $$d; $(MAKE) install-deps); done

.PHONY: all-uninstall-deps
all-uninstall-deps:
	- for d in $(DIRS); do (cd $$d; $(MAKE) uninstall-deps); done
