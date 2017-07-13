include Makefile.inc
DEPS = stow
DIRS = $(wildcard */)

.PHONY: all
all: install-deps
	- for d in $(DIRS); do (cd $$d; CURR=$$d $(MAKE) noop); done

.PHONY: install-all-deps
install-all-deps:
	- for d in $(DIRS); do (cd $$d; $(MAKE) install-deps); done

.PHONY: uninstall-all-deps
uninstall-deps:
	- for d in $(DIRS); do (cd $$d; $(MAKE) uninstall-deps); done
