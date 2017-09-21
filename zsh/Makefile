include ../Makefile.inc
DEPS = zsh pkgfile grml-zsh-config zsh-completions
POST_INSTALL = post
PRE_INSTALL = pre

.PHONY: pre
pre:
	curl -sL https://git.io/antibody | bash -s

.PHONY: post
post:
	antibody bundle < ~/.bundles.txt
