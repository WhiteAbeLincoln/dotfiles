# makefile template from http://blog.byronjsmith.com/makefile-shortcuts.html

.git:
	git init

git-config: | .git
	;

VENV = .venv
export VIRTUAL_ENV := $(abspath ${VENV})
export PATH := ${VIRTUAL_ENV}/bin:${PATH}

${VENV}:
	python3 -m venv $@

python-reqs: requirements.pip | ${VENV}
	sudo pacman -S libgit2
	pip3 install --upgrade -r requirements.pip

config/gpgkeys:
	gpg --pinentry-mode loopback --decrypt salt_dotfiles.key.gpg > salt_dotfiles.key
	sudo mkdir config/gpgkeys
	sudo chmod 600 config/gpgkeys
	sudo gpg --homedir config/gpgkeys --import salt_dotfiles.key

install: python-reqs config/gpgkeys
	sudo env "PATH=${PATH}" salt-call state.apply

.PHONY: python-reqs setup git-config
