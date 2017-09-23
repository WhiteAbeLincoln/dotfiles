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
	pip install --upgrade -r requirements.pip

install: ${VENV}
	$(EXEC) salt-call state.apply $(SLS)

.PHONY: python-reqs setup git-config
