git:
  users:
    abe:
      fullname: Abraham White
      email: abelincoln.white@gmail.com

git_config:
  user:
    email: abelincoln.white@gmail.com
    name: Abraham White
  push:
    default: simple
  core:
    excludesfile: /home/abe/.gitignore_global
    editor: emacs
  alias:
    branches: branch -a
    commits: log
    tags: tag
    stashes: stash list
    remotes: remote -v
    unmerged: diff --name-only --diff-filter=U
    unstage: "reset -q HEAD --"
    discard: "checkout --"
    uncommit: reset --mixed HEAD~
    amend: commit --amend
    nuke: "!git reset --hard HEAD && git clean -d -f"
    graphd: log --graph --abbrev-commit --decorate --date=relative --all
    graph: log --graph --oneline --decorate --all
  web:
    browser: xdg-open
