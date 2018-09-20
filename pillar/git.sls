git:
  managed_userconfig: True
  users:
    abe:
      git_config:
        user:
          name: Abraham White
          email: abelincoln.white@gmail.com
        core:
          excludesfile: /home/abe/.gitignore_global
          editor: vim

git_config:
  push:
    default: simple
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

# vim: ft=yaml
