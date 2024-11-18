{
  delta = {
    enable = true;
    options = {
      number = true;
      syntax-theme = "Github";
    };
  };
  userName = "Abraham White";
  userEmail = "abelincoln.white@gmail.com";
  aliases = {
    aliases = ''! git config --get-regexp ^alias\. | sed -e s/^alias\.// -e s/\ /\ =\ /'';
    branches = "branch -a";
    commits = "log";
    tags = "tag";
    stashes = "stash list";
    remotes = "remote -v";
    unmerged = "diff --name-only --diff-filter=U";
    unstage = "reset -q HEAD --";
    discard = "checkout --";
    uncommit = "reset --mixed HEAD~";
    amend = "commit --amend";
    nuke = "!git reset --hard HEAD && git clean -d -f";
    graphd = "log --graph --abrev-commit --decorate --date=relative --all";
    grapho = "log --graph --oneline --decorate --all";
    stash-working = ''!f() {
      git commit --quiet --no-verify -m "temp for stash-working" &&
      git stash push -m "$(git show --format='%h %s' -s HEAD~1)" "$@" &&
      git reset --quiet --soft HEAD~1; }; f'';
  };
  extraConfig = {
    init.defaultBranch = "trunk";
    pull.rebase = false;
    log.date = "local";
    push.autoSetupRemote = true;
  };
  ignoreFiles = [
    ./ignores/vscode.ignore
  ];
  ignores = [
    "*~"
    "\\#*\\#"
    "/.emacs.desktop"
    "/.emacs.desktop.lock"
    "*.elc"
    "auto-save-list"
    "tramp"
    ".\\#*"
    ".org-id-locations"
    "*_archive"
    "*_flymake.*"
    "/eshell/history"
    "/eshell/lastdir"
    "/elpa/"
    "*.rel"
    "/auto/"
    ".cask"
    "dist/"
    "flycheck_*.el"
    "/server/"
    ".projectile"
    ".dir-locals.el"
    "[._]*.s[a-v][a-z]"
    "[._]*.sw[a-p]"
    "[._]s[a-v][a-z]"
    "[._]sw[a-p]"
    "Session.vim"
    ".netrwhist"
    "tags"
    ".Python"
    "[Bb]in"
    "[Ii]nclude"
    "[Ll]ib"
    "[Ll]ib64"
    "[Ll]ocal"
    "pyvenv.cfg"
    ".venv"
    "pip-selfcheck.json"
    ".fuse_hidden*"
    ".directory"
    ".Trash-*"
    ".nfs*"
    ".direnv/"
  ];
}
