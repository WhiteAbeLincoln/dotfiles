pkgs:

{
  extensions = with pkgs.vscode-extensions; [
    bbenoist.Nix
    vscodevim.vim
    # ms-vscode.cpptools
    # ms-python.python
    ms-azuretools.vscode-docker
  ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
    # use function gethash { nix-prefetch-url "https://$1.gallery.vsassets.io/_apis/public/gallery/publisher/$1/extension/$2/$3/assetbyname/Microsoft.VisualStudio.Services.VSIXPackage" | pbcopy }
    # $ gethash $publisher $name $version
    {
      name = "vscode-firefox-debug";
      publisher = "firefox-devtools";
      version = "2.9.1";
      sha256 = "1xr1z96kd2lcamklc0x4sv0if8n78cr0ara5lmc7bh5afy0h085g";
    }
    {
      name = "editorconfig";
      publisher = "editorconfig";
      version = "0.15.1";
      sha256 = "18r19dn1an81l2nw1h8iwh9x3sy71d4ab0s5fvng5y7dcg32zajd";
    }
    {
      name = "gitlens";
      publisher = "eamodio";
      version = "11.0.2";
      sha256 = "0v7hwykwrfsn702sg6dyjlhhl2nqzg1scpjbxbkkxni5yv4i1li0";
    }
    {
      name = "prettier-vscode";
      publisher = "esbenp";
      version = "5.8.0";
      sha256 = "0h7wc4pffyq1i8vpj4a5az02g2x04y7y1chilmcfmzg2w42xpby7";
    }
    {
      name = "pyright";
      publisher = "ms-pyright";
      version = "1.1.87";
      sha256 = "0i54yl71a40682liqg6v7gzkjr72nzn5f7ryfa8awzn8zq5y96fn";
    }
    {
      name = "shellcheck";
      publisher = "timonwong";
      version = "0.12.1";
      sha256 = "0apvbs90mdjk5y6vy2v4azwxhdjqfypqp5d5hh9rlgxyq4m0azz2";
    }
    {
      name = "pdf";
      publisher = "tomoki1207";
      version = "1.1.0";
      sha256 = "0pcs4iy77v4f04f8m9w2rpdzfq7sqbspr7f2sm1fv7bm515qgsvb";
    }
    {
      name = "vscode-colorize";
      publisher = "kamikillerto";
      version = "0.8.17";
      sha256 = "1ln0d39h6as5jbq730cynad0fwhsc005i2ilc099xgw56vxkl2x1";
    }
  ];
  userSettings = {
    "keyboard.dispatch" = "keyCode";
    "javascript.preferences.quoteStyle" = "single";
    "typescript.preferences.quoteStyle" = "single";

    "files.insertFinalNewline" = true;
    "files.trimTrailingWhitespace" = true;
    "files.trimFinalNewlines" = true;

    "editor.lineNumbers" = "relative";
    "editor.tabSize" = 2;
    "editor.renderWhitespace" = "boundary";
    "editor.tabCompletion" = "on";
    "editor.fontLigatures" = true;
    "editor.suggestSelection" = "first";
    "editor.snippetSuggestions" = "bottom";
    "editor.fontFamily" = "'Fira Code', 'Droid Sans Mono', 'monospace', monospace, 'Droid Sans Fallback'";

    "breadcrums.enabled" = true;
    "workbench.editor.highlightModifiedTabs" = true;
    "debug.inlineValues" = true;
    "explorer.confirmDelete" = false;

    "[markdown]"."files.trimTrailingWhitespace" = false;
    "[typescript]"."editor.defaultFormatter" = "esbenp.prettier-vscode";
    "[typescriptreact]"."editor.defaultFormatter" = "esbenp.prettier-vscode";
    "[javascript]"."editor.defaultFormatter" = "esbenp.prettier-vscode";
    "[markdown]"."editor.defaultFormatter" = "esbenp.prettier-vscode";
    "[json]"."editor.defaultFormatter" = "esbenp.prettier-vscode";
    "emmet.showSuggestionsAsSnippets" = true;
    "vim.handleKeys"."<C-f>" = false;
    "vim.normalModeKeyBindings" = [
      {
        before = ["g" "e"];
        commands = [ { command = "editor.action.marker.next"; when = "editorTextFocus && !editorReadonly"; } ];
      }
      {
        before = ["g" "E"];
        commands = [ { command = "editor.action.marker.prev"; when = "editorTextFocus && !editorReadonly"; } ];
      }
    ];
    "debug.javascript.autoAttachFilter" = "onlyWithFlag";
  };
  keybindings = [
    {
      key = "ctrl+/";
      command = "-editor.action.commentLine";
      when = "editorTextFocus && !editorReadonly";
    }
    {
      key = "ctrl+k ctrl+i";
      command = "-editor.action.showHover";
      when = "editorTextFocus";
    }
    {
      key = "ctrl+/";
      command = "editor.action.showHover";
      when = "editorTextFocus";
    }
    {
      key = "alt+Right";
      command = "workbench.action.navigateForward";
      when = "editorTextFocus";
    }
    {
      key = "alt+Left";
      command = "workbench.action.navigateBack";
      when = "editorTextFocus";
    }
  ];
}
