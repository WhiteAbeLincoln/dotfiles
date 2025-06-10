{
  pkgs,
  lib,
  ...
}:
with lib; {
  programs.neovim = {
    enable = true;
    coc.enable = true;
    defaultEditor = true;
    plugins = with pkgs.vimPlugins; [
      vim-polyglot
      vim-commentary
      vim-surround
      vim-closetag
      vim-unimpaired
      auto-pairs
      colorizer
    ];
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    extraConfig = ''
      syntax on
      filetype plugin indent on
      set expandtab
      set shiftwidth=2
      set tabstop=2
      set number
      set relativenumber
      set mouse=a
      set hidden
      set softtabstop=2
      set showcmd
      set cursorline
      set wildmenu
      set lazyredraw
      set showmatch
      set incsearch
      set hlsearch
      set splitbelow
      set splitright
      set foldenable
      set foldlevelstart=10
      set foldnestmax=10
      set foldmethod=indent
      set termguicolors
      let g:colorizer_auto_map = 1
      nnoremap <leader><space> :nohlsearch<CR>
      nnoremap <space> za
      nnoremap <C-J> <C-W><C-J>
      nnoremap <C-K> <C-W><C-K>
      nnoremap <C-L> <C-W><C-L>
      nnoremap <C-H> <C-W><C-H>
      command W :execute ':silent w !sudo tee % > /dev/null' | :edit!
    '';
  };
}
