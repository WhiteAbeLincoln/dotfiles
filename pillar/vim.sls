#vim:ft=yaml
vim_config:
  plugins:
    - sheerun/vim-polyglot
    - tpope/vim-commentary
    - tpope/vim-surround
    - alvan/vim-closetag
    - jiangmiao/auto-pairs
  settings:
# spaces and tabs
    - expandtab
    - shiftwidth: 4 
    - tabstop: 4 
    - softtabstop: 4 
# ui config
    - number
    - relativenumber
    - showcmd
    - cursorline
    - wildmenu
    - lazyredraw
    - showmatch
    - splitbelow
    - splitright
    - mouse: a
    - hidden
# searching
    - incsearch
    - hlsearch
# folding
    - foldenable
    - foldlevelstart: 10
    - foldnestmax: 10
    - foldmethod: indent
  config:
    - syntax: enable
    - filetype plugin indent on
  mappings:
# turn of search highlight with <leader><space>
   -
     from: <leader><space>
     to: ":nohlsearch<CR>"
     mode: nnoremap 
# space opens/closes folds
   -
     from: <space>
     to: za
     mode: nnoremap
   -
     from: <C-J>
     to: <C-W><C-J>
     mode: nnoremap
   -
     from: <C-K>
     to: <C-W><C-K>
     mode: nnoremap
   -
     from: <C-L>
     to: <C-W><C-L>
     mode: nnoremap
   -
     from: <C-H>
     to: <C-W><C-H>
     mode: nnoremap
