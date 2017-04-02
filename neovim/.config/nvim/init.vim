""" Vim-Plug Core {{{
if has('vim_starting')
    set nocompatible
endif

let vimplug_exists=expand('~/.config/nvim/autoload/plug.vim')

if !filereadable(vimplug_exists)
  echo "Installing Vim-Plug..."
  echo ""
  silent !\curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  let g:not_finish_vimplug = "yes"

  autocmd VimEnter * PlugInstall
endif

" }}}
""" Vim-Plug {{{
call plug#begin('~/.config/nvim/plugged')
Plug 'neomake/neomake'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'airblade/vim-gitgutter'
Plug 'gko/vim-coloresque'
Plug 'kassio/neoterm'
Plug 'critiqjo/lldb.nvim'
Plug 'alvan/vim-closetag'
Plug 'jiangmiao/auto-pairs'
Plug 'chriskempson/base16-vim'
" Syntax
Plug 'sheerun/vim-polyglot'
Plug 'HerringtonDarkholme/yats.vim'
Plug 'davidhalter/jedi-vim', { 'for': 'python' }
" Snippets
Plug 'Shougo/neosnippet.vim'
Plug 'Shougo/neosnippet-snippets'
" Completion
Plug 'artur-shaik/vim-javacomplete2', { 'for': 'java' }
Plug 'Shougo/deoplete.nvim', {'do': ':UpdateRemotePlugins'}
Plug 'zchee/deoplete-clang'
Plug 'zchee/deoplete-jedi', { 'for': 'python' }
Plug 'mhartington/nvim-typescript', { 'do': 'npm install -g typescript' }
" Plug 'wokalski/autocomplete-flow', { 'do': 'npm install -g flow-bin' }
Plug 'carlitux/deoplete-ternjs', { 'do': 'npm install -g tern' }
Plug 'Shougo/neoinclude.vim'

" latex live reload 'donRaphaco/neotex'
" project live search 'enugen0329/vim-esearch'
call plug#end()
" }}}
""" Colors {{{
syntax enable    " enable syntax processing
set termguicolors
colorscheme base16-custom
" }}}
""" Spaces & Tabs {{{
set expandtab        " tabs are spaces
set shiftwidth=4     " sets auto indent size
set tabstop=4        " number of visual spaces per TAB
set softtabstop=4    " number of spaces in tab when editing
" }}}
""" UI config {{{
set number           " show line numbers
set relativenumber   " relative line numbers
set showcmd          " show command in bottom bar
set cursorline       " highlight current line
filetype plugin indent on   " load filetype-specific indent files
set wildmenu         " visual autocomplete for command menu
set lazyredraw       " redraw only when needed
set showmatch        " hightlight matching brackets
" }}}
""" Searching {{{
set incsearch        " search as characters are entered
set hlsearch         " highlight matches
" turn off search highlight with \<space>
nnoremap <leader><space> :nohlsearch<CR>
" }}}
""" Folding {{{
set foldenable       " enable folding
set foldlevelstart=10    " open most folds by default
set foldnestmax=10   " 10 nested fold max
" space opens/closes folds
nnoremap <space> za
set foldmethod=indent    " fold based on indent level
" }}}
""" Movement {{{
" nnoremap B ^
" nnoremap E $
" inoremap jk <esc>
" }}}
""" Remaps {{{
" creates a new java project
" nnoremap <leader>j :ProjectCreate . -n java<CR>
" }}}
""" deoplete {{{
let g:deoplete#enable_at_startup = 1
" }}}
""" deoplete-clang {{{
let g:deoplete#sources#clang#libclang_path = '/usr/lib/libclang.so'
let g:deoplete#sources#clang#clang_header = '/usr/include/c++/6.3.1'
" }}}
""" deoplete-ternjs {{{
let g:tern_request_timeout = 1
let g:tern_show_signature_in_pum = '0'  " This do disable full signature type on autocomplete
" }}}
""" neosnippet {{{
let g:neosnippet#enable_completed_snippet = 1
" }}}
""" Airline {{{
let g:airline_powerline_fonts = 1
let g:airline_theme = 'luna'
set laststatus=2
let g:airline_exclude_preview = 1
let g:airline#extensions#tabline#enabled = 1
" }}}
" vim:foldmethod=marker:foldlevel=0
