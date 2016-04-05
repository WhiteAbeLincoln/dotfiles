""" Colors {{{
syntax enable    " enable syntax processing
" }}}
""" Spaces & Tabs {{{
set expandtab        " tabs are spaces
set shiftwidth=4     " sets auto indent size
set tabstop=4        " number of visual spaces per TAB
set softtabstop=4    " number of spaces in tab when editing
" }}}
""" UI config {{{
set number           " show line numbers
set showcmd          " show command in bottom bar
set cursorline       " highlight current line
filetype indent on   " load filetype-specific indent files
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
nnoremap <leader>j :ProjectCreate . -n java<CR>
" }}}
""" Vundle {{{
set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
" }}}
""" Plugins {{{
Plugin 'scrooloose/syntastic'
Plugin 'Valloric/YouCompleteMe'
Plugin 'skammer/vim-css-color'
Plugin 'digitaltoad/vim-pug'
" Plugin 'whatyouhide/vim-gotham'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'Raimondi/delimitMate'
Plugin 'scrooloose/nerdtree'

call vundle#end()
filetype plugin indent on
" }}}
""" Syntastic {{{
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_cpp_compiler_options = ' -std=c++11'
" }}}
""" YouCompleteMe {{{
let g:ycm_global_ycm_extra_conf = '~/.ycm_extra_conf.py'
let g:ycm_confirm_extra_conf = 0
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:EclimCompletionMethod = 'omnifunc'
" }}}
""" Airline {{{
let g:airline_powerline_fonts = 1
let g:airline_theme='luna'
set laststatus=2
let g:airline_exclude_preview = 1
let g:airline#extensions#tabline#enabled = 1
" }}}
""" NERDTree {{{
map <C-n> :NERDTreeToggle<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
" }}}
""" delimitMate {{{
let delimitMate_expand_cr = 1
let delimitMate_expand_space = 1
let delimitMate_quotes = "\" '"
" }}}

" vim:foldmethod=marker:foldlevel=0
