" Fish compatibility fix
if &shell =~# 'fish$'
        set shell=/usr/bin/bash
endif
""" Colors {{{
syntax enable    " enable syntax processing
""COLORS""
let g:terminal_color_foreground = "#aaaaaa"
let g:terminal_color_background = "#2b2b2b"
let g:terminal_color_cursor= "#aaaaaa"
let g:terminal_color_0 = "#2b2b2b"
let g:terminal_color_1 = "#eb9e8c"
let g:terminal_color_2 = "#cff797"
let g:terminal_color_3 = "#e1cb69"
let g:terminal_color_4 = "#a5afca"
let g:terminal_color_5 = "#e597f7"
let g:terminal_color_6 = "#79ebc2"
let g:terminal_color_7 = "#f9f7f3"
let g:terminal_color_8 = "#505d7e"
let g:terminal_color_9 = "#c8593d"
let g:terminal_color_10 = "#8fab69"
let g:terminal_color_11 = "#caae68"
let g:terminal_color_12 = "#7c92ca"
let g:terminal_color_13 = "#9e69ab"
let g:terminal_color_14 = "#519e83"
let g:terminal_color_15 = "#e6e1dc"
""ENDCOLORS""
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
""" NeoVim {{{
if has("nvim")
    " set termguicolors
    let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif
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
" Plugin 'Valloric/YouCompleteMe'
Plugin 'Shougo/neocomplete'
"Plugin 'osyo-manga/vim-marching'
Plugin 'tpope/vim-surround'
Plugin 'davidhalter/jedi-vim'
Plugin 'sheerun/vim-polyglot'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'alvan/vim-closetag'
Plugin 'jiangmiao/auto-pairs'
Plugin 'chriskempson/base16-vim'

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
""" NeoComplete {{{
let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_smart_case = 1

" Define keyword.
if !exists('g:neocomplete#keyword_patterns')
    let g:neocomplete#keyword_patterns = {}
endif
let g:neocomplete#keyword_patterns['default'] = '\h\w*'

" Plugin key-mappings.
inoremap <expr><C-g>     neocomplete#undo_completion()
inoremap <expr><C-l>     neocomplete#complete_common_string()

" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return (pumvisible() ? "\<C-y>" : "" ) . "\<CR>"
  " For no inserting <CR> key.
  "return pumvisible() ? "\<C-y>" : "\<CR>"
endfunction

inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=jedi#completions
let g:jedi#completions_enabled = 0
let g:jedi#auto_vim_configuration = 0
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

if !exists('g:neocomplete#sources#omni#input_patterns')
    let g:neocomplete#sources#omni#input_patterns = {}
endif

let g:neocomplete#sources#omni#input_patterns.python =
            \ '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'')'

""" }}}
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
