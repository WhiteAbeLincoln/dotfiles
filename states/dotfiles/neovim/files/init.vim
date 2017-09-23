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
""" Plugins {{{
call plug#begin('~/.config/nvim/plugged')

" Interface
Plug 'chriskempson/base16-vim'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-airline/vim-airline'
Plug 'kassio/neoterm'
Plug 'Shougo/denite.nvim'
Plug 'Shougo/echodoc.vim'
Plug 'scrooloose/nerdtree'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'jeetsukumaran/vim-buffergator'
Plug 'dbgx/lldb.nvim', {'do': ':UpdateRemotePlugins'}
Plug 'simnalamburt/vim-mundo'
Plug 'christoomey/vim-tmux-navigator'
Plug 'jgdavey/tslime.vim'
Plug 'tpope/vim-obsession'

" Git
Plug 'tpope/vim-fugitive'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'airblade/vim-gitgutter'

" Text manip
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'alvan/vim-closetag'
Plug 'jiangmiao/auto-pairs'
Plug 'terryma/vim-multiple-cursors'
Plug 'godlygeek/tabular'
Plug 'easymotion/vim-easymotion'


" Syntax and linters
Plug 'sheerun/vim-polyglot'
Plug 'gko/vim-coloresque', { 'for': ['css', 'html', 'sass', 'less'] }
Plug 'HerringtonDarkholme/yats.vim', { 'for': 'typescript' }
Plug 'davidhalter/jedi-vim', { 'for': 'python' }
Plug 'lervag/vimtex', { 'for': 'latex' }
Plug 'editorconfig/editorconfig-vim'
Plug 'mpickering/hlint-refactor-vim', { 'for': 'haskell' }
Plug 'neovimhaskell/haskell-vim', { 'for': 'haskell' }
Plug 'Twinside/vim-hoogle', { 'for': 'haskell' }
Plug 'w0rp/ale'

" Snippets
Plug 'Shougo/neosnippet.vim'
Plug 'Shougo/neosnippet-snippets'
Plug 'mattn/emmet-vim'

" Completion
Plug 'autozimu/LanguageClient-neovim', {'do': ':UpdateRemotePlugins'}
Plug 'artur-shaik/vim-javacomplete2', { 'for': 'java' }
Plug 'Shougo/deoplete.nvim', {'do': ':UpdateRemotePlugins'}
Plug 'zchee/deoplete-clang', { 'for': ['cpp', 'c'] }
Plug 'zchee/deoplete-jedi', { 'for': 'python' }
Plug 'mhartington/nvim-typescript', { 'do': ':UpdateRemotePlugins', 'for': 'typescript' }

"" Snippets
Plug 'Shougo/neoinclude.vim'
Plug 'Shougo/neco-syntax'

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
set splitbelow
set splitright
set mouse=a
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
" Neovim terminal configurations
if has('nvim')
  " Use <Esc> to escape terminal insert mode
  tnoremap <Esc> <C-\><C-n>
  " Make terminal split moving behave like normal neovim
  tnoremap <c-h> <C-\><C-n><C-w>h
  tnoremap <c-j> <C-\><C-n><C-w>j
  tnoremap <c-k> <C-\><C-n><C-w>k
  tnoremap <c-l> <C-\><C-n><C-w>l
endif

" Easier split navigation
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
" }}}
""" Files, backups and undo {{{
" Source the vimrc file after saving it
set autoread
augroup sourcing
  autocmd!
  if has('nvim')
    autocmd bufwritepost init.vim source $MYVIMRC
  else
    autocmd bufwritepost .vimrc source $MYVIMRC
  endif
augroup END
autocmd BufWritePost package.yaml silent !hpack --silent
" }}}
""" deoplete {{{
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_smart_case = 1

inoremap <expr><C-g> deoplete#undo_completion()
" inoremap <expr><C-l> deoplete#complete_common_string()

" deoplete tab complete
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

inoremap <expr><C-h> deoplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS>  deoplete#smart_close_popup()."\<C-h>"

" inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
" function! s:my_cr_function()
"     return (pumvisible() ? "\<C-y>" : "") . "\<CR>"
" endfunction

" let g:deoplete#omni#functions = {}
" let g:deoplete#omni#input_patterns = {}
" let g:deoplete#sources = {}

" let g:deoplete#omni#functions.elm = ['elm#Complete']
" let g:deoplete#omni#input_patterns.elm = '[^ \t]+'
" let g:deoplete#sources.elm = ['omni']

let g:deoplete#sources#clang#libclang_path = '/usr/lib/libclang.so'
let g:deoplete#sources#clang#clang_header = '/usr/include/c++/6.3.1'
" }}}
""" LanguageClient-neovim {{{
set hidden

let g:LanguageClient_serverCommands = {
    \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
    \ 'haskell': ['hie', '--lsp']
    \ }

function! StartLanguageClient() abort
    nnoremap <silent> K :call LanguageClient_textDocument_hover()<CR>
    nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>
    nnoremap <silent> <F2> :call LanguageClient_textDocument_rename()<CR>
    nnoremap <leader>sym :call LanguageClient_textDocument_documentSymbol()<CR>:Denite documentSymbol<CR>
    nnoremap <leader>wsym :call LanguageClient_workspace_symbol()<CR>:Denite workspaceSymbol<CR>
    nnoremap <leader>ref :call LanguageClient_textDocument_references()<CR>:Denite references<CR>
    execute "LanguageClientStart"
endfunction

nnoremap <leader>Ls :call StartLanguageClient()<CR>

let g:LanguageClient_diagnosticsDisplay = {
            \ 1: {
            \   "name": "Error",
            \   "texthl": "SyntasticError",
            \   "signText": "✖",
            \   "signTexthl": "Error"
            \ },
            \ 2: {
            \   "name": "Warning",
            \   "texthl": "SyntasticWarning",
            \   "signText": "⚠",
            \   "signTexthl": "SignWarning"
            \ },
            \ 3: {
            \   "name": "Information",
            \   "texthl": "LanguageClientInformation",
            \   "signText": "ℹ",
            \   "signTexthl": "SignInformation"
            \ },
            \ 4: {
            \   "name": "Hint",
            \   "texthl": "LanguageClientHint",
            \   "signText": "ℹ",
            \   "signTexthl": "SignHint"
            \ }
            \ }
" }}}
""" nvim-typescript {{{
let g:nvim_typescript#javascript_support=1
" nnoremap <silent> gd :TSDef<CR>
" }}}
""" neosnippet {{{
let g:neosnippet#enable_completed_snippet = 1
imap <C-k> <Plug>(neosnippet_expand_or_jump)
smap <C-k> <Plug>(neosnippet_expand_or_jump)
xmap <C-k> <Plug>(neosnippet_expand_target)

imap <C-k> <Plug>(neosnippet_expand_or_jump)
smap <expr><TAB> neosnippet#expandable_or_jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"

if has('conceal')
    set conceallevel=2 concealcursor=niv
endif
" }}}
""" neomake {{{
"autocmd! BufReadPost,BufWritePost * Neomake
" }}}
""" ale {{{
let g:ale_sign_error = '✖'
let g:ale_sign_warning = '⚠'
let g:ale_sign_info = 'ℹ'
let g:ale_sign_style_error = '✖|'
let g:ale_sign_style_warning = '⚠|'
let g:airline#extensions#ale#enabled = 1
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'

if !filereadable(expand('./stack.yaml'))
    let g:ale_linters = { 'haskell': ['ghc-mod', 'ghc', 'hdevtools', 'hlint'] }
else
    let g:ale_linters = { 'haskell': ['stack-ghc-mod', 'stack-ghc', 'hlint', 'hdevtools'] }
endif
" }}}
""" ctrlp.vim {{{
set wildignore+=*/tmp/*,*.so,*.swp,*.zip     " MacOSX/Linux
set wildignore+=*\\tmp\\*,*.swp,*.zip,*.exe  " Windows

let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/](\.(git|hg|svn)|\_site|node_modules)$',
  \ 'file': '\v\.(exe|so|dll|class|png|jpg|jpeg)$',
  \ 'link': 'some_bad_symbolic_links',
  \ }

let g:ctrlp_working_path_mode = 'r'
nmap <leader>p :CtrlP<cr>
nmap <leader>bb :CtrlPBuffer<cr>
nmap <leader>bm :CtrlPMixed<cr>
nmap <leader>bs :CtrlPMRU<cr>
" }}}
""" Buffergator {{{
" Use the right side of the screen
let g:buffergator_viewport_split_policy = 'R'

" I want my own keymappings...
let g:buffergator_suppress_keymaps = 1

" Looper buffers
"let g:buffergator_mru_cycle_loop = 1

" Go to the previous buffer open
nmap <leader>jj :BuffergatorMruCyclePrev<cr>

" Go to the next buffer open
nmap <leader>kk :BuffergatorMruCycleNext<cr>

" View the entire list of buffers open
nmap <leader>bl :BuffergatorOpen<cr>
" Shared bindings from Solution #1 from earlier
nmap <leader>T :enew<cr>
nmap <leader>bq :bp <BAR> bd #<cr>
" }}}
""" NERDtree {{{
" autocmd vimenter * NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
nmap <leader>n :NERDTreeToggle<cr>
" }}}
""" IndentGuides {{{
" let g:indent_guides_enable_on_vim_startup = 1
nmap <silent> <Leader>ig <Plug>IndentGuidesToggle
" }}}
""" Airline {{{
let g:airline_powerline_fonts = 1
let g:airline_theme = 'base16'
set laststatus=2
let g:airline_exclude_preview = 1
let g:airline#extensions#tabline#enabled = 1
" }}}
""" vim-polyglot {{{
let g:polygot_disabled = ['latex', 'haskell']
" }}}
""" Mundo {{{
let g:mundo_right = 1
nmap <silent> <leader>u :MundoToggle<CR>
" }}}
""" vim-tmux-navigator {{{
let g:tmux_navigator_save_on_switch = 2
let g:tmux_navigator_disable_when_zoomed = 1
let g:tmux_navigator_no_mappings = 1

nnoremap <silent> <C-h> :TmuxNavigateLeft<cr>
nnoremap <silent> <C-j> :TmuxNavigateDown<cr>
nnoremap <silent> <C-k> :TmuxNavigateUp<cr>
nnoremap <silent> <C-l> :TmuxNavigateRight<cr>

" }}}
""" Slime {{{

vmap <silent> <Leader>rs <Plug>SendSelectionToTmux
nmap <silent> <Leader>rs <Plug>NormalModeSendToTmux
nmap <silent> <Leader>rv <Plug>SetTmuxVars

" }}}

" vim:foldmethod=marker:foldlevel=0
