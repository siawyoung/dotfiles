if !has('nvim')
    set nocompatible
endif

call plug#begin('~/.local/share/nvim/plugged')

"""Colour scheme
Plug 'morhetz/gruvbox'

""" list of Plugins
Plug 'airblade/vim-gitgutter'
Plug 'christoomey/vim-tmux-navigator'
Plug 'ervandew/supertab'
Plug 'jiangmiao/auto-pairs'
Plug 'jistr/vim-nerdtree-tabs'
Plug 'junegunn/fzf.vim'
Plug 'ludovicchabant/vim-gutentags'
Plug 'milkypostman/vim-togglelist'
Plug 'neomake/neomake'
Plug 'ntpeters/vim-better-whitespace'
Plug 'rking/ag.vim'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeTabsToggle' }
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'terryma/vim-expand-region'
Plug 'tmhedberg/SimpylFold'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'yggdroot/indentline'

" Language specific plugins

" HTML/CSS
Plug 'mattn/emmet-vim'
" Python
Plug 'nvie/vim-flake8'
" Javascript/JSX
Plug 'pangloss/vim-javascript'
Plug 'jaawerth/nrun.vim'
Plug 'mxw/vim-jsx'
" Elm
Plug 'elmcast/elm-vim'
" Golang
Plug 'fatih/vim-go'
Plug 'zchee/deoplete-go', { 'do': 'make' }
" Elixir
Plug 'elixir-lang/vim-elixir'

call plug#end()

filetype plugin indent on
filetype indent on
filetype on

" Theme
syntax enable
colorscheme gruvbox
if (has("termguicolors"))
 set termguicolors
endif

" Gruvbox specific settings
set background=dark

" Enable to copy to clipboard for operations like yank, delete, change and put
" http://stackoverflow.com/questions/20186975/vim-mac-how-to-copy-to-clipboard-without-pbcopy
if has('unnamedplus')
  set clipboard^=unnamed
  set clipboard^=unnamedplus
endif

" This enables us to undo files even if you exit Vim.
if has('persistent_undo')
  set undofile
  set undodir=~/.config/vim/tmp/undo//
endif

" Map Ctrl-A and Ctrl-X to no-ops to stop number scrolling
nmap <C-a> <Nop>
nmap <C-x> <Nop>
" Quick ESC
imap jk <ESC>
imap kj <ESC>
" Map leader to Space
let mapleader="\<Space>"
" Map <leader>w to save file
nnoremap <Leader>w :w<CR>
" Tie zz after all vertical navigation to force centering
nnoremap <C-U> 11kzz
nnoremap <C-D> 11jzz
nnoremap j jzz
nnoremap k kzz
nnoremap # #zz
nnoremap * *zz
nnoremap n nzz
nnoremap N Nzz
" Cycle through tabs with a and s, as well as numbered switching
nnoremap <Leader>a gT
nnoremap <Leader>s gt
nnoremap <Leader>1 1gt
nnoremap <Leader>2 2gt
nnoremap <Leader>3 3gt
nnoremap <Leader>4 4gt
nnoremap <Leader>5 5gt
nnoremap <Leader>6 6gt
nnoremap <Leader>7 7gt
nnoremap <Leader>8 8gt
nnoremap <Leader>9 9gt
nnoremap <Leader>0 :tablast<cr>

""" General settings"""
"""""""""""""""""""""""
" set tab settings
set expandtab
" set cursor to line
set cursorline
" Convert tabs to spaces
set modelines=0
set clipboard=unnamed
set wrap
set number
set nowritebackup
set noswapfile
set nobackup
" Remove delay for ESC
set timeoutlen=1000 ttimeoutlen=0
" set relative line numbers
set relativenumber
" highlight search result
set hlsearch
set ignorecase
set smartcase
set lazyredraw
set ttyfast
" escape to cancel search highlights
nnoremap <silent> <Esc> :nohlsearch<Bar>:echo<CR>
" remove whitespace on save
autocmd BufWritePre * :%s/\s\+$//e

" Fold settings
set foldmethod=indent
set foldnestmax=2

""" Plugin specific settings"""
"""""""""""""""""""""""""""""""

"fugitive
"Set diff as vertical split instead of horizontal
set diffopt+=vertical

"airline
let g:airline_theme='gruvbox'
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_section_z=''

" emmet
" enable jsx completions
let g:user_emmet_settings={
\  'javascript.jsx' : {
\      'extends' : 'jsx',
\  },
\}

" vim-jsx
" Enable jsx highlighting in .js files as well
let g:jsx_ext_required=0

" vim-json
" Don't hide quotes in json files
set conceallevel=0

"Neomake
"nrun plugin allows us to quickly check local eslint exec
let g:neomake_javascript_eslint_exe=nrun#Which('eslint')
let g:neomake_javascript_enabled_makers=['eslint']
let g:neomake_jsx_enabled_makers=['eslint']
let g:neomake_python_enabled_makers=['flake8']
" Run Neomake only on buffer save
autocmd! BufWritePost,BufEnter * Neomake

" NERDTree
" Toggle NERDTree with Ctrl-N
map <C-n> :NERDTreeTabsToggle<CR>
" Show hidden files
let NERDTreeShowHidden=1
let NERDTreeIgnore=['\.pyc$']
" Add keyboard shortcut for revealing current file in NERDTree
nmap ,n :NERDTreeFind<CR>

" Ag
" Always start searching from project root, not cwd
let g:ag_working_path_mode="r"

" vim-expand-region
" Use v and C-v to increase and decrease region expansion
vmap v <Plug>(expand_region_expand)
vmap <C-v> <Plug>(expand_region_shrink)

" indentLine
" Change colour of indentation guides to a more subtle colour
let g:indentLine_color_term=220

" Gutentags
let g:gutentags_cache_dir='~/.config/nvim/tags/'
let g:gutentags_exclude=['node_modules', 'env']

" fzf setup
set rtp+=/usr/local/opt/fzf
nnoremap <silent> <C-p> :Files<CR>
nnoremap <silent> <Leader>f :Tags<CR>

" deoplete settings
let g:deoplete#enable_at_startup = 1

" deoplete-go settings
let g:deoplete#sources#go#gocode_binary = $GOPATH.'/bin/gocode'
let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']
let g:deoplete#sources#go#use_cache = 1
let g:deoplete#sources#go#json_directory = $HOME.'/github/go/deoplete/json'
