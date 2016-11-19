if !has('nvim')
    set nocompatible
endif
set hidden
syntax on
filetype off

set rtp+=~/.vim/Bundle/Vundle.vim
call vundle#rc()

" Let Vundle manage Vundle
Bundle 'gmarik/Vundle.vim'

"""Colour scheme
Plugin 'jacoborus/tender'

""" list of Plugins
Bundle 'tpope/vim-sensible'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-sleuth'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Bundle 'scrooloose/syntastic'
Bundle 'myint/syntastic-extras'
Bundle 'scrooloose/nerdtree'
Bundle 'jistr/vim-nerdtree-tabs'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Bundle 'rking/ag.vim'
Plugin 'jiangmiao/auto-pairs'
Plugin 'ntpeters/vim-better-whitespace'
Plugin 'airblade/vim-gitgutter'
Plugin 'scrooloose/nerdcommenter'
Plugin 'ervandew/supertab'
Plugin 'terryma/vim-expand-region'
Plugin 'mattn/emmet-vim'
Plugin 'yggdroot/indentline'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'junegunn/fzf.vim'
Plugin 'ludovicchabant/vim-gutentags'

" Language specific plugins
Bundle 'nvie/vim-flake8'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'
Plugin 'fatih/vim-go'
Plugin 'elixir-lang/vim-elixir'
Plugin 'raichoo/purescript-vim'

call vundle#end()
filetype plugin indent on
filetype indent on
filetype on

" Theme
syntax enable
colorscheme tender
if (has("termguicolors"))
 set termguicolors
endif

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
let mapleader = "\<Space>"
" Map <leader>w to save file
nnoremap <Leader>w :w<CR>
" Tie zz after all vertical navigation to force centering
" It's still buggy when editing text near the end of a file
" but can't be bothered to fix
:nnoremap j jzz
:nnoremap k kzz
:nnoremap <C-d> <C-d>zz
:nnoremap <C-u> <C-u>zz
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
" Set scrolloff to a super high value to center cursor in vertical center
set scrolloff=999
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

""" Plugin specific settings"""
"""""""""""""""""""""""""""""""

"fugitive
"Set diff as vertical split instead of horizontal
set diffopt+=vertical

"airline
let g:airline_theme = 'tender'
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_section_z=''

" vim-jsx
" Enable jsx highlighting in .js files as well
let g:jsx_ext_required = 0

" Syntastic
let g:syntastic_mode_map = { 'mode': 'active',
                            \ 'active_filetypes': ['python', 'javascript'],
                            \ 'passive_filetypes': [] }

let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_python_checkers = ['flake8']
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" NERDTree
" Toggle NERDTree with Ctrl-N
map <C-n> :NERDTreeTabsToggle<CR>
" Show hidden files
let NERDTreeShowHidden=1
let NERDTreeIgnore=['\.pyc$']

" Ag
" Always start searching from project root, not cwd
let g:ag_working_path_mode="r"

" vim-expand-region
" Use v and C-v to increase and decrease region expansion
vmap v <Plug>(expand_region_expand)
vmap <C-v> <Plug>(expand_region_shrink)

" indentLine
" Change colour of indentation guides to a more subtle colour
let g:indentLine_color_term = 220

" Gutentags
let g:gutentags_cache_dir = '~/.config/nvim/tags/'
let g:gutentags_exclude = ['node_modules', 'env']

" fzf setup
set rtp+=/usr/local/opt/fzf
nnoremap <silent> <C-p> :Files<CR>
nnoremap <silent> <Leader>f :Tags<CR>
