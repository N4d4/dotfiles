""""""""""""""""""""""""""""""""""""""""""
"
" .vimrc - vim configuration file
"
" Version: 0.1 - ven  14 apr 2017, 19.41.00
" Author: N4d4
"
""""""""""""""""""""""""""""""""""""""""""

" SETTING {{{
"scriptencoding utf-8
"set encoding=utf-8

" Set ctermbg to none TODO undestand
"highlight NonText ctermbg=none

" Switch syntax highlighting on, when the terminal has colors
"syntax enable
syntax on

" Use vim, not vi api
set nocompatible

" No backup files
set nobackup

" No write backup
set nowritebackup

" No swap file
set noswapfile

" Command history
set history=100

" Always show cursor
set ruler

" Show incomplete commands
set showcmd

" Incremental searching (search as you type)
set incsearch

" Highlight search matches
set hlsearch

" Ignore case in search
set smartcase

" Make sure any searches /searchPhrase doesn't need the \c escape character
set ignorecase

" A buffer is marked as ‘hidden’ if it has unsaved changes, and it is not currently loaded in a window
" if you try and quit Vim while there are hidden buffers, you will raise an error:
" E162: No write since last change for buffer “a.txt”
set hidden

" Turn word wrap off
set nowrap

" Allow backspace to delete end of line, indent and start of line characters
set backspace=indent,eol,start

" Convert tabs to spaces
set expandtab

" Set tab size in spaces (this is for manual indenting)
set tabstop=4

" The number of spaces inserted for a tab (used for auto indenting)
set shiftwidth=4

" Turn on line numbers
set number

" Set Relative Number in normal mode & Absolute Number in Insert mode
set relativenumber
autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber

" Highlight tailing whitespace
" See issue: https://github.com/Integralist/ProVim/issues/4
set list listchars=tab:\ \ ,trail:·

" Get rid of the delay when pressing O (for example)
" http://stackoverflow.com/questions/2158516/vim-delay-before-o-opens-a-new-line
set timeout timeoutlen=1000 ttimeoutlen=100

" Always show status bar
set laststatus=2

" Set the status line to something useful
set statusline=%f\ %=L:%l/%L\ %c\ (%p%%)

" Set extra options when running in GUI mode
if has("gui_running")
	" Hide the toolbar
    set guioptions-=T
    set guioptions+=e
	set t_Co=256
    set guitablabel=%M\ %t
	" Fonts (if not set, use default terminal fonts)
	set gfn=Monospace\ 14
endif

" UTF encoding
set encoding=utf-8

" Autoload files that have changed outside of vim
set autoread

" Use system clipboard
" http://stackoverflow.com/questions/8134647/copy-and-paste-in-vim-via-keyboard-between-different-mac-terminals
set clipboard+=unnamed

" Don't show intro
set shortmess+=I

" Better splits (new windows appear below and to the right)
set splitbelow
set splitright

" Highlight the current line
set cursorline

" Ensure Vim doesn't beep at you every time you make a mistype
set visualbell
" No annoying sound on errors TODO understand
set noerrorbells
"set novisualbell
"set t_vb=
"set tm=500

" Visual autocomplete for command menu (e.g. :e ~/path/to/file)
set wildmenu

" redraw only when we need to (i.e. don't redraw when executing a macro)
set lazyredraw

" highlight a matching [{()}] when cursor is placed on start/end character
set showmatch

" Set built-in file system explorer to use layout similar to the NERDTree plugin
let g:netrw_liststyle=3

" Always highlight column 80 so it's easier to see where
" cutoff appears on longer screens
autocmd BufWinEnter * highlight ColorColumn ctermbg=darkred
set colorcolumn=80

" Extend background color to the whole screen (xfce4-terminal fix)
set t_ut=

" Use Unix as the standard file type
set ffs=unix,dos,mac

" Auto indent
set ai 
" Smart indent
set si 
" Wrap lines
set wrap 

" Add a bit of extra margin to the left
set foldcolumn=1

" lint mispelled word in IT and EN
set spelllang=it,en

" change window splitting char 
set fillchars+=vert:│

" }}}

" PLUGINS {{{

" AutoInstall Plug if not Installed
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Load plugins with the plugin manager {{{
call plug#begin('~/.vim/plugged')

    "/// THEME 
    Plug 'tomasr/molokai'
    Plug 'ciaranm/inkpot'
    Plug 'jacoborus/tender.vim'
    Plug 'deviantfero/wpgtk.vim'
    Plug 'kristijanhusak/vim-hybrid-material'
	Plug 'tyrannicaltoucan/vim-deep-space'
    Plug 'ayu-theme/ayu-vim'
    Plug 'dylanaraps/wal'
    Plug 'morhetz/gruvbox'
    Plug 'whatyouhide/vim-gotha'
            

    "/// AESTHETICS
	Plug 'vim-airline/vim-airline'
	Plug 'vim-airline/vim-airline-themes'
	Plug 'mhinz/vim-startify'
    Plug 'ryanoasis/vim-devicons'
	Plug 'chrisbra/Colorizer'

    "/// AUTOCOMPLETIONS & SNIPPET
	"Plug 'Shougo/neocomplete.vim'
	"Plug 'Shougo/neosnippet'
	"Plug 'Shougo/neosnippet-snippets'
	Plug 'SirVer/ultisnips'
	Plug 'honza/vim-snippets'
	Plug 'lifepillar/vim-mucomplete'
	Plug 'tpope/vim-commentary'
    Plug 'jiangmiao/auto-pairs'

    "/// LINTING
	Plug 'w0rp/ale'
	"Plug 'rhysd/vim-grammarous'

    "/// SYNTAX
    Plug 'sheerun/vim-polyglot'
	"Plug 'mattn/emmet-vim'
	"Plug 'digitaltoad/vim-pug'
    Plug 'vim-pandoc/vim-pandoc'
    Plug 'vim-pandoc/vim-pandoc-syntax'

    "/// WORKFLOW & IMPROVEMENT
    Plug 'airblade/vim-gitgutter'
    Plug 'tpope/vim-fugitive'
    Plug 'scrooloose/nerdtree'
	"Plug 'ervandew/supertab'
	Plug 'godlygeek/tabular'
	Plug 'dhruvasagar/vim-table-mode/'
	"Plug 'tpope/vim-sensible'
	"Plug 'junegunn/goyo.vim', { 'for': ['markdown', 'pandoc', 'text'] }
	"Plug 'junegunn/limelight.vim', { 'for': ['markdown', 'pandoc', 'text'] }
    Plug 'majutsushi/tagbar'

call plug#end()

"""""""""""""""""""""""""""""""""""""""""" }}}
" THEME {{{

" for vim 7
set t_Co=256

" If you have vim >=8.0 or Neovim >= 0.1.5
if (has("termguicolors"))
 set termguicolors
endif

" Nice colorscheme (wal,deep-space,tender,hybrid_material,deep-space}
colorscheme ayu
set background=dark

" Airline
" Always show the status line "set laststatus=2
let g:airline#extensions#tabline#enabled = 1

" Colorscheme for airlineline {wal,hybrid, deep_space}
let g:airline_theme = "ayu"
let g:enable_bold_font = 1

" Airline (status line)
let g:airline_powerline_fonts = 1

" Ale linter
let g:ale_sign_error = ''
let g:ale_sign_warning = ''

let g:ale_linters = {'cpp': ['gcc', 'cppcheck', 'flawfinder']}
let g:ale_fixers = {'cpp': ['uncrustify']}

" GitGutter
set updatetime=100

let g:gitgutter_sign_added='┃'
let g:gitgutter_sign_modified='┃'
let g:gitgutter_sign_removed='◢'
let g:gitgutter_sign_removed_first_line='◥'
let g:gitgutter_sign_modified_removed='◢'

""""""""""""""""""""""""""""""""""""""""""" }}}
" LIMELIGHT {{{
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!

"""""""""""""""""""""""""""""""""""""""""" }}}
" COLORIZER {{{
" Colorize all the color supported by x11
let g:colorizer_x11_names = 1
" Skip comment from being colored
let g:colorizer_skip_comments = 1
" Auto color certain filetype automatically
let g:colorizer_auto_filetype='css,html,htm'

"""""""""""""""""""""""""""""""""""""""""" }}}
" MU COMPLETE {{{

" Completion popup settings (:help 'completeopt')
set completeopt+=menuone,noselect,noinsert

" Change the behavior of return
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
let g:mucomplete#enable_auto_at_startup = 1

let g:mucomplete#chains = {}
let g:mucomplete#chains.default = ['file', 'omni', 'ulti', 'keyn', 'dict']
let g:mucomplete#chains.unite = []

let g:clang_user_options = '-std=c++14'
let g:clang_complete_auto = 1

"""""""""""""""""""""""""""""""""""""""""" }}}
" ULTISNIPS {{{

"" workaround for interferences with the built-in complete function: |i_CTRL-X_CTRL-K|
"inoremap <c-x><c-k> <c-x><c-k>

"" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger = "<f5>"        " Do not use <tab>
let g:UltiSnipsJumpForwardTrigger = "<right>"  " Do not use <c-j>
let g:UltiSnipsJumpBackwardTrigger="<left>"
inoremap <silent> <expr> <cr> mucomplete#ultisnips#expand_snippet("\<cr>")
"let g:UltiSnips#ExpandSnippetOrJump="<tab>"
"let g:UltiSnipsListSnippets="<s-tab>"
"let g:UltiSnipsJumpForwardTrigger="<c-j>"

"""""""""""""""""""""""""""""""""""""""""" }}}
" ALE {{{
" Enable completion where available.
let g:ale_completion_enabled = 1

"""""""""""""""""""""""""""""""""""""""""" }}}
" TAGBAR {{{
nmap <F8> :TagbarToggle<CR>

"""""""""""""""""""""""""""""""""""""""""" }}}

" }}}

" MAPPING {{{

" Command with pinky :D
nmap ò :
let mapleader = "\<Space>"

" Yank and paste to the OS clipboard.
nnoremap <leader>y "+y
nnoremap <leader>Y "+yy
nnoremap <leader>p "+p
nnoremap <leader>P "+P
vnoremap <leader>y "+y
vnoremap <leader>p "+p

" Save with sudo
cmap w!! %!sudo tee > /dev/null %

" autobold selected word in markdown
autocmd FileType markdown,pandoc vnoremap <leader>b c**<c-r>"**<Esc>

" Toggle Transparent Background
let t:is_transparent = 0
function! Toggle_transparent()
    if t:is_transparent == 0
        hi Normal guibg=NONE ctermbg=NONE
        let t:is_transparent = 1
    else
        set background=dark
        let t:is_tranparent = 0
    endif
endfunction
nnoremap <C-t> : call Toggle_transparent()<CR>

" }}}

" COMMANDS {{{

" Close all folds when opening a new buffer
autocmd BufRead * setlocal foldmethod=marker
autocmd BufRead * normal zM

" reload .vimrc when change occours
augroup myvimrc
    au!
    au BufWritePost .vimrc,_vimrc,vimrc,.gvimrc,_gvimrc,gvimrc so $MYVIMRC | if has('gui_running') | so $MYGVIMRC | endif
augroup END

" Remove whitespace on save
" autocmd BufWritePre * :%s/\s\+$//e

" Rainbow parenthesis always on!
if exists(':RainbowParenthesesToggle')
  autocmd VimEnter * RainbowParenthesesToggle
  autocmd Syntax * RainbowParenthesesLoadRound
  autocmd Syntax * RainbowParenthesesLoadSquare
  autocmd Syntax * RainbowParenthesesLoadBraces
endif

" Compile markdown with pandoc
autocmd FileType markdown,pandoc inoremap <F5> <esc>:!pandoc<space><c-r>%<space>-o<space>/tmp/<c-r>%.pdf<enter>a
autocmd FileType markdown,pandoc nnoremap <F5> :!pandoc<space><c-r>%<space>-o<space>/tmp/<c-r>%.pdf<enter>
autocmd Filetype markdown,pandoc map <F5> :!pandoc<space><C-r>%<space>-o<space>/tmp/<C-r>%.pdf<Enter><Enter>

autocmd FileType markdown,pandoc inoremap <F6> <esc>:!pandoc_mdToPdf_monitor.sh<space><c-r>%<space>&<enter>a
autocmd FileType markdown,pandoc nnoremap <F6> :!pandoc_mdToPdf_monitor.sh<space><c-r>%<space>&<enter>
autocmd Filetype markdown,pandoc map <F6> :!pandoc_mdToPdf_monitor.sh<space><c-r>%<space>&<enter><enter>
" }}}

