" General config
" ______________________________ 
"
set encoding=utf-8
set laststatus=2
set nowrap
set smarttab
set backspace=indent,eol,start
set ttyfast
set timeout timeoutlen=1000 ttimeoutlen=50
set clipboard=unnamed "use p to paste clipboard
set history=1000
set undolevels=1000
set nobackup
set nowritebackup
set noswapfile
set viminfo='20,\"100 "max 100 lines in registers
set visualbell
set noerrorbells
set nomodeline

" Keybindings
" ______________________________
"
" no arrow keys
map <Up> <NOP>
map <Down> <NOP>
map <Left> <NOP>
map <Right> <NOP>

" forgiving exit
:ca Q q

" 2x scrolling
nnoremap <C-e> 2<C-e>
nnoremap <C-y> 2<C-y>

" Plugins
" ______________________________ 

call plug#begin()
Plug 'arcticicestudio/nord-vim'
Plug 'scrooloose/nerdtree'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'ryanoasis/vim-devicons'
Plug 'junegunn/fzf', { 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'
call plug#end()

" NERDTree
let g:NERDTreeShowHidden = 1
let g:NERDTreeMinimalUI = 1
let g:NERDTreeIgnore = []
let g:NERDTreeStatusline = ''

" fzf - use silversearcher-ag to respect .gitignore
let $FZF_DEFAULT_COMMAND = 'ag -g ""'

" plugin keybindings
nnoremap <silent> <C-b> :NERDTreeToggle<CR>
nnoremap <C-p> :FZF<CR>
let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-s': 'split',
  \ 'ctrl-v': 'vsplit'
  \}

" Theme
" ______________________________  

colorscheme nord

