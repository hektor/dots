" general config _______________________ 

set nocompatible

filetype plugin indent on

set encoding=utf-8
set hidden
set ttyfast
set updatetime=300
set timeout timeoutlen=1000 ttimeoutlen=5
set undolevels=500
set history=500
set shortmess+=c
set signcolumn=number " make sign replace number
set nowrap
set backspace=indent,eol,start " indentat
set incsearch ignorecase smartcase hlsearch" search     
set autoindent tabstop=2 softtabstop=2 shiftwidth=2 expandtab " indenting
set lazyredraw " only essential redraws
set synmaxcol=180
set nobackup nowb noswapfile " turn off backups
set viminfo='20,\"100 "max 100 lines in registers
set novisualbell
set conceallevel=1
set clipboard=unnamedplus

" functions ____________________________

func! ToggleRnu() " toggle: no numbers - relative nummbers
  if(&nu) | set nonu nornu | else | set nu rnu | endif
endfunc

" keybindings __________________________

nnoremap <space> <nop>
let mapleader = " "
let maplocalleader = ";"

" split
nmap ss :sp<Return><c-w>w
nmap sv :vs<Return><c-w>w

" split navigate
nnoremap sw <c-w>w
nnoremap sh <c-w>h
nnoremap sj <c-w>j
nnoremap sk <c-w>k
nnoremap sl <c-w>l

" split resize
nnoremap sH <C-w>8<
nnoremap sJ <C-w>8-
nnoremap sK <C-w>8+
nnoremap sL <C-w>8>

" file tree
nnoremap sb :Lex<cr>

" terminal
nnoremap <leader>t :term<cr>

" hard mode
nnoremap <left> <nop>
nnoremap <down> <nop>
nnoremap <up> <nop>
nnoremap <right> <nop>
inoremap <left> <nop>
inoremap <down> <nop>
inoremap <up> <nop>
inoremap <right> <nop>

" quick quit
nnoremap <leader>w :w<cr>
nnoremap <leader>W :wq<cr>
nnoremap <leader>q :q<cr>
nnoremap <leader>Q :q!<cr>

" quick exit insert
inoremap jj <esc>

" clear search highlight
nnoremap <leader><space> :noh<cr>

" silver search
nnoremap <leader>A :Ag <cr> 

" toggle numbers
nnoremap <leader>n :call ToggleRnu()<cr>

" edit vim config
nnoremap <leader>ec :vsplit $MYVIMRC<cr>

" source current file
nnoremap <leader>so :so %<cr>

" plugins ______________________________ 

call plug#begin()

Plug 'preservim/nerdcommenter'
Plug 'junegunn/fzf', { 'do': './install --bin' }
Plug 'junegunn/fzf.vim'

Plug 'pangloss/vim-javascript', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'leafgarland/typescript-vim', { 'for': ['typescript', 'typescript.tsx'] }
Plug 'peitalin/vim-jsx-typescript', { 'for': ['typescript.tsx'] }
Plug 'evanleck/vim-svelte'

Plug 'neoclide/coc.nvim', {'branch': 'release'}
let g:coc_global_extensions = [
  \'coc-eslint',
  \'coc-prettier',
  \'coc-tsserver',
  \'coc-python',
  \'coc-html',
  \'coc-emmet',
  \'coc-css',
  \'coc-svg',
  \'coc-svelte',
  \'coc-json',
  \'coc-markdownlint',
  \'coc-yaml',
  \]

Plug 'supercollider/scvim'
Plug 'tidalcycles/vim-tidal'

call plug#end()

" plugin config ________________________ 

" NERDCommenter
let g:NERDSpaceDelims = 1
let g:NERDCompactSexyComs = 1
let NERDAltDelims_haskell = 1
let g:NERDCustomDelimiters = { 'tidal': { 'left': '{-','right': '-}' } }
let g:NERDCustomDelimiters = { 'tidal': { 'left': '--','right': '' } }
let g:NERDCommentEmptyLines = 1

" fzf popup
let g:fzf_layout = {'window': { 'width': 0.62, 'height': 0.62}}

" fzf - use silversearcher-ag to respect .gitignore
let $FZF_DEFAULT_COMMAND = 'ag --hidden --ignore .git -g ""'
let g:ag_working_path_mode="r"
set wildignore+=*/node_modules/*,*/tmp/*,*.so,*.swp,*.zip " fzf ignore

" js & ts
let g:javascript_plugin_jsdoc = 1 " jsdoc syntax highlighting
let g:javascript_plugin_flow = 1 " flow syntax highlighting
let g:javascript_conceal_function = "ƒ"
let g:javascript_conceal_return   = "⇚""

" svelte
let g:svelte_indent_script = 0
let g:svelte_indent_style = 0

" SuperCollider
au BufEnter,BufWinEnter,BufNewFile,BufRead *.sc,*.scd set filetype=supercollider
au Filetype supercollider packadd scvim

" tidalvim
let g:tidal_default_config = {"socket_name": "default", "target_pane": "tidal:1.1"}

" plugin keybindings ___________________  

" coc autocompletion
inoremap <silent><expr> <C-j>
  \ pumvisible() ? "\<C-n>" :
  \ <SID>check_back_space() ? "\<TAB>" :
  \ coc#refresh()

inoremap <expr><C-k> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" <TAB> confirm completion
inoremap <expr> <TAB> pumvisible() ? "\<cr>" : "\<C-g>u\<CR>"

" code action on cursor position
nmap <leader>do <Plug>(coc-codeaction)

set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" :Prettier command to prettify file
command! -nargs=0 Prettier :CocCommand prettier.formatFile

nnoremap <c-p> :FZF<cr>
nnoremap <leader>p :FZF<cr>
nnoremap <leader>b :Buffers<cr>
nnoremap <leader>h :History<cr>
let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-s': 'split',
  \ 'ctrl-v': 'vsplit'
  \}

" theme ________________________________  

colorscheme darkness
